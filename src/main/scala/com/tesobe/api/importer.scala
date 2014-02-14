/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com
TESOBE / Music Pictures Ltd
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com

 */
package com.tesobe.api

import com.tesobe.actors.EnvelopeInserter
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST._
import java.util.Calendar
import net.liftweb.mongodb._
import net.liftweb.json.JsonAST.JString
import _root_.java.math.MathContext
import org.bson.types._
import org.joda.time.{ DateTime, DateTimeZone }
import java.util.regex.Pattern
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.sitemap._
import _root_.scala.xml._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.mapper.view._
import com.tesobe.model._
import java.util.Date
import net.liftweb.json.Extraction


object ImporterAPI extends RestHelper with Loggable {

  def errorJsonResponse(message : String = "error", httpCode : Int = 400) : JsonResponse =
    JsonResponse(Extraction.decompose(ErrorMessage(message)), Nil, Nil, httpCode)

  serve {
    //a temporary way to add transaction via api for a single specific exception case. should be removed later.
    case "api" :: "tmp" :: "transactions" :: Nil JsonPost json => {
      val secretKey = S.param("secret")

      def addMatchingTransactions(secret: String) = {
        val rawEnvelopes = json._1.children
        val envelopes = rawEnvelopes.flatMap(OBPEnvelope.fromJValue)
        val matchingEnvelopes = for {
          e <- envelopes
          bankName <- Props.get("exceptional_account_bankName")
          number <- Props.get("exceptional_account_number")
          kind <- Props.get("exceptional_account_kind")
          if(e.obp_transaction.get.this_account.get.bank.get.name.get == bankName)
          if(e.obp_transaction.get.this_account.get.number.get == number)
          if(e.obp_transaction.get.this_account.get.kind.get == kind)
        } yield e

        val ipAddress = json._2.remoteAddr
        logger.info("Received " + rawEnvelopes.size +
          " json transactions to insert from ip address " + ipAddress)
        logger.info("Received " + matchingEnvelopes.size +
          " valid transactions to insert from ip address " + ipAddress)

        /**
         * Using an actor to do insertions avoids concurrency issues with
         * duplicate transactions by processing transaction batches one
         * at a time. We'll have to monitor this to see if non-concurrent I/O
         * is too inefficient. If it is, we could break it up into one actor
         * per "Account".
         */
        val l = EnvelopesToInsert(matchingEnvelopes)
        val createdEnvelopes = EnvelopeInserter !? (3 seconds, l)

        createdEnvelopes match {
          case Full(env: InsertedEnvelopes) =>{
            val insertedEnvelopes = env.l
            if(insertedEnvelopes.size!=0){
              Account.find(("number" -> Props.get("exceptional_account_number").getOrElse("")) ~
                ("bankName" -> Props.get("exceptional_account_bankName").getOrElse("")) ~
                ("kind" -> Props.get("exceptional_account_kind").getOrElse("")))
              match {
                case Full(account) =>  account.lastUpdate(new Date).save
                case _ =>
              }
            }
            val jsonList = insertedEnvelopes.map(_.whenAddedJson)
            JsonResponse(JArray(jsonList))
          }
          case _ => InternalServerErrorResponse()
        }
      }

      def valid(secret : String) = {
        val authorised = for (validSecret <- Props.get("exceptional_account_secret"))
          yield secret == validSecret

        authorised getOrElse false
      }

      secretKey match {
        case Full(s) => if(valid(s))
                          addMatchingTransactions(s)
                        else
                          UnauthorizedResponse("wrong secret key")
        case _ => NotFoundResponse()
      }

    }
  }

  serve {

    /**
     * curl -i -H "Content-Type: application/json" -X POST -d '{
     * "obp_transaction":{
     * "this_account":{
     * "holder":"Music Pictures Limited",
     * "number":"123567",
     * "kind":"current",
     * "bank":{
     * "IBAN":"DE1235123612",
     * "national_identifier":"de.10010010",
     * "name":"Postbank"
     * }
     * },
     * "other_account":{
     * "holder":"Client 1",
     * "number":"123567",
     * "kind":"current",
     * "bank":{
     * "IBAN":"UK12222879",
     * "national_identifier":"uk.10010010",
     * "name":"HSBC"
     * }
     * },
     * "details":{
     * "type_en":"Transfer",
     * "type_de":"Überweisung",
     * "posted":{
     * "$dt":"2012-01-04T18:06:22.000Z"
     * },
     * "completed":{
     * "$dt":"2012-09-04T18:52:13.000Z"
     * },
     * "new_balance":{
     * "currency":"EUR",
     * "amount":"4323.45"
     * },
     * "value":{
     * "currency":"EUR",
     * "amount":"123.45"
     * },
     * "other_data":"9"
     * }
     * }
     * }  ' http://localhost:8080/api/transactions
     */
    case "api" :: "transactions" :: Nil JsonPost json => {

      def savetransactions ={
        val rawEnvelopes = json._1.children
        val envelopes : List[OBPEnvelope]= rawEnvelopes.flatMap(e => {
          OBPEnvelope.envlopesFromJvalue(e)
        })

        def updateAccountBalance(accountNumber: String, bankId: String, account: Account) = {
          val newest =
            OBPEnvelope.findAll(
              ("obp_transaction.this_account.number" -> accountNumber) ~
              ("obp_transaction.this_account.bank.national_identifier" -> bankId),
              ("obp_transaction.details.completed" -> -1),
              Limit(1)
            ).headOption

          if(newest.isDefined) {
            logger.debug(s"Updating current balance for account $accountNumber at bank $bankId")
            account.balance(newest.get.obp_transaction.get.details.get.new_balance.get.amount.get).save
          }
          else
            logger.warn("Could not update the balance for the account $accountNumber at bank $bankId")
        }
        def updateBankAccount(insertedEnvelopes: List[OBPEnvelope]) = {
          if(insertedEnvelopes.nonEmpty) {
            //we assume here that all the Envelopes concerns only one account
            val envelope = insertedEnvelopes(0)
            val thisAccount = envelope.obp_transaction.get.this_account.get
            val accountNumber = thisAccount.number.get
            val bankId = thisAccount.bank.get.national_identifier.get
            envelope.theAccount match {
              case Full(account) =>  {
                account.lastUpdate(new Date).save
                updateAccountBalance(accountNumber, bankId, account)
              }
              case _ => logger.info("account $accountNumber at bank $bankId not found")
            }
          }
        }

        val ipAddress = json._2.remoteAddr
        logger.info("Received " + rawEnvelopes.size +
          " json transactions to insert from ip address " + ipAddress)
        logger.info("Received " + envelopes.size +
          " valid transactions to insert from ip address " + ipAddress)

      /**
       * Using an actor to do insertions avoids concurrency issues with
       * duplicate transactions by processing transaction batches one
       * at a time. We'll have to monitor this to see if non-concurrent I/O
       * is too inefficient. If it is, we could break it up into one actor
       * per "Account".
       */

        val l = EnvelopesToInsert(envelopes)
        // TODO: this duration limit should be fixed
        val createdEnvelopes = EnvelopeInserter !? (3 minutes, l)

        createdEnvelopes match {
          case Full(env: InsertedEnvelopes) =>{
            val insertedEnvelopes = env.l
              logger.info("inserted " + insertedEnvelopes.size + " transactions")
              updateBankAccount(insertedEnvelopes)
              val jsonList = insertedEnvelopes.map(_.whenAddedJson)
              JsonResponse(JArray(jsonList))
            }
          case _ => {
            logger.warn("no envelopes inserted")
            InternalServerErrorResponse()
          }
        }
      }

      S.param("secret") match {
        case Full(s) => {
          Props.get("importer_secret") match {
            case Full(localS) =>
            if(localS == s)
              savetransactions
            else
              errorJsonResponse("wrong secret", 401)
            case _ => errorJsonResponse("importer_secret not set")
          }
        }
        case _ => errorJsonResponse("secret missing")
      }

    }
  }
}