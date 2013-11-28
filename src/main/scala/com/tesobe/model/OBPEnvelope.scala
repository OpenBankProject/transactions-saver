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
package com.tesobe.model

import net.liftweb.mongodb._
import net.liftweb.record.MandatoryTypedField
import net.liftweb.mongodb.record.field._
import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord, BsonMetaRecord, BsonRecord}
import net.liftweb.common.{Box, Full, Empty, Failure}
import java.util.Calendar
import java.text.SimpleDateFormat
import net.liftweb.json.DefaultFormats
import java.util.Date
import net.liftweb.record.field.{StringField,LongField}
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb.record.{MongoId}
import net.liftweb.mongodb.record.field.{MongoJsonObjectListField, MongoRefField, ObjectIdRefField}
import scala.util.Random
import com.mongodb.QueryBuilder
import com.mongodb.BasicDBObject
import net.liftweb.common.Loggable
import org.bson.types.ObjectId
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import java.net.URL
import net.liftweb.record.field.{DoubleField,DecimalField}

/**
 * "Current Account View"
curl -i -H "Content-Type: application/json" -X POST -d '[{
         "obp_transaction":{
            "this_account":{
               "holder":"Music Pictures Limited",
               "number":"123567",
               "kind":"current",
               "bank":{
                  "IBAN":"DE1235123612",
                  "national_identifier":"de.10010010",
                  "name":"Postbank"
               }
            },
            "other_account":{
               "holder":"Client 1",
               "number":"123567",
               "kind":"current",
               "bank":{
                  "IBAN":"UK12222879",
                  "national_identifier":"uk.10010010",
                  "name":"HSBC"
               }
            },
            "details":{
               "type_en":"Transfer",
               "type_de":"Überweisung",
               "posted":{
                  "$dt":"2012-01-04T18:06:22.000Z"
                },
               "completed":{
                  "$dt":"2012-09-04T18:52:13.000Z"
                },
               "new_balance":{
                     "currency":"EUR",
                  "amount":"4323.45"
               },
               "value":{
                  "currency":"EUR",
                  "amount":"123.45"
               },
               "other_data":"9"
            }
         }
 },
{
         "obp_transaction":{
            "this_account":{
               "holder":"Music Pictures Limited",
               "number":"123567",
               "kind":"current",
               "bank":{
                  "IBAN":"DE1235123612",
                  "national_identifier":"de.10010010",
                  "name":"Postbank"
               }
            },
            "other_account":{
               "holder":"Client 2",
               "number":"123567",
               "kind":"current",
               "bank":{
                  "IBAN":"UK22222879",
                  "national_identifier":"uk.10010010",
                  "name":"HSBC"
               }
            },
            "details":{
               "type_en":"Transfer",
               "type_de":"Überweisung",
               "posted":{
                  "$dt":"2012-01-04T14:06:22.000Z"
                },
               "completed":{
                  "$dt":"2012-09-04T14:52:13.000Z"
                },
               "new_balance":{
                     "currency":"EUR",
                  "amount":"2222.45"
               },
               "value":{
                  "currency":"EUR",
                  "amount":"223.45"
               },
               "other_data":"9"
            }
         }
 }]' http://localhost:8080/api/transactions
 */

// Seems to map to a collection of the plural name
class OBPEnvelope private() extends MongoRecord[OBPEnvelope] with ObjectIdPk[OBPEnvelope] with Loggable{
  def meta = OBPEnvelope

  // This creates a json attribute called "obp_transaction"
  object obp_transaction extends BsonRecordField(this, OBPTransaction)

  object DateDescending extends Ordering[OBPEnvelope] {
    def compare(e1: OBPEnvelope, e2: OBPEnvelope) = {
      val date1 = e1.obp_transaction.get.details.get.completed.get
      val date2 = e2.obp_transaction.get.details.get.completed.get
      date1.compareTo(date2)
    }
  }

  lazy val theAccount = {
    val thisAcc = obp_transaction.get.this_account.get
    val num = thisAcc.number.get
    val accKind = thisAcc.kind.get
    val bankName = thisAcc.bank.get.name.get
    val holder = thisAcc.holder.get
    val accQry = QueryBuilder.start("number").is(num).
      put("kind").is(accKind).put("holder").is(holder).get

    for {
      account <- Account.find(accQry)
      bank <- HostedBank.find("name", bankName)
      if(bank.id.get == account.bankID.get)
    } yield account
  }

  def orderByDateDescending = (e1: OBPEnvelope, e2: OBPEnvelope) => {
    val date1 = e1.obp_transaction.get.details.get.completed.get
    val date2 = e2.obp_transaction.get.details.get.completed.get
    date1.after(date2)
  }
  def createAliases : Box[String] = {
    val realOtherAccHolder = this.obp_transaction.get.other_account.get.holder.get
    def publicAliasExists(realValue: String): Boolean = {
      this.theAccount match {
        case Full(a) => {
          val otherAccs = a.otherAccounts.objs
          val aliasInQuestion = otherAccs.find(o =>
            o.holder.get.equals(realValue))
          aliasInQuestion.isDefined
        }
        case _ => false
      }
    }

    def createPublicAlias(realOtherAccHolder : String) : Box[String] = {

      /**
       * Generates a new alias name that is guaranteed not to collide with any existing public alias names
       * for the account in question
       */
      def newPublicAliasName(account: Account): String = {
        val firstAliasAttempt = "ALIAS_" + Random.nextLong().toString.take(6)

        /**
         * Returns true if @publicAlias is already the name of a public alias within @account
         */
        def isDuplicate(publicAlias: String, account: Account) = {
          account.otherAccounts.objs.find(oAcc => {
            oAcc.publicAlias.get == publicAlias
          }).isDefined
        }

        /**
         * Appends things to @publicAlias until it a unique public alias name within @account
         */
        def appendUntilUnique(publicAlias: String, account: Account): String = {
          val newAlias = publicAlias + Random.nextLong().toString.take(1)
          if (isDuplicate(newAlias, account)) appendUntilUnique(newAlias, account)
          else newAlias
        }

        if (isDuplicate(firstAliasAttempt, account)) appendUntilUnique(firstAliasAttempt, account)
        else firstAliasAttempt
      }

      this.theAccount match {
        case Full(a) => {
          val randomAliasName = newPublicAliasName(a)
          //create a new "otherAccount"
          val otherAccount = OtherAccount.createRecord.holder(realOtherAccHolder).publicAlias(randomAliasName).save
          a.otherAccounts(otherAccount.id.is :: a.otherAccounts.get).save
          Full(randomAliasName)
        }
        case _ => {
          logger.warn("Account not found to create aliases for")
          Failure("Account not found to create aliases for")
        }
      }
    }

    if (!publicAliasExists(realOtherAccHolder))
      createPublicAlias(realOtherAccHolder)
    else
      Full(realOtherAccHolder)
  }
  /**
   * A JSON representation of the transaction to be returned when successfully added via an API call
   */
  def whenAddedJson : JObject = {
    JObject(
      List(
        JField("obp_transaction", obp_transaction.get.whenAddedJson(id.toString))
      )
    )
  }
}

object OBPEnvelope extends OBPEnvelope with MongoMetaRecord[OBPEnvelope] with Loggable {

  class OBPQueryParam
  trait OBPOrder { def orderValue : Int }
  object OBPOrder {
    def apply(s: Option[String]): OBPOrder = s match {
      case Some("asc") => OBPAscending
      case _ => OBPDescending
    }
  }
  object OBPAscending extends OBPOrder { def orderValue = 1 }
  object OBPDescending extends OBPOrder { def orderValue = -1}
  case class OBPLimit(value: Int) extends OBPQueryParam
  case class OBPOffset(value: Int) extends OBPQueryParam
  case class OBPFromDate(value: Date) extends OBPQueryParam
  case class OBPToDate(value: Date) extends OBPQueryParam
  case class OBPOrdering(field: Option[String], order: OBPOrder) extends OBPQueryParam

  def envlopesFromJvalue(jval: JValue) : Box[OBPEnvelope] = {
    val created = fromJValue(jval)
    created match {
      case Full(c) => c.createAliases match {
          case Full(alias) => Full(c)
          case Failure(msg, _, _ ) => Failure(msg)
          case _ => Failure("Alias not created")
        }
      case _ => Failure("could not create Envelope form JValue")
    }
  }
}


class OBPTransaction private() extends BsonRecord[OBPTransaction]{
  def meta = OBPTransaction

  object this_account extends BsonRecordField(this, OBPAccount)
  object other_account extends BsonRecordField(this, OBPAccount)
  object details extends BsonRecordField(this, OBPDetails)

  def whenAddedJson(envelopeId : String) : JObject  = {
    JObject(List(JField("obp_transaction_uuid", JString(envelopeId)),
           JField("this_account", this_account.get.whenAddedJson),
             JField("other_account", other_account.get.whenAddedJson),
             JField("details", details.get.whenAddedJson)))
  }
}

object OBPTransaction extends OBPTransaction with BsonMetaRecord[OBPTransaction]

class OBPAccount private() extends BsonRecord[OBPAccount]{
  def meta = OBPAccount

  object holder extends StringField(this, 255)
  object number extends StringField(this, 255)
  object kind extends StringField(this, 255)
  object bank extends BsonRecordField(this, OBPBank)

  /**
   * @param moderatingAccount a temporary way to provide the obp account whose aliases should
   *  be used when displaying this account
   */
  def whenAddedJson: JObject = {

    JObject(List(JField("holder",
      JObject(List(
        JField("holder", JString(holder.get)),
        JField("alias", JString("no"))))),
      JField("number", JString(number.get)),
      JField("kind", JString(kind.get)),
      JField("bank", bank.get.whenAddedJson)))
  }
}

object OBPAccount extends OBPAccount with BsonMetaRecord[OBPAccount]{
  sealed abstract class AnAlias
  case object APublicAlias extends AnAlias
  case object APrivateAlias extends AnAlias
}

class OBPBank private() extends BsonRecord[OBPBank]{
  def meta = OBPBank

  object IBAN extends net.liftweb.record.field.StringField(this, 255)
  object national_identifier extends net.liftweb.record.field.StringField(this, 255)
  object name extends net.liftweb.record.field.StringField(this, 255)


  def whenAddedJson : JObject = {
    JObject(List( JField("IBAN", JString(IBAN.get)),
              JField("national_identifier", JString(national_identifier.get)),
              JField("name", JString(name.get))))
  }
}

object OBPBank extends OBPBank with BsonMetaRecord[OBPBank]



class OBPDetails private() extends BsonRecord[OBPDetails]{
  def meta = OBPDetails

  object kind extends net.liftweb.record.field.StringField(this, 255)
  object posted extends DateField(this)
  object other_data extends net.liftweb.record.field.StringField(this, 5000)
  object new_balance extends BsonRecordField(this, OBPBalance)
  object value extends BsonRecordField(this, OBPValue)
  object completed extends DateField(this)
  object label extends net.liftweb.record.field.StringField(this, 255)


  def formatDate(date : Date) : String = {
    OBPDetails.formats.dateFormat.format(date)
  }

  def whenAddedJson : JObject = {
    JObject(List( JField("kind", JString(kind.get)),
              JField("posted", JString(formatDate(posted.get))),
              JField("completed", JString(formatDate(completed.get))),
              JField("other_data", JString(other_data.get)),
              JField("new_balance", new_balance.get.whenAddedJson),
              JField("value", value.get.whenAddedJson)))
  }
}

object OBPDetails extends OBPDetails with BsonMetaRecord[OBPDetails]


class OBPBalance private() extends BsonRecord[OBPBalance]{
  def meta = OBPBalance

  object currency extends StringField(this, 5)
  object amount extends DecimalField(this, 0) // ok to use decimal?

  def whenAddedJson : JObject = {
    JObject(List( JField("currency", JString(currency.get)),
              JField("amount", JString(amount.get.toString))))
  }
}

object OBPBalance extends OBPBalance with BsonMetaRecord[OBPBalance]

class OBPValue private() extends BsonRecord[OBPValue]{
  def meta = OBPValue

  object currency extends net.liftweb.record.field.StringField(this, 5)
  object amount extends net.liftweb.record.field.DecimalField(this, 0) // ok to use decimal?

  def whenAddedJson : JObject = {
    JObject(List( JField("currency", JString(currency.get)),
              JField("amount", JString(amount.get.toString))))
  }
}

object OBPValue extends OBPValue with BsonMetaRecord[OBPValue]