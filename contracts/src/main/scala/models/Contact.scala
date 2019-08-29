package models

import cloud.{Api, Item}
import v1import.ImportFromV1

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Date
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global
import tools.universa.UniversaTools._

@JSExportTopLevel("Universa.Contact")
class ContactExported(val contact: Contact) extends js.Object {
  val firstName: String = contact.firstName.orNull
  val lastName: String = contact.lastName.orNull

  def toCloud(api: Api): js.Promise[Double] = {
    val mapToSave = contact.toMap(false)

    val itemF = contact.cloudId.map { cloudId =>
      api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    }.getOrElse {
      val item = new Item(api)
      item.setTag(ContactExported.ItemTag)
      Future.successful(item)
    }
    itemF.flatMap { item =>
      item.setPrivateData("contact", mapToSave)
      item.setPrivateData("version", 2)
      item.save.map { _ =>
        val itemId = item.id.get
        contact.cloudId = Option(itemId)
        itemId.toDouble
      }
    }.toJSPromise
  }

  def toJsObject(): js.Dictionary[_] = contact.toMap(true).toJSDictionary
  def toJsObjectForChat(): js.Dictionary[_] = contact.toMap(true, true).toJSDictionary

  def deleteFromCloud(api: Api): js.Promise[Boolean] = {
    val cloudId = contact.cloudId.getOrElse(throw new RuntimeException("Not known cloud id"))
    val loaded = api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    loaded.flatMap(_.destroy).toJSPromise
  }

}

class Contact(
                      birthdate: Date,
                      emails: HashMap[String, Boolean],
                      val firstName: Option[String],
                      val lastName: Option[String],
                      partyId: Int,
                      var cloudId: Option[Int],
                      gender: Option[String],
                      nicks: HashMap[String, Boolean],
                      sendName: Option[Boolean],
                      packedAvatarImage: Option[String],
                      packedAvatarImageType: Option[String],
                      phoneNumbers: HashMap[String, Boolean],
                      publicKey: PublicKey,
                      profileId: Option[String],
                      lastSerial: Option[Int],
                      settings: HashMap[String, Any] //Map(uThreshold -> 99, uAuto -> false, uAmount -> 10)
                    ) {

  def toMap(jsView: Boolean, forChat: Boolean = false): HashMap[String, Any] = {
    def mapToDict(field: HashMap[String, _]) = {
      if (jsView && field != null) field.toJSDictionary else field
    }

    val result = HashMap(
      "birthdate" -> null,
      "emails" -> mapToDict(emails),
      "partyId" -> partyId,
      "gender" -> gender.orNull,
      "packed_avatar_image" -> packedAvatarImage.orNull,
      "packed_avatar_image_type" -> packedAvatarImageType.orNull,
      "phone_numbers" -> mapToDict(phoneNumbers),
      "profile_id" -> profileId.orNull,
      "lastSerial" -> lastSerial.getOrElse(null),
      "settings" -> mapToDict(settings)
    )

    if (!forChat) {
      result ++= HashMap(
        "nicks" -> mapToDict(nicks),
        "publicKey" -> {
          val lite = publicKey.toBOSSLite
          if (jsView) lite.toJSArray
          else asByteString(lite)
        },
        "send_name" -> sendName.orNull,
        "cloudId" -> cloudId.orNull,
        "first_name" -> firstName.orNull,
        "last_name" -> lastName.orNull,
      )
    } else {
      if (sendName.getOrElse(false)) {
        result ++= HashMap(
          "first_name" -> firstName.getOrElse(""),
          "last_name" -> lastName.getOrElse(""),
        )
      }
    }

    result
  }
}

object ContactExported {
  val ItemTag = "contact"

  @JSExportStatic("allFromCloud")
  def downloadAllFromCloud(api: Api): js.Promise[js.Array[ContactExported]] = {
    cloud.Tools.importAllObjects(api, ItemTag, importContactFromCloudV2).toJSPromise
  }

  private def importContactFromCloudV2(item: Item): Option[ContactExported] = {
    item.priv.get("version") match {
      case Some(2) =>
        val result = ImportFromV1.importContactFromCloud(item)
        result.map{res =>
          res.contact.cloudId = item.id
          res
        }
      case _ => None
    }
  }

  @JSExportStatic("fromJsObject")
  def fromJsObject(dict: js.Dictionary[_]): ContactExported = {
    val map: mutable.Map[String, _] = dict
    val contact = convertFromJsToContact(map)
    new ContactExported(contact)
  }

  private def convertFromJsToContact(contactHashMap: mutable.Map[String, _]): Contact = {
    val contact = new Contact(
      birthdate = null,
      packedAvatarImageType = Option(contactHashMap("packed_avatar_image_type").asInstanceOf[String]),
      packedAvatarImage = Option(contactHashMap("packed_avatar_image").asInstanceOf[String]),
      settings = {
        Option(contactHashMap("settings").asInstanceOf[js.Dictionary[Any]])
          .map(jsDict => mutable.HashMap(jsDict.toSeq: _*))
          .getOrElse(mutable.HashMap.empty[String, Any])
      },
      lastName = Option(contactHashMap("last_name").asInstanceOf[String]),
      sendName = Option(contactHashMap("send_name").asInstanceOf[Boolean]),
      gender = Option(contactHashMap("gender").asInstanceOf[String]),
      emails =
        Option(contactHashMap("emails").asInstanceOf[js.Dictionary[Boolean]])
          .map(jsDict => mutable.HashMap(jsDict.toSeq: _*))
          .getOrElse(mutable.HashMap.empty[String, Boolean]),
      nicks = {
        val jsDict = contactHashMap("nicks").asInstanceOf[js.Dictionary[Boolean]]
        mutable.HashMap(jsDict.toSeq: _*)
      },
      partyId = contactHashMap("partyId").asInstanceOf[Int],
      phoneNumbers = {
        Option(contactHashMap("phone_numbers").asInstanceOf[js.Dictionary[Boolean]])
          .map(jsDict => mutable.HashMap(jsDict.toSeq: _*))
          .getOrElse(mutable.HashMap.empty[String, Boolean])
      },
      lastSerial = Option(contactHashMap("lastSerial").asInstanceOf[Int]),
      profileId = Option(contactHashMap("profile_id").asInstanceOf[String]),
      cloudId = Option(contactHashMap("cloudId").asInstanceOf[Int]),
      publicKey = {
        val keyString = contactHashMap("publicKey").asInstanceOf[js.Array[Byte]]
        new PublicKey(keyString.toSeq)
      },
      firstName = Option(contactHashMap("first_name").asInstanceOf[String])
    )
    contact
  }

  @JSExportStatic("fromCloud")
  def downloadFromCloudJS(cloudId: Double, api: Api): js.Promise[ContactExported] = {
    val loaded = api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    loaded
      .map{item => importContactFromCloudV2(item).get}
      .toJSPromise
  }

}
