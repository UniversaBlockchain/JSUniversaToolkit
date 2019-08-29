package v1import

import cloud.Tools._
import cloud.{Api, Item}
import models.contracts.ContractFactory
import models.{Contact, ContactExported, ContractExported, Pair, PairExported, PrivateKey, PublicKey}
import tools.universa.UniversaTools._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.concurrent.ExecutionContext.Implicits.global

object ImportFromV1 {

  type JSDict = js.Dictionary[Any]

  @JSExportTopLevel("Universa.Import.Contacts")
  def importContacts(api: Api): js.Promise[js.Array[JSDict]] = {
    importAllObjects(api, "contact", importContact).toJSPromise
  }

  def importContact(item: Item): Option[JSDict] = {
    item.priv.get("version") match {
      case None =>
        val contact: Option[ContactExported] = importContactFromCloud(item)
        Some(
          js.Dictionary(
            "contact" -> contact.get,
            "extra" -> js.Dictionary[Any]()
          )
        )
      case _ => //new version contact
        None
    }
  }

  def importContactFromCloud(item: Item): Option[ContactExported] = {
    val contactHashMap = item.priv("contact").asInstanceOf[mutable.HashMap[String, Any]]
    val contact: Contact = convertToContact(contactHashMap)
    Some(
      new ContactExported(contact)
    )
  }

  private def convertToContact(contactHashMap: mutable.Map[String, _]): Contact = {
    var realProfileId = ""

    try { realProfileId = contactHashMap("profile_id").asInstanceOf[String] }
    catch { case _: Throwable => realProfileId = contactHashMap("profile_id").asInstanceOf[Int].toString }

    val contact = new Contact(
      birthdate = null,
      packedAvatarImageType = Option(contactHashMap("packed_avatar_image_type").asInstanceOf[String]),
      packedAvatarImage = Option(contactHashMap("packed_avatar_image").asInstanceOf[String]),
      settings = contactHashMap("settings").asInstanceOf[mutable.HashMap[String, Any]],
      lastName = Option(contactHashMap("last_name").asInstanceOf[String]),
      sendName = Option(contactHashMap("send_name").asInstanceOf[Boolean]),
      gender = Option(contactHashMap("gender").asInstanceOf[String]),
      emails = Option(contactHashMap("emails").asInstanceOf[mutable.HashMap[String, Boolean]])
        .getOrElse(HashMap.empty[String, Boolean]),
      nicks = contactHashMap("nicks").asInstanceOf[mutable.HashMap[String, Boolean]],
      partyId = contactHashMap("partyId").asInstanceOf[Int],
      phoneNumbers = contactHashMap("phone_numbers").asInstanceOf[mutable.HashMap[String, Boolean]],
      lastSerial = Option(contactHashMap("lastSerial").asInstanceOf[Int]),
      profileId = Option(realProfileId),
      cloudId = None,
      publicKey = {
        val keyString = contactHashMap("publicKey").asInstanceOf[String]
        new PublicKey(asByteSeq(keyString))
      },
      firstName = Option(contactHashMap("first_name").asInstanceOf[String])
    )
    contact
  }

  @JSExportTopLevel("Universa.Import.Contracts")
  def importContracts(api: Api): js.Promise[js.Array[JSDict]] = {
    val contracts = importAllObjects(api, "contract", importContract)
    contracts.map{ arr =>
      val map = arr.map(dict => (dict("extra").asInstanceOf[JSDict]("id").asInstanceOf[String], dict)).toMap
      arr.foreach { dict =>
        val mainSibling = dict("extra").asInstanceOf[JSDict]("main_sibling_id").asInstanceOf[String]
        val foundMain = map.get(mainSibling)
        if (foundMain.isDefined) {
          dict("contract").asInstanceOf[ContractExported].contract.mainSiblingId =
            Some(foundMain.get("contract").asInstanceOf[ContractExported].contract.original.hashId)
        }
      }
    }
    contracts.toJSPromise
  }

  def importContract(item: Item): Option[JSDict] = {
    val contractHashMapOpt = item.priv.get("contract").map(_.asInstanceOf[HashMap[String, Any]])

    contractHashMapOpt.map { contractHashMap =>
      val pending = None //Option(contractHashMap("pending").asInstanceOf[Seq[Byte]])
      val temp = {
        val tempValue = contractHashMap("temp")
        if (tempValue.isInstanceOf[String]) Option(tempValue.asInstanceOf[String]).map(asByteSeq)
        else Option(tempValue.asInstanceOf[Seq[Byte]])
      }
      //modified = Seq[Byte]
      val original = contractHashMap("original").asInstanceOf[Seq[Byte]]

      val contract =
        if (original != null) {
          val imported = contractHashMap("imported").asInstanceOf[Seq[Byte]]
          if (imported != null) ContractFactory.buildFromBytes(imported, temp, pending)
          else ContractFactory.buildFromBytes(original, temp, pending)
        }
        else ContractFactory.buildFromBytes(temp.get, None, None)
      contract.state = contractHashMap("state").asInstanceOf[String]
      contract.contractName = Option(contractHashMap("name").asInstanceOf[String])
      //    contract.filesCloudId = contractHashMap("attachments").asInstanceOf[ListBuffer[Double]].headOption
      // contract.contractCloudId = Option(contractHashMap("cloud_id").asInstanceOf[Double])
      contract.errors = contractHashMap("errors").asInstanceOf[ListBuffer[String]]

      js.Dictionary(
        "contract" -> contract.toExport(),
        "extra" -> js.Dictionary[Any](
          "createdAt" -> contractHashMap("createdAt"),
          "id" -> contractHashMap("id"),
          "main_sibling_id" -> contractHashMap("main_sibling_id")
        )
      )
    }
  }

  @JSExportTopLevel("Universa.Import.Pairs")
  def importPairs(api: Api): js.Promise[js.Array[JSDict]] = {
    importAllObjects(api, "pair", importPair).toJSPromise
  }

  def importPair(item: Item): Option[JSDict] = {
    item.priv.get("version") match {
      case None =>
        val pairHashMap = item.priv("pair").asInstanceOf[HashMap[String, Any]]
        val pair: Pair = new Pair(
          new PublicKey(pairHashMap("publicKey").asInstanceOf[Seq[Byte]]),
          pairHashMap.get("privateKey").map(pk => new PrivateKey(pk.asInstanceOf[Seq[Byte]])),
          createdAt = pairHashMap("createdAt").asInstanceOf[js.Date],
          name = pairHashMap("name").asInstanceOf[String]
        )

        Some(
          js.Dictionary(
            "pair" -> new PairExported(pair),
            "extra" -> js.Dictionary[Any](
              "id" -> pairHashMap("id")
            )
          )
        )
      case _ => None
    }
  }
}
