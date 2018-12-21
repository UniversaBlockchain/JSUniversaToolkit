package models.account

import cloud.{Api, Item}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

@JSExportTopLevel("Universa.Profile")
class AccountProfileExported(val profile: AccountProfile) extends js.Object {
  def firstName: String = profile.firstName.orNull
  def lastName: String = profile.lastName.orNull
  def isPublic: Boolean = profile.isPublic
  def settings: js.Dictionary[Any] = profile.settings.toJSDictionary

  def updateFirstName(firstName: String): Unit = {
    profile.firstName = Option(firstName)
  }

  def updateLastName(lastName: String): Unit = {
    profile.lastName = Option(lastName)
  }

  def setPublic(isPublic: Boolean): Unit = {
    profile.isPublic = isPublic
  }


  def toCloud(api: Api): js.Promise[Double] = {
    val itemF = profile.cloudId.map { cloudId =>
      api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    }.getOrElse {
      var item: Item = null
      item = new Item(api)
      item.setTag(AccountProfileExported.ItemTag)
      Future.successful(item)
    }
    itemF.flatMap { item =>
      item.setPrivateData("profile", profile.toMap)
      item.setPrivateData("version", 2)
      item.save.map { _ =>
        val itemId = item.id.get.toDouble
        profile.cloudId = Option(itemId)
        itemId
      }
    }.toJSPromise
  }

  def deleteFromCloud(api: Api): js.Promise[Boolean] = {
    val cloudId = profile.cloudId.getOrElse(throw new RuntimeException("Not known cloud id"))
    val loaded = api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    loaded.flatMap(_.destroy).toJSPromise
  }
}

class AccountProfile(
                      var firstName: Option[String],
                      var lastName: Option[String],
                      var isPublic: Boolean = false,
                      val settings: mutable.HashMap[String, Any], ////Map(uThreshold -> 99, uAuto -> false, uAmount -> 10)
                      var cloudId: Option[Double] = None
                    ) extends js.Object {

  def toMap: mutable.HashMap[String, Any] = {
    mutable.HashMap(
      "firstName" -> firstName.orNull,
      "lastName" -> lastName.orNull,
      "is_public" -> isPublic,
      "send_name" -> isPublic,
      "settings" -> settings,
      "cloudId" -> cloudId.map(_.toString).orNull
    )
  }

}

object AccountProfileExported {
  val ItemTag = "profile"

  private def fromItemOpt(item: Item): Option[AccountProfileExported] = {
    if (!item.priv.get("version").asInstanceOf[Option[Int]].contains(2)) return None

    val profileHashMap = item.priv("profile").asInstanceOf[mutable.HashMap[String, Any]]

    val profile = new AccountProfile(
      firstName = Option(profileHashMap("firstName").asInstanceOf[String]),
      lastName = Option(profileHashMap("lastName").asInstanceOf[String]),
      isPublic = profileHashMap("is_public").asInstanceOf[Boolean],
      settings = profileHashMap("settings").asInstanceOf[mutable.HashMap[String, Any]],
      cloudId = Option(profileHashMap("cloudId").asInstanceOf[String])
        .map(a => a.toDouble)
        .orElse(item.id.map(_.toDouble))
    )
    Some(new AccountProfileExported(profile))
  }

  @JSExportStatic("FromCloud")
  def downloadFromCloud(api: Api, cloudId: String): js.Promise[AccountProfileExported] = {
    val loadMap =
      if (cloudId != null && cloudId.nonEmpty) {
        mutable.HashMap[String, Any]("id" -> cloudId.toDouble)
      } else {
        mutable.HashMap[String, Any]("tag" -> ItemTag) //not needed limit 1
      }
    val loaded = api.getItem(
      { api => new Item(api) },
      loadMap
    )

    loaded
      .map(item => fromItemOpt(item).orNull)
      .toJSPromise
  }

  @JSExportStatic("create")
  def createProfile(
           firstName: String,
           lastName: String,
           isPublic: Boolean
           ): AccountProfileExported = {
    val profile = new AccountProfile(Option(firstName), Option(lastName), isPublic, mutable.HashMap.empty[String, Any], None)
    new AccountProfileExported(profile)
  }
}