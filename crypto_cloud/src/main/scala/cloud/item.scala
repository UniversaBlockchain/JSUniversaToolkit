package cloud

import boss._
import boss.jsany._
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Represents cloud item contains data
 *
 * @constructor creates item with given API provider
 * @param service - cloud access API provider
 */
@JSExportTopLevel("CryptoCloud.Item")
class Item(val service: Api) {
  var publicData = HashMap[String, Any]()
  var privateData: Option[HashMap[String, Any]] = None
  var encryptingKeys = ListBuffer[Any]()
  var capsule: Option[Capsule] = None
  var record: Option[HashMap[String, Any]] = None
  var tag: Option[String] = None
  var isDestroyed: Boolean = false
  var capsuleSetup: Option[Capsule => Capsule] = None

  initialize

  /** Initializes new item */
  def initialize: Unit = {}

  /** Performs after-load setup in async mode */
  def setupLoadedData: Future[Unit] = Future.successful((): Unit)

  /** Loads item content by item ID
   *
   * @param id - item id
   */
  def loadId(id: Int): Future[Boolean] = {
    val getItem = service.execute("get_item", HashMap(("id", id)))

    getItem.map {
      response => {
        response.get("item") match {
          case None => throw new NotFound(s"item not found: $id")
          case Some(item) =>
            if (item == null) throw new NotFound(s"item not found: $id")
            else _loadRecord(item.asInstanceOf[HashMap[String, Any]])
            true
        }
      }
    }
  }

  /** Loads item content by item tag
   *
   * @param givenTag - item tag
   */
  def loadTag(givenTag: String): Future[Boolean] = {
    tag = Some(givenTag)

    val getItem = service.execute("get_item", HashMap(("tag", givenTag)))

    getItem.map {
      response =>
        response.get("item") match {
          case Some(item) => {
            if (item == null) false
            else {
              _loadRecord(item.asInstanceOf[HashMap[String, Any]])
              true
            }
          }
          case None => false
        }
    }
  }

  def _loadRecord(givenRecord: HashMap[String, Any]): Unit = {
    record = Some(givenRecord)
    tag = givenRecord.get("tag").asInstanceOf[Option[String]]
    val cap = new Capsule(givenRecord("data").asInstanceOf[Seq[Byte]])
    loadCapsule(cap)
  }

  /** Loads item from capsule
   *
   * @param givenCapsule - capsule to load
   */
  def loadCapsule(givenCapsule: Capsule): Unit = {
    givenCapsule.decrypt(service.getKeyRing)

    publicData = givenCapsule.publicData
    privateData = givenCapsule.privateData

    capsule = Some(givenCapsule)
  }

  /** Reloads item content from cloud */
  def reload: Future[Boolean] = {
    record match {
      case Some(value) => loadId(value("id").asInstanceOf[Int])
      case None => Future.failed( throw new ApiError("can't reload: no record exist"))
    }
  }

  /** Packs item to capsule */
  def toCapsule: Capsule = {
    if (tag.isEmpty) throw new ApiError("tag is required")

    val cap = new Capsule
    cap.publicData = publicData.clone
    cap.publicData("tag") = tag.get
    val gid = cap.publicData.getOrElse("gid", encode64(randomBytes(48)))
    cap.publicData("gid") = gid

    cap.privateData = privateData
    cap.addSigner(service.privateKey)
    val storageKey = service.storageKey
    storageKey.foreach(cap.addEncryptor)

    capsuleSetup match {
      case Some(func) => func(cap)
      case None => cap
    }
  }

  /** Saves item to cloud */
  def save: Future[HashMap[String, Any]] = {
    def update(record: HashMap[String, Any]): Future[HashMap[String, Any]] = {
      service.execute("update_item", HashMap(
        ("id", record("id")),
        ("serial", record("serial")),
        ("data", pack)
      )) map {
        response =>
          response.get("conflict") match {
            case None =>
              record("serial") = response("serial")
              response
            case Some(_) => mergeWithRemote()
          }
      }
    }

    def create(): Future[HashMap[String, Any]] = {
      service.execute("create_item", HashMap(
        ("tag", tag.get),
        ("data", pack)
      )) map {
        response =>
          record = Some(response)
          response
      }
    }

    record match {
      case None => create()
      case Some(value) => update(value)
    }
  }

  /** Removes item from cloud */
  def destroy: Future[Boolean] = {
    if (record.isEmpty) Future.failed( throw new ApiError("can't destroy: no record exist"))
    else {
      val realRecord = record.get

      service.execute("destroy_item", HashMap(
        ("id", realRecord("id"))
      )) map {
        _ =>
          isDestroyed = true
          true
      }
    }
  }

  override def toString: String = {
    val realId = id.getOrElse("null")
    val realTag = tag.getOrElse("null")
    val realSerial = serial.getOrElse("null")
    s"Item($realId,$realTag:$realSerial)"
  }

  /** Packs item to BOSS binary */
  def pack: Seq[Byte] = toCapsule.pack

  def mergeWithRemote(): HashMap[String, Any] =
    throw new WriteConflict("can't save: cloud version is newer then our")

  def id: Option[Int] = record.flatMap(_.get("id")).asInstanceOf[Option[Int]]
  def serial: Option[Int] = record.flatMap(_.get("serial")).asInstanceOf[Option[Int]]
  def expiresAt: Option[js.Date] = record.flatMap(_.get("expiresAt")).asInstanceOf[Option[js.Date]]
  /** Flag to check if item is not saved in cloud yet */
  def isNew: Boolean = record.isEmpty
  /** Shortcut to item public data */
  def pub: HashMap[String, Any] = publicData
  /** Shortcut to item private data */
  def priv: HashMap[String, Any] = privateData.get

  /** Sets data to private section with given path (ex. data.foo.bar = "abc")
   *
   * @param path - path to set data
   * @param value - data
   */
  def setPrivateData(path: String, value: Any): Unit = {
    privateData match {
      case Some(data) => set(data, path, value)
      case None =>
        val data = HashMap[String, Any]()
        set(data, path, value)
        privateData = Some(data)
    }
  }

  @JSExport("setTag")
  def setTag(givenTag: String): Unit = {
    tag = Some(givenTag)
  }

  @JSExport("setPrivateData")
  def setPrivateDataJS(path: String, value: js.Any): Unit = setPrivateData(path, toAny(value))

  @JSExport("privateData")
  def privateDataJS: js.Any = Boss.write[js.Any](priv)

  @JSExport("service")
  val serviceJS = service

  @JSExport("tag")
  def tagJS: String = {
    tag.orNull
  }

  @JSExport("record")
  def recordJS: js.Dictionary[Any] = {
    record.map(_.toJSDictionary).orNull
  }

  @JSExport("id")
  def idJS: Int = {
    record match {
      case None => -1
      case Some(reg) => reg("id").asInstanceOf[Int]
    }
  }

  @JSExport("serial")
  def serialJS: Int = {
    record match {
      case None => -1
      case Some(reg) => reg("serial").asInstanceOf[Int]
    }
  }

  @JSExport("save")
  def saveJS(): js.Promise[Any] = save

  @JSExport("reload")
  def reloadJS(): js.Promise[Boolean] = reload

  @JSExport("destroy")
  def destroyJS(): js.Promise[Any] = destroy

  @JSExport("capsuleSetup")
  def capsuleSetupJS(func: js.Function1[Capsule, Capsule]): Unit = {
    capsuleSetup = Some(func)
  }
}
