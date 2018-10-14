package cloud

import models.{AbstractKey, AbstractKeyJS, SymmetricKey, SymmetricKeyExported}

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Represents user private profile and registry
 *
 * @param service - cloud access API provider
 */
@JSExportTopLevel("CryptoCloud.Registry")
class Registry(override val service: Api) extends Item(service) {
  var storageKey = None: Option[SymmetricKey]
  var passwordKey = None: Option[AbstractKey]

  /** Initializes new registry */
  override def initialize(): Unit = {
    tag = Some("registry")
    setPrivateData("tokens", HashMap(("nicks", HashMap[String, Boolean]())))
    setPrivateData("main_key", service.privateKey.toRecord())
  }

  override def toString: String = {
    val partyId = service.partyId
    s"Registry(pty=$partyId)"
  }

  /** Loads registry from capsule
   *
   * @param capsule - capsule to load
   */
  override def loadCapsule(capsule: Capsule): Unit = {
    super.loadCapsule(capsule)
    val storageKeyData = priv("storage_key").asInstanceOf[HashMap[String, Any]]

    storageKey = Some(SymmetricKey(storageKeyData))
    priv.get("password_key").foreach{passwordKeyData =>
      passwordKey = Some(SymmetricKey(
        passwordKeyData.asInstanceOf[HashMap[String, Any]]
      ))
    }
  }

  /** Sets new password to access registry in cloud
   *
   * @param password - password for registry
   */
  def setPassword(password: String): Future[HashMap[String, Any]] = {
    passwordKey = Some(SymmetricKey.deriveFromPassword(password))
    setPrivateData("password_key", passwordKey.get.toRecord())
    save
  }

  /** Loads the unique tagged registry object for the account
   *  and create new proper one if it does not exist
   */
  def load: Future[Any] = {
    loadTag("registry") flatMap {
      isLoaded =>
        if (isLoaded) Future.successful(true)
        else {
          storageKey = Some(new SymmetricKey())
          setPrivateData("storage_key", storageKey.get.toRecord())
          save
        }
      }
  }

  /** Returns hash of registered nicks and their searchable options */
  def nicks: HashMap[String, Boolean] = {
    val tokens = priv("tokens").asInstanceOf[HashMap[String, Any]]
    tokens("nicks").asInstanceOf[HashMap[String, Boolean]]
  }

  /** Packs registry to capsule */
  override def toCapsule: Capsule = {
    val cap = super.toCapsule
    cap.addEncryptor(service.privateKey.publicKey)
    storageKey.foreach(key => cap.addEncryptor(key))
    passwordKey.foreach(key => cap.addEncryptor(key))
    cap
  }

  @JSExport("storageKey")
  def storageKeyJS: SymmetricKeyExported = new SymmetricKeyExported(storageKey.get)
}
