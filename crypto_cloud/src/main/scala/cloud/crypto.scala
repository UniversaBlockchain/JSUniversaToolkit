package cloud

import boss._
import boss.jsany._
import models._
import tools.universa.ImplicitConverters._
import tools.universa.SHA
import tools.universa.UniversaTools._

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.control.Breaks._
import scala.util.control.NoStackTrace

class EncryptionError(
  val message: String,
  val details: HashMap[String, Any] = HashMap[String, Any]()
) extends Exception(message) with NoStackTrace {
  @JSExport("message")
  val messageJS = message

  @JSExport("details")
  val detailsJS = details.toJSDictionary

  override def toString: String = s"EncryptionError: $message"
}

@JSExportTopLevel("CryptoCloud.AuthenticationError")
class AuthenticationError(
  override val message: String = "Authentication failed",
  override val details: HashMap[String, Any] = HashMap[String, Any]()
) extends EncryptionError(message, details) {}

object AuthenticationError {
  def apply(message: String, details: HashMap[String, Any]): AuthenticationError = {
    new AuthenticationError(message, details)
  }

  def apply(details: HashMap[String, Any]): AuthenticationError = {
    new AuthenticationError("Authentication failed", details)
  }
}

/** Represents general key ring implementation */
abstract class KeyRing {
  /** Returns matching keys from key ring for given key info
   *
   * @param ki - key info
   */
  def matchingKeys(ki: KeyInfo): ListBuffer[AbstractKey]

  /** Decrypts data with given keys
   *
   * @param encryptedData - hashmap contains keys and encrypted data
   */
  def decrypt(encryptedData: HashMap[String, Any]): Option[Seq[Byte]] = {
    val cipherText = encryptedData("data").asInstanceOf[Seq[Byte]]
    var result: Option[Seq[Byte]] = null
    val keyRecords = encryptedData("keys").asInstanceOf[ListBuffer[HashMap[String, Any]]]

    breakable {
      for (keyRecord <- keyRecords) {
        val dataKeyInfo = KeyInfo(readBytesSafe(keyRecord("keyInfo")))

        for (key <- matchingKeys(dataKeyInfo)) {
          try {
            val unpacked = key.decrypt(readBytesSafe(keyRecord("key")))
            val sk = new SymmetricKey(unpacked)
            result = Some(sk.etaDecrypt(cipherText))
            break
          } catch {
            case e: Exception =>
          }
        }
      }
    }

    result
  }
}

/** Container for KeyRing static methods */
object KeyRing {
  /** Encrypts data with given keys
   *
   * @param options - hashmap of keys and data to encrypt
   */
  def encrypt(options: HashMap[String, Any]): HashMap[String, Any] = {
    val keys = options("keys").asInstanceOf[ListBuffer[AbstractKey]]
    if (keys.isEmpty) throw new EncryptionError("can't encrypt: no keys provided")

    val data = options("data").asInstanceOf[HashMap[String, Any]]
    val fill = options("fill").asInstanceOf[Double]

    val mainKey = new SymmetricKey()
    val packedKey = mainKey.pack()
    val encryptedKeys = keys.map(key => HashMap(
      ("keyInfo", key.info().pack),
      ("key", key.encrypt(packedKey))
    ))

    val writer = new BossWriter()
    writer.write(data)
    writer.write(randomBytes(js.Math.random().toInt))
    val encryptedData = mainKey.etaEncrypt(writer.get())

    HashMap(
      ("keys", encryptedKeys),
      ("data", encryptedData)
    )
  }
}

/** Simple key ring */
class SimpleKeyRing(val keys: ListBuffer[AbstractKey]) extends KeyRing {
  /** Returns matching keys from key ring for given key info
   *
   * @param ki - key info
   */
  def matchingKeys(ki: KeyInfo): ListBuffer[AbstractKey] =
    keys.filter(_.info.matchType(ki))
}

/** Password-protected key ring */
class PasswordKeyRing(val password: String) extends KeyRing {
  /** Returns matching keys from key ring for given key info
   *
   * @param ki - key info
   */
  def matchingKeys(ki: KeyInfo): ListBuffer[AbstractKey] = {
    if (!ki.isPassword) return new ListBuffer[AbstractKey]

    val dk = pbkdf2(new SHA(256), HashMap(
      ("password", password),
      ("salt", "attesta"),
      ("iterations", ki.rounds.get),
      ("keyLength", ki.keyLength)
    ))
    ListBuffer(new SymmetricKey(dk))
  }
}
