package cloud

import boss._
import boss.jsany._
import models.{AbstractKey, AbstractKeyJS}

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Represents protected capsule of data
 *
 * @constructor creates instance from BOSS binary
 * @param packed - BOSS binary
 */
@JSExportTopLevel("CryptoCloud.Capsule")
class Capsule(packed: Seq[Byte] = Seq[Byte]()) {
  type MutableMap = HashMap[String, Any]
  private var newSigners = ListBuffer[MutableMap]()
  private var encryptors = ListBuffer[AbstractKey]()
  var privateData: Option[MutableMap] = None
  var publicData = HashMap[String, Any]()

  var signatures = ListBuffer[MutableMap]()
  private var existingSignatures = ListBuffer[MutableMap]()

  private var encryptedData = HashMap[String, Any]()
  private var signers = ListBuffer[MutableMap]()
  private var validSigners = ListBuffer[MutableMap]()
  private var invalidSigners = ListBuffer[MutableMap]()

  var isPartiallySigned = false
  var isSigned = false

  if (packed.nonEmpty) {
    val loaded = Boss.load(packed).asInstanceOf[MutableMap]
    val content = loaded("content").asInstanceOf[Seq[Byte]]
    signatures = loaded("signatures").asInstanceOf[ListBuffer[MutableMap]]
    existingSignatures = signatures

    val toplevel = Boss.load(content).asInstanceOf[MutableMap]
    publicData = toplevel("public").asInstanceOf[MutableMap]
    encryptedData = toplevel("private").asInstanceOf[MutableMap]
    signers = toplevel("signers").asInstanceOf[ListBuffer[MutableMap]]

    checkSignatures(content)
  }

  private def checkSignatures(content: Seq[Byte]): Unit = {
    validSigners = ListBuffer()
    invalidSigners = ListBuffer()

    for (signer <- signers) {
      val signerId = signer("id").asInstanceOf[String]
      val mayBeSignature = existingSignatures.find(_("key") == signerId)

      if (mayBeSignature.isEmpty) {
        invalidSigners += signer
      } else {
        val signature = mayBeSignature.get
        signer("signaturePresent") = true
        val key = new models.PublicKey(signer("key").asInstanceOf[Seq[Byte]])
        var ok = false

        try {
          ok = key.verify(content, signature("signature").asInstanceOf[Seq[Byte]])
        } catch {
          case e: Exception => println(e)
        }

        if (ok) {
          validSigners += signer
          signer("signed") = true
        } else {
          invalidSigners += signer
          signer("signed") = false
        }
      }
    }

    val validPresent = validSigners.nonEmpty
    val invalidPresent = invalidSigners.nonEmpty

    isPartiallySigned = validPresent && invalidPresent
    isSigned = validPresent && !invalidPresent
  }

  /** Decrypts private data of capsule with given key ring
   *
   * @param keyRing - key ring
   */
  def decrypt(keyRing: KeyRing): Boolean = {
    if (encryptedData.isEmpty) return false

    val dd = keyRing.decrypt(encryptedData)

    dd match {
      case Some(value) =>
        privateData = Some(Boss.load(value).asInstanceOf[MutableMap])
        true
      case _ => false
    }
  }

  /** Decrypts private data of capsule with given key
   *
   * @param key - key for decryption
   */
  def decrypt(key: AbstractKey): Boolean =
    decrypt(new SimpleKeyRing(ListBuffer(key)))

  /** Decrypts private data of capsule with given password
   *
   * @param password - password for decryption
   */
  def decrypt(password: String): Boolean =
    decrypt(new PasswordKeyRing(password))

  /** Adds signer to capsule
   *
   * @param signer - private key to sign
   * @param data - signer info
   */
  def addSigner(
    signer: models.PrivateKey,
    data: MutableMap = HashMap[String, Any]()
  ): Unit = {
    val label = newSigners.length.toString

    newSigners += HashMap(
      ("id", label),
      ("data", data),
      ("key", signer)
    )
  }

  /** Adds encryptor
   *
   * @param key - encryptor key
   */
  @JSExport("addEncryptor")
  def addEncryptor(key: AbstractKeyJS): Unit = addEncryptor(key.getAbstractKey())

  def addEncryptor(key: AbstractKey): Unit = encryptors += key

  /** Packs capsule to BOSS binary */
  def pack: Seq[Byte] = {
    val privateEncoded = privateData match {
      case null => null
      case None => null
      case Some(value) =>
        KeyRing.encrypt(HashMap(
          ("keys", encryptors),
          ("data", value),
          ("fill", js.Math.round(71))
        ))
    }

    val payload = HashMap(
      ("private", privateEncoded),
      ("public", publicData),
      ("type", "capsule")
    )

    if (newSigners.nonEmpty) {
      payload("signers") = newSigners.map(signer => HashMap(
        ("id", signer("id")),
        ("key", signer("key").asInstanceOf[models.PrivateKey].publicKey.pack()),
        ("data", signer("data"))
      ))
    }

    val content = Boss.dump(payload)
    val outer = HashMap(
      "signatures" -> newSigners.map(signer => HashMap(
          "key" -> signer("id"),
          "signature" -> signer("key").asInstanceOf[models.PrivateKey].sign(content)
        )),
      "content" -> content
    )

    Boss.dump(outer)
  }
}
