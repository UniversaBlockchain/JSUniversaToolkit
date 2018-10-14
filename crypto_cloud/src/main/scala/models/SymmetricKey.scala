package models

import cloud.{AuthenticationError, EncryptionError}
import tools.universa.ImplicitConverters.readBytesSafe
import tools.universa.UniversaTools.{encode64, ensureBytes, pbkdf2, randomBytes}
import tools.universa.{AESCTRTransformer, HMAC, SHA}

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Universa.SymmetricKey")
class SymmetricKeyExported(val symmetricKey: SymmetricKey) extends AbstractKeyJS {
  override def getAbstractKey(): AbstractKey = symmetricKey

  def pack(): js.Array[Byte] = symmetricKey.pack.toJSArray
}


/** Symmetric key class
  *
  * @constructor create a new symmetric key
  * @param keyBytes if present, should be 32 bytes long use bytes from it as keys.
  *        Otherwise a random key will be created.
  * @param keyInfo optinal key information, used by
  *        SummetricKey.deriveFromPassword to save key derivation data.
  */
class SymmetricKey (
                     val keyBytes: Seq[Byte] = randomBytes(32),
                     val keyInfo: Option[HashMap[String, Any]] = None
                   ) extends AbstractKey {
  if (keyBytes.length != 32) {
    val keyLen = keyBytes.length
    throw new EncryptionError(s"wrong symmetric key size: $keyLen: $keyBytes")
  }

  var _info: Option[KeyInfo] = None

  if (keyInfo.isDefined) {
    val infoCopy = HashMap[String, Any]() ++= keyInfo.get
    infoCopy("algorithm") = "AES256"
    infoCopy("keyLength") = length
    _info = Some(KeyInfo(infoCopy))
  }

  /** Packs key to BOSS binary */
  def pack: Seq[Byte] = keyBytes

  /** Returns key info */
  def info: KeyInfo = _info match {
    case Some(value) => value
    case None => KeyInfo(HashMap(
      ("algorithm", "AES256"),
      ("keyLength", length)
    ))
  }

  def setInfo(in: KeyInfo): Unit = _info = Some(in)

  /** Key size in bits */
  def bits: Int = 256

  override def toString: String = {
    val encoded = encode64(keyBytes)
    s"SymmetricKey($encoded)"
  }

  /** Decrypts binary data with key
    *
    * @param data - encrypted binary data
    */
  def decrypt(data: Seq[Byte]): Seq[Byte] = {
    val list = data.toList
    val part1 = list.slice(0, 16)
    val part2 = list.slice(16, list.size)

    new AESCTRTransformer(keyBytes, part1.toSeq).transform(part2.toSeq)
  }

  /** Encrypts binary data with key
    *
    * @param data - binary data to encrypt
    */
  def encrypt(data: Seq[Byte]): Seq[Byte] = {
    val iv = randomBytes(16)
    val result = new ListBuffer[Byte]
    result ++= iv
    result ++= new AESCTRTransformer(keyBytes, iv).transform(data)
    // FIXME: dirty way to ensure that array will be uint8array on transit
    ensureBytes(result)
  }

  def _newHMAC: HMAC = new HMAC(new SHA(256), keyBytes)

  /** Decrypts binary data with Encrypt then Authenticate (EtA) method
    *
    * @param data - encrypted binary data
    */
  def etaDecrypt(data: Seq[Byte]): Seq[Byte] = {
    val dataList = data.toList
    // FIXME: Without ensure bytes there is array instead of uint8array
    val cleanData = ensureBytes(dataList.slice(16, dataList.size - 32))
    val calculatedHmac = _newHMAC.get(cleanData)
    val readHmac = dataList.slice(dataList.size - 32, dataList.size)

    if (encode64(readHmac) != encode64(calculatedHmac))
      throw new AuthenticationError

    decrypt(dataList.slice(0, dataList.size - 32))
  }

  /** Encrypts binary data with Encrypt then Authenticate (EtA) method
    *
    * @param data - binary data to encrypt
    */
  def etaEncrypt(data: Seq[Byte]): Seq[Byte] = {
    val encrypted = encrypt(data)
    // FIXME: Without ensure bytes there is array instead of uint8array
    val calculatedHmac = _newHMAC.get(ensureBytes(encrypted.slice(16, encrypted.size)))

    encrypted ++ calculatedHmac
  }
}

/** Factory for [[models.SymmetricKey]] instances. */
object SymmetricKey {
  /** Creates SymmetricKey from password
    *
    * @param password the password to generate key
    */
  def deriveFromPassword(password: String): SymmetricKey = {
    if (password.length == 0)
      throw new EncryptionError("missing password")
    if (password.length < 6)
      throw new EncryptionError("password is too short")

    val iterations = 10000
    val keyLength = 32

    val dk = pbkdf2(new SHA(256), HashMap(
      ("password", password),
      ("salt", "attesta"),
      ("iterations", iterations),
      ("keyLength", keyLength)
    ))

    new SymmetricKey(dk, Some(HashMap(
      ("prf", "HMAC_SHA256"),
      ("rounds", iterations),
      ("keyLength", keyLength)
    )))
  }

  /** Creates SymmetricKey from key record object
    *
    * @param record the hashmap of key record
    */
  def apply(record: HashMap[String, Any]): SymmetricKey = {
    val data = readBytesSafe(record("data"))
    val keyInfo = readBytesSafe(record("keyInfo"))

    if (data.isEmpty)
      throw new EncryptionError(s"wrong key record: $keyInfo -> $data")

    val ki = KeyInfo(keyInfo)
    if (ki.algorithm != "AES256")
      throw new EncryptionError(s"Unknown key type for main storage: $ki")

    val sk = new SymmetricKey(data)
    sk.setInfo(ki)
    sk
  }
}
