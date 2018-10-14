package models

import boss._
import boss.jsany._
import cloud.EncryptionError
import tools.universa.SHA
import tools.universa.UniversaTools._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel, JSGlobal}

case class PublicKeyJS(packed: Seq[Byte]) extends BossCase

@js.native
@JSGlobal("Universa.pki.publicKey")
class _PublicKey extends js.Object {
  def pack(format: String): js.Array[Byte] = js.native
  def this(tpe: String, opts: js.Any) = this()
  def getBitStrength(): Int = js.native
  def fingerprint(): js.Array[Byte] = js.native
  def shortAddress(): js.Array[Byte] = js.native
  def longAddress(): js.Array[Byte] = js.native
  def verify(
    message: js.Array[Byte],
    signature: js.Array[Byte],
    options: js.Dictionary[_ <: Any]
  ): Boolean = js.native

  def encrypt(
    data: js.Array[Byte],
    options: js.Dictionary[_ <: Any]
  ): js.Array[Byte] = js.native
}

/**
 * Public key
 *
 * @constructor wrap js-key by facade
 */
@JSExportTopLevel("Universa.PublicKey")
class PublicKeyExported(val publicKey: PublicKey) extends AbstractKeyJS {
  override def getAbstractKey(): AbstractKey = publicKey

  def packed: js.Array[Byte] = publicKey.pack.toJSArray

  def toBOSS: js.Array[Byte] = publicKey.toBOSS.toJSArray

  def size: Int = publicKey.bits

  def fingerprint: js.Array[Byte] = publicKey.fingerprint.toJSArray

  def shortAddress: js.Array[Byte] = publicKey.shortAddress.toJSArray

  def longAddress: js.Array[Byte] = publicKey.longAddress.toJSArray

  //TODO add js test
  def isMatchingAddress(addr: js.Array[Byte]): Boolean = {
    val addrEncoded = encode64(addr)
    encode64(shortAddress) == addrEncoded || encode64(longAddress) == addrEncoded
  }
}


class PublicKey(key: _PublicKey) extends BossSerializable with AbstractKey {
  /** Pack to BOSS (without type property) */
  def toBOSSLite: Seq[Byte] = key.pack("BOSS")

  def pack: Seq[Byte] = key.pack("BOSS")

  /** Pack to BOSS (with type property) */
  def toBOSS: Seq[Byte] = Boss.dump(toJS)

  /** Pack to JS-compatible structure */
  def toJS: PublicKeyJS = PublicKeyJS(toBOSSLite)

  def this(bossEncoded: Seq[Byte]) =
    this(new _PublicKey("BOSS", bossEncoded.toJSArray))

  def this(serialized: PublicKeyJS) =
  	this(new _PublicKey("BOSS", serialized.packed.toJSArray))

  /** Get key size */
  def bits: Int = key.getBitStrength()

  /** Get key fingerprint */
  def fingerprint: Seq[Byte] = key.fingerprint()

  def shortAddress: Seq[Byte] = key.shortAddress()

  def longAddress: Seq[Byte] = key.longAddress()

  /** Encrypts binary data
    *
    * @param data - data to encrypt
    */
  def encrypt(data: Seq[Byte]): Seq[Byte] = {
    val options = mutable.HashMap(
      ("oaepHash", new SHA(1)),
      ("mgf1Hash", new SHA(1))
    )

    key.encrypt(data.toJSArray, options.toJSDictionary).toSeq
  }

  def decrypt(data: Seq[Byte]): Seq[Byte] =
    throw new EncryptionError("decrypt is not available for public key")

  /** Returns key info */
  def info: KeyInfo = {
    KeyInfo(HashMap(
      ("algorithm", "RSA_PUBLIC"),
      ("keyLength", length),
      ("tag", fingerprint.slice(1, 5))
    ))
  }

  /** Verifies binary data with signature
    *
    * @param data - data to verify
    * @param signature - private key signature
    */
  def verify(data: Seq[Byte], signature: Seq[Byte]): Boolean = {
    val options = mutable.HashMap(
      ("pssHash", new SHA(512)),
      ("mgf1Hash", new SHA(1))
    )

    key.verify(data.toJSArray, signature.toJSArray, options.toJSDictionary)
  }
}

object PublicKeyExported {
  def fromJS(serialized: BossCase): BossSerializable = {
    new PublicKey(serialized.asInstanceOf[PublicKeyJS])
  }

  @JSExportStatic("fromBOSS")
  def fromBOSS(bossEncoded: js.Array[Byte]): PublicKeyExported = {
//    val packed = Boss.load(bossEncoded).asInstanceOf[PublicKeyJS].packed.toJSArray
//    new PublicKey(new _PublicKey("BOSS", packed))
    val pk = Boss.load(bossEncoded).asInstanceOf[PublicKey]
    new PublicKeyExported(pk)
  }
}

case class KeyAddressJS(uaddress: Seq[Byte]) extends BossCase

class KeyAddress(var uaddress: Seq[Byte]) extends BossSerializable {
  def this(loaded: KeyAddressJS) = this(loaded.uaddress)
  def this(pubk: PublicKey, long: Boolean = false) =
    this(if (long) pubk.longAddress else pubk.shortAddress)

  def toJS: KeyAddressJS = KeyAddressJS(uaddress)

  def equals(otherId: KeyAddress): Boolean = {
    uaddress == otherId.uaddress
  }
}

object KeyAddress {
  @JSExportTopLevel("Universa.KeyAddress")
  def KeyAddressJS(key: PublicKeyExported): KeyAddress = {
    new KeyAddress(key.publicKey)
  }

  @JSExportTopLevel("Universa.UAddress")
  def UAddressJS(key: PublicKeyExported): js.Array[Byte] = {
    new KeyAddress(key.publicKey).uaddress.toJSArray
  }

  def fromJS(serialized: BossCase): BossSerializable = {
    new KeyAddress(serialized.asInstanceOf[KeyAddressJS])
  }
}

case class KeyRecordJS(key: PublicKey) extends BossCase

class KeyRecord(val key: PublicKey) extends BossSerializable {
  def this(loaded: KeyRecordJS) = this(loaded.key)

  def toJS: KeyRecordJS = KeyRecordJS(key)
}

object KeyRecord {
  def fromJS(serialized: BossCase): BossSerializable = {
    new KeyRecord(serialized.asInstanceOf[KeyRecordJS])
  }
}
