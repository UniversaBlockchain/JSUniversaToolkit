package models

import cloud.EncryptionError
import tools.universa.SHA

import scala.scalajs.js
import js.annotation._
import js.JSConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap

import tools.universa.UniversaTools._

@js.native
@JSGlobal("Universa.pki.rsa")
object KeyGenerator extends js.Object {

  @JSName("createPrivateKey")
	def create(
    bits: Int,
    exp: Int,
    callback: js.Function2[js.Dynamic, PrivateKeyJS, Unit]
  ): Unit = js.native
}

@js.native
@JSGlobal("Universa.pki.extendedSignature")
object ExtendedSignatureJS extends js.Object {
  def sign(key: PrivateKeyJS, data: js.Array[Byte]): js.Array[Byte] = js.native
}



@js.native
@JSGlobal("Universa.pki.privateKey")
class PrivateKeyJS extends js.Object {
  val publicKey: _PublicKey = js.native

  def pack(format: String): js.Array[Byte] = js.native
  def this(tpe: String, opts: js.Any) = this()
  def getBitStrength(): Int = js.native
  def sign(
    message: js.Array[Byte],
    options: js.Dictionary[_ <: Any]
  ): js.Array[Byte] = js.native

  def decrypt(
    encrypted: js.Array[Byte],
    options: js.Dictionary[_ <: Any]
  ): js.Array[Byte] = js.native
}

/**
 * Private key
 *
 * @constructor wrap js-key by facade
 */
@JSExportTopLevel("Universa.PrivateKey")
class PrivateKeyExported(val privateKey: PrivateKey) extends AbstractKeyJS {
  override def getAbstractKey(): AbstractKey = privateKey

//  override def pack(): js.Array[Byte] = privateKey.pack.toJSArray

  def this(bossEncoded: js.Array[Byte]) = {
    this(new PrivateKey(bossEncoded))
  }

  def toBOSS: js.Array[Byte] = privateKey.pack.toJSArray

  def toBase64: String = encode64(privateKey.pack)

  def size: Int = privateKey.bits

  def publicKey: PublicKeyExported = {
    val pk = privateKey.publicKey
    new PublicKeyExported(pk)
  }

  def isMatchingAddress(addr: js.Array[Byte]): Boolean = privateKey.isMatchingAddress(addr)
}

class PrivateKey(val key: PrivateKeyJS) extends AbstractKey {
  /** Pack key to BOSS */

  def pack: Seq[Byte] = key.pack("BOSS")

  def this(bossEncoded: Seq[Byte]) =
    this(new PrivateKeyJS("BOSS", bossEncoded.toJSArray))

  def this(bossEncoded: js.Array[Byte]) = this(bossEncoded.toSeq)

  /** Key size in bits */
  def bits: Int = key.getBitStrength()
  /** Get public key by private */

  def publicKey: PublicKey = new PublicKey(key.publicKey)

  def shortAddress: Seq[Byte] = publicKey.shortAddress
  def longAddress: Seq[Byte] = publicKey.longAddress

  def signExtended(data: Seq[Byte]): Seq[Byte] = {
    ExtendedSignatureJS.sign(key, data.toJSArray).toSeq
  }

  def sign(data: Seq[Byte]): Seq[Byte] = {
    val options = HashMap(
      ("pssHash", new SHA(512)),
      ("mgf1Hash", new SHA(1))
    )

    key.sign(data.toJSArray, options.toJSDictionary).toSeq
  }

  def isMatchingAddress(addr: js.Array[Byte]): Boolean = {
    val encodedAddr = encode64(addr)
    encode64(shortAddress) == encodedAddr || encode64(longAddress) == encodedAddr
  }

  def encrypt(data: Seq[Byte]): Seq[Byte] =
    throw new EncryptionError("encrypt is not available for private key")

  /** Decrypts binary data
    *
    * @param data - data to decrypt
    */
  def decrypt(data: Seq[Byte]): Seq[Byte] = {
    val options = HashMap(
      ("oaepHash", new SHA(1)),
      ("mgf1Hash", new SHA(1))
    )

    key.decrypt(data.toJSArray, options.toJSDictionary).toSeq
  }

  /** Returns key info */
  def info: KeyInfo = {
    KeyInfo(HashMap(
      ("algorithm", "RSA_PRIVATE"),
      ("keyLength", length),
      ("tag", publicKey.fingerprint.slice(1, 5))
    ))
  }

  /** Calculates key's fingerprint */
  def fingerprint: Seq[Byte] = publicKey.fingerprint
}

object PrivateKeyExported {
  /**
   * Generates private key by size and public exponent
   * @param bits - size (2048/4096)
   * @param exp - public exponent (default 0x10001)
   */
  def create(bits: Int, exp: Int = 0x10001): Future[PrivateKey] = {
    val p = Promise[PrivateKey]()

    def onGenerate(err: js.Dynamic, key: PrivateKeyJS): Unit =
      p.success(new PrivateKey(key))

    KeyGenerator.create(bits, exp, onGenerate)

    p.future
  }

  def createForWorker(bits: Int, exp: Int, postMessageFunction: js.Function1[js.Any, Unit]): Unit = {

    def onGenerate(err: js.Dynamic, key: PrivateKeyJS): Unit = {
      if (err == null) {
        val pk = new PrivateKey(key)
        val resultArray = js.Array[Any]("PrivateKeyExported", new PrivateKeyExported(pk).toBase64)
        postMessageFunction(resultArray)
      } else {
        postMessageFunction(err)
      }
    }

    KeyGenerator.create(bits, exp, onGenerate)
  }

  @JSExportStatic
  def fromBOSS(encoded: js.Array[Byte]): PrivateKeyExported = {
    val pk = new PrivateKey(encoded)
    new PrivateKeyExported(pk)
  }

  @JSExportStatic
  def fromBase64(encoded: String): PrivateKeyExported = {
    val pk = new PrivateKey(decode64(encoded))
    new PrivateKeyExported(pk)
  }

  @JSExportStatic("create")
  def createJS(bits: Int, exp: Int): js.Promise[PrivateKeyExported] = {
//    dapi.createPrivateKey(bits, exp)
//      .map(pk => new PrivateKeyExported(pk))
//      .toJSPromise
     create(bits, exp)
       .map(pk => new PrivateKeyExported(pk))
       .toJSPromise
  }

  @JSExportStatic("create")
  def createJS(bits: Int): js.Promise[PrivateKeyExported] = {
//    dapi.createPrivateKey(bits, 0x10001)
//      .map(pk => new PrivateKeyExported(pk))
//      .toJSPromise
     create(bits, 0x10001)
       .map(pk => new PrivateKeyExported(pk))
       .toJSPromise
  }
}


object PrivateKeyImplicits {

  // implicit val rw: ReadWriter[PrivateKey] = readwriter[String].bimap[PrivateKey](
  //   inst => encode64(inst.pack),
  //   json => new PrivateKey(decode64(json))
  // )
}
