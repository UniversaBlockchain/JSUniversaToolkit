package tools.universa

import boss._
import boss.jsany._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.Dictionary
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal}
import scala.util.{Failure, Success}


@js.native
@JSGlobal("Universa.hash.SHA")
class SHA(val shaType: Any) extends js.Object {
  def get(data: js.Any): js.Array[Byte] = js.native
}

@js.native
@JSGlobal("Universa.hash.HMAC")
class HMACJS(val hash: SHA, key: js.Any) extends js.Object {
  def get(data: js.Any): js.Array[Byte] = js.native
}

class HMAC(val jsHMAC: HMACJS) {

  def this(hash: SHA, key: Seq[Byte]) = {
    this(new HMACJS(hash, key.toJSArray))
  }

  def this(hash: SHA, key: String) = {
   this(new HMACJS(hash, key))
  }

  def get(data: Seq[Byte]): Seq[Byte] = {
    jsHMAC.get(data.toJSArray).toSeq
  }

  def get(data: String): Seq[Byte] = {
    jsHMAC.get(data).toSeq
  }
}

@js.native
@JSGlobal("Universa.cipher.AESCTRTransformer")
class AESCTRTransformerJS(val key: js.Array[Byte], val iv: js.Array[Byte]) extends js.Object {
  def transform(data: js.Array[Byte]): js.Array[Byte] = js.native
}

class AESCTRTransformer(val key: Seq[Byte], iv: Seq[Byte]) {
  var jsTransformer = new AESCTRTransformerJS(key.toJSArray, iv.toJSArray)

  def transform(data: Seq[Byte]): Seq[Byte] = {
    jsTransformer.transform(data.toJSArray).toSeq
  }
}

@js.native
@JSGlobal("Universa.pki")
object PKI extends js.Object {
  def pbkdf2(hash: SHA, options: js.Dictionary[Any]): js.Array[Byte] = js.native
}


@js.native
@JSGlobal("Universa.utils.v2")
object UniversaToolsJS extends js.Object {
	def encode64(bytes: js.Array[Byte]): String = js.native
	def decode64(data: String): js.Array[Byte] = js.native
  def encode58(bytes: js.Array[Byte]): String = js.native
  def decode58(data: String): js.Array[Byte] = js.native

	def shortId(): String = js.native
  def hashId(data: js.Array[Byte]): js.Array[Byte] = js.native
  def uuid(): String = js.native

  def asByteString(data: js.Array[Byte]): String = js.native
  def asByteArray(data: String): js.Array[Byte] = js.native
  def crc32(data: String): js.Array[Byte] = js.native
  def randomBytes(size: Int): js.Array[Byte] = js.native
  def textToBytes(text: String): js.Array[Byte] = js.native
}

object UniversaTools {
  @JSExportTopLevel("Universa.tools.encode64")
  def encode64JS(bytes: js.Array[Byte]): String = encode64(bytes.toSeq)
  def encode64(bytes: Seq[Byte]): String = UniversaToolsJS.encode64(bytes.toJSArray)
  @JSExportTopLevel("Universa.tools.decode64")
  def decode64JS(data: String): js.Array[Byte] = UniversaToolsJS.decode64(data)
  def decode64(data: String): Seq[Byte] = UniversaToolsJS.decode64(data).toSeq
  def decode64Seq(data: String): mutable.Seq[Byte] = UniversaToolsJS.decode64(data)

  @JSExportTopLevel("Universa.tools.encode58")
  def encode58JS(bytes: js.Array[Byte]): String = encode58(bytes.toSeq)
  def encode58(bytes: Seq[Byte]): String = UniversaToolsJS.encode58(bytes.toJSArray)
  @JSExportTopLevel("Universa.tools.decode58")
  def decode58(data: String): js.Array[Byte] = UniversaToolsJS.decode58(data)
  def decode58JS(data: String): Seq[Byte] = decode58(data).toSeq

  def shortId: String = UniversaToolsJS.shortId()
  def uuid: String = UniversaToolsJS.uuid()
  def getHashId(data: Seq[Byte]): Seq[Byte] = UniversaToolsJS.hashId(data.toJSArray).toSeq

  def asByteString(data: Seq[Byte]): String = UniversaToolsJS.asByteString(data.toJSArray)
  @JSExportTopLevel("Universa.tools.asByteString")
  def asByteString(data: js.Array[Byte]): String = UniversaToolsJS.asByteString(data)
  def asByteSeq(data: String): Seq[Byte] = UniversaToolsJS.asByteArray(data).toSeq

  def crc32(data: String): Seq[Byte] = UniversaToolsJS.crc32(data).toSeq

  def pbkdf2(hash: SHA, options: HashMap[String, Any]): Seq[Byte] = {
    PKI.pbkdf2(hash, options.toJSDictionary).toSeq
  }

  def randomBytes(size: Int): Seq[Byte] = UniversaToolsJS.randomBytes(size).toSeq

  def textToBytes(text: String): Seq[Byte] = UniversaToolsJS.textToBytes(text).toSeq

  def sleep(milliseconds: Int): Future[Unit] = {
    val p = Promise[Unit]()

    js.timers.setTimeout(milliseconds) { p.success(()) }

    p.future
  }

  def randomInRange(min: Int, max: Int): Int = {
    val randomDouble = js.Math.random
    val scaled = randomDouble * (max - min + 1) + min
    js.Math.floor(scaled).toInt
  }

  def millisNow: Double = (new js.Date).getTime
  //we need this as they removed DateTimeFormatter from scala.js
  def dateTimeStringNow = {
    val now = new js.Date
    val nowString =
      now.toLocaleDateString().split("/").reverse.mkString +
        now.toLocaleTimeString().replaceAll(":", "")
    nowString
  }


  type mutableMap = mutable.HashMap[String, Any]

  def get(ctx: mutableMap, path: String): Any = {
    val steps = path.split("\\.")
    var prevResult: mutableMap = ctx

    steps.tail.foreach {stepp =>
      prevResult = prevResult(stepp).asInstanceOf[mutableMap]
    }
    prevResult(steps.last)
  }

  def set(ctx: mutableMap, path: String, value: Any): Unit = {
    val steps = path.split("\\.")
    var prevResult: mutableMap = ctx

    steps.tail.foreach {stepp =>
      val res = prevResult.get(stepp).map(_.asInstanceOf[mutableMap])
      prevResult = res.getOrElse{
        val map = new mutableMap
        prevResult put (stepp, map)
        map
      }
    }
    prevResult put (steps.last, value)
  }

  def toAny(value: js.Any): Any = Boss.read[js.Any](value)

  // FIXME: Somewhere there is array in js instead of Uint8Array
  def ensureBytes(value: Seq[Byte]): Seq[Byte] = decode64(encode64(value))
}

object ImplicitConverters {
  // FIXME: remove this method after V1 registries update
  def readBytesSafe(value: Any): Seq[Byte] = {
    try { value.asInstanceOf[Seq[Byte]] }
    catch { case _: Throwable => UniversaTools.asByteSeq(value.asInstanceOf[String]) }
  }

  def readIntSafe(value: Any): Int = {
    value match {
      case intValue: Int => intValue
      case someValue => someValue.toString.toInt
    }
  }

  implicit def toJSPromiseDict(f: Future[HashMap[String, Any]]): js.Promise[js.Dictionary[Any]] = {
    val p = Promise[js.Dictionary[Any]]()

    f onComplete {
      case Success(value: HashMap[String, Any]) => {
        p.success(hashMapAsDictionary(value))
        value
      }

      case Failure(err) => p.failure(err)
    }

    p.future.toJSPromise
  }

  def hashMapAsDictionary(value: mutable.HashMap[String, Any]): Dictionary[Any] = {
    Boss.write[js.Any](value).asInstanceOf[Dictionary[Any]]
  }

  implicit def toJSPromiseBool(f: Future[Boolean]): js.Promise[Boolean] = f.toJSPromise
  implicit def toJSPromiseAny(f: Future[Any]): js.Promise[Any] = f.toJSPromise

  implicit def jsAsHashMap(dict: js.Dictionary[Any]): HashMap[String, Any] = {
    Boss.read[js.Any](dict).asInstanceOf[HashMap[String, Any]]
  }

}
