package workers

import boss.Boss
import boss.jsany._

import cloud.{ Api, CryptoCloud }

import models.PrivateKeyExported

import files.{ ArchiveExp, Archive }

import org.scalajs.dom
import org.scalajs.dom.raw.{ Blob, BlobPropertyBag, URL }
import org.scalajs.dom.webworkers.Worker

import tools.universa.UniversaTools
import tools.universa.UniversaTools.{ uuid, decode64, ensureBytes }

import scala.collection.mutable
import scala.scalajs.js
import js.JSConverters._
import js.annotation.{ JSExportTopLevel, JSGlobalScope }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@js.native
@JSGlobalScope
object WorkerGlobal extends js.Object {
  def addEventListener(`type`: String, f: js.Function): Unit = js.native
  def postMessage(data: js.Any): Unit = js.native
}

/**
 * Defines workers API
 */
object WorkerApi {
  type jsDict = js.Dictionary[js.Any]

  private val workers: mutable.Map[String, Worker] = mutable.Map.empty

  def callbackWithMeta(meta: jsDict) = (data: js.Any) => {
    WorkerGlobal.postMessage(js.Dictionary(("result", data), ("meta", meta)))
  }

  /**
   * Creates plain worker body script
  **/
  def getWorkerScript(params: js.Dictionary[String]): String = {
    val scripts = params("assets").asInstanceOf[js.Array[String]]
    val imports = scripts.map(asset =>
      "importScripts(\"" + asset + "\");"
    ).mkString("\n")

    s"""
      |self.onmessage = function(e) {
      |  if (e.data == 'urls') {
      |    $imports
      |  }
      |  else if (e.data == 'init') { Universa.GlobalWorker.init(); }
      |};
    """.stripMargin
  }

  @JSExportTopLevel("Universa.Worker3.init")
  def initWorker(
    params: js.Dictionary[String],
    callback: js.Function3[Any, Any, Any, Any]
  ): String = {
    val script = getWorkerScript(params)

    val url = URL.createObjectURL(new Blob(
      js.Array(getWorkerScript(params)),
      BlobPropertyBag(Option("text/javascript").orUndefined)
    ))

    val worker = new Worker(url)
    worker.postMessage("urls")
    worker.postMessage("init")

    worker.onmessage = (e: js.Any) => {
      val msg = e.asInstanceOf[dom.MessageEvent].data.asInstanceOf[jsDict]
      val res = msg("result").asInstanceOf[js.Any]
      val meta = msg("meta")

      if (!js.Array.isArray(res)) {
        callback(res, null, meta)
      } else {
        val msgArray = res.asInstanceOf[js.Array[_]]
        val resultType = msgArray(0)
        val result = msgArray(1)

        def cb(result: Any): Any = callback(null, result, meta)

        resultType match {
          case "Double" => cb(result)

          case "Bytes" => {
            cb(ensureBytes(result.asInstanceOf[js.Array[Byte]]).toJSArray)
          }

          case "Archive" => {
            cb(ArchiveExp.unpack(result.asInstanceOf[js.Dictionary[js.Any]]))
          }

          case "PrivateKeyExported" =>
            cb(PrivateKeyExported.fromBase64(result.toString))

          case "Items" =>
            val itemsEncoded = result.asInstanceOf[js.Array[String]]
            val items = itemsEncoded
              .toSeq
              .map(decode64)
              .map(v => Boss.load(v).asInstanceOf[mutable.HashMap[String, Any]])
            //how to check it before we start to load items?
            val api = params("api").asInstanceOf[Api]
            val makers = items.map(item => api.makeItem(item))
            val seq = Future.sequence(makers).map(_.flatten.toJSArray)
            cb(seq.toJSPromise)

          case _ => println(s"Unknown result type: ${`resultType`}")
        }
      }
    }

    val value = uuid
    workers.put(value, worker)
    value
  }

  @JSExportTopLevel("Universa.Worker3.run")
  def run(name: String, ctx: jsDict): Unit = run(name, ctx, js.Dictionary())

  @JSExportTopLevel("Universa.Worker3.run")
  def run(name: String, ctx: jsDict, meta: jsDict): Unit = {
    val workerOpt = workers.get(name)

    if (workerOpt.isEmpty)
      throw new RuntimeException(s"Could not find worker '$name'")

    val msg = js.Dictionary(("ctx", ctx), ("meta", meta))

    workerOpt.foreach { w => w.postMessage(msg) }
  }

  @JSExportTopLevel("Universa.Worker3.destroy")
  def destroyWorker(name: String): Unit = {
    workers.get(name).foreach { w =>
      w.terminate()
      workers.remove(name)
    }
  }
}

object WorkerGlobalImpl {
  @JSExportTopLevel("Universa.GlobalWorker.init")
  def main(): Unit =
    WorkerGlobal.addEventListener("message", onMessage _ )

  def onMessage(msg: dom.MessageEvent) = {
    val msgBody = msg.data.asInstanceOf[js.Dictionary[_]]
    val ctx = msgBody("ctx").asInstanceOf[js.Dictionary[_]]
    val meta = msgBody("meta").asInstanceOf[js.Dictionary[js.Any]]
    val callback = WorkerApi.callbackWithMeta(meta)

    ctx("command") match {
      case "generatePrivateKey" =>
        println(s"Worker received task: generate PK")
        val bits = ctx("bits").asInstanceOf[Int]
        val exp = ctx("exp").asInstanceOf[Int]
        PrivateKeyExported.createForWorker(bits, exp, callback)

      case "loadItems2" =>
        println(s"Worker received task: load items")
        val serializedApi = ctx("serializedApi").asInstanceOf[js.Dictionary[Any]]
        CryptoCloud.loadItemsForWorker(serializedApi("session_token").asInstanceOf[String], "comm", callback)

      case "loadItems" =>
        println(s"Worker received task: load items")
        val serializedApi = ctx("serializedApi").asInstanceOf[js.Dictionary[Any]]
        val options = ctx("options").asInstanceOf[js.Dictionary[Any]]
        CryptoCloud.loadItemsWorker(serializedApi, options, callback)

      case "uploadArchive" =>
        println("Worker received task: upload archive")
        val serializedApi = ctx("serializedApi").asInstanceOf[js.Dictionary[Any]]
        val files = ctx("files").asInstanceOf[js.Array[Any]]
        ArchiveExp.uploadWorker(serializedApi, files, callback)

      case "downloadArchive" =>
        println("Worker received task: download archive")
        val serializedApi = ctx("serializedApi").asInstanceOf[js.Dictionary[Any]]
        val headItemId = ctx("headItemId").asInstanceOf[Double]
        ArchiveExp.downloadWorker(serializedApi, headItemId, callback)

      case "downloadZip" =>
        println("Worker received task: download zip")
        val serializedApi = ctx("serializedApi").asInstanceOf[js.Dictionary[Any]]
        val headItemId = ctx("headItemId").asInstanceOf[Double]
        ArchiveExp.downloadZipWorker(serializedApi, headItemId, callback)

      case smth =>
        println(s"Worker don't know task: $smth")
    }
  }
}
