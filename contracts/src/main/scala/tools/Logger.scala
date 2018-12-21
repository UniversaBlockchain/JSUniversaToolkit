package tools.universa

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal}
import scala.concurrent.{Future, Promise}

// import io.github.shogowada.scala.jsonrpc.{
//   DisposableFunction0, DisposableFunction1
// }

import tools.{
  TimeUtils,
  UnicodeUtils,
  ProcessUtils
}


trait LoggerAPI {
  def log(message: String): Unit

  def info(message: String): Unit
  def event(message: String): Unit
  def error(message: String): Unit
  def recv(message: String): Unit
  def send(message: String): Unit
  def enter(message: String): Unit
  def leave(message: String): Unit
}


class LoggerAPIBase extends LoggerAPI {
  override def log(message: String): Unit = this.synchronized {
    throw new UnsupportedOperationException("Not implemented.")
  }

  override def info(message: String): Unit = {
    log(s"${UnicodeUtils.INFO} $message")
  }

  override def event(message: String): Unit =
    log(s"${UnicodeUtils.EVENT} $message")

  override def error(message: String): Unit =
    log(s"${UnicodeUtils.FAIL} $message")

  override def recv(message: String): Unit =
    log(s"${UnicodeUtils.RECV} $message")

  override def send(message: String): Unit =
    log(s"${UnicodeUtils.SEND} $message")

  override def enter(message: String): Unit =
    log(s"${UnicodeUtils.ENTER} $message")

  override def leave(message: String): Unit =
    log(s"${UnicodeUtils.LEAVE} $message")
}

class LoggerAPIImpl extends LoggerAPIBase {
  override def log(message: String): Unit = this.synchronized {
    println(s"${TimeUtils.ts}: ${UnicodeUtils.Sun} [${ProcessUtils.name}] $message")
  }
}


object logger extends LoggerAPIBase {

  var local =  new LoggerAPIImpl()

  private var impl: LoggerAPI = new LoggerAPIImpl()

  def setImpl(newLogger: LoggerAPI): Unit = {
    impl = newLogger
  }

  override def log(message: String): Unit = this.synchronized {
    impl.log(message)
  }
}


object LoggerJS {
  @JSExportTopLevel("Universa.logger.log")
  def log(message: String): Unit = logger.log(message)
}
