package tools.universa

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

// import io.github.shogowada.scala.jsonrpc.{
//   DisposableFunction0,
//   DisposableFunction1
// }

import tools.{
  TimeUtils,
  UnicodeUtils,
  ProcessUtils
}

class WorkerLoggerAPIImpl extends LoggerAPIBase {

  override def log(message: String): Unit = this.synchronized {
    println(s"${TimeUtils.ts}: ${UnicodeUtils.Cloud} [${ProcessUtils.name}]$message")
    Future()
  }
}
