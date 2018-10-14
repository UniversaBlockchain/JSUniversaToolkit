package universa

import boss.Boss
import models._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object CryptoCloudStarter {

  def main(args: Array[String]): Unit = {
    if (!js.isUndefined(dom.window)) {

      dom.window.onload = { _ =>
        registerBasicModels()
      }
    }
  }

  @JSExportTopLevel("Universa.registerBasicModels")
  def registerBasicModels(): Unit = {
    Boss.registerClass[PublicKeyJS](PublicKeyExported.fromJS, "RSAPublicKey")
    Boss.registerClass[KeyRecordJS](KeyRecord.fromJS, "KeyRecord")
    Boss.registerClass[KeyAddressJS](KeyAddress.fromJS, "KeyAddress")
  }
}
