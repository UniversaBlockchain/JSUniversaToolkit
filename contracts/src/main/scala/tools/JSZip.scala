package tools

import scala.scalajs.js
import scala.scalajs.js.Promise
import scala.scalajs.js.annotation.JSGlobal


@js.native
@JSGlobal
class ZipObject() extends js.Object {
  def async(`type`: String): Promise[js.Object] = js.native
}

@js.native
@JSGlobal
class JSZip() extends js.Object {
  def folder(name : String) : js.Object = js.native
  //read the file
  def file(name : String) : ZipObject = js.native
  //create the file
  def file(name: String, content: Any, options: js.Dictionary[Any]): JSZip = js.native
  def generateAsync(options : js.Dictionary[Any]) : Promise[js.Object] = js.native
  def loadAsync(data: js.Object, options : js.Dictionary[Any] = null) : Promise[js.Object] = js.native
  def files : js.Object = js.native
}

