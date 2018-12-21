package files

import boss.{BossCase, BossSerializable}
import models.HashId

import scala.scalajs.js

//scalajs-dom was used here for json serialisation. There are other libraries like uPickle or Circe,
//don't know if they are better or quicker.
//check examples here http://www.g-widgets.com/2018/05/08/working-with-json-in-scala-js/
class FileMetaInfo extends js.Object {
  var fileName: String = _
  var hashId: js.Array[Byte] = _
  var description: String = _
  var __type: String = "file"
}

class ArchiveMetaInfo extends js.Object {
  var fileInfos: js.Array[FileMetaInfo] = _
  var itemIds: js.Array[Double] = _
}

class FileBoss(
                   val fileName: String,
                   val hashId: HashId,
                   fileDescription: String
                   ) extends BossSerializable {
  override def toJS: Any = FileBossJS(fileName, hashId, fileDescription)
}

object FileBoss {
  def apply(fileInfo: FileInfo): FileBoss = {
    new FileBoss(fileInfo.name, fileInfo.hashId, fileInfo.description)
  }

  def fromJS(bossCase: BossCase): BossSerializable = {
    val fileJS = bossCase.asInstanceOf[FileBossJS]
    new FileBoss(fileJS.fileName, fileJS.hashId, fileJS.fileDescription)
  }
}

case class FileBossJS(fileName: String,
                      hashId: HashId,
                      fileDescription: String
                     ) extends BossCase