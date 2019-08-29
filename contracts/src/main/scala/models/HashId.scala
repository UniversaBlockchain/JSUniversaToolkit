package models

import boss._

import tools.universa.UniversaTools._

import scala.scalajs.js

import js.JSConverters._
import js.annotation.{ JSExport, JSExportStatic, JSExportTopLevel }

case class HashIdJS(composite3: Seq[Byte]) extends BossCase

@JSExportTopLevel("Universa.HashId")
class HashIdExported(val hashId: HashId) extends js.Object {
  def base64: String = hashId.base64
  def bytes: js.Array[Byte] = hashId.composite3.toJSArray
}

object HashIdExported {
  @JSExportStatic("create")
  def apply(data: js.Array[Byte]): HashIdExported = {
    new HashIdExported(HashId(data.toSeq))
  }
}

class HashId private(val composite3: Seq[Byte]) extends BossSerializable {
  def this(loaded: HashIdJS) = this(loaded.composite3)

  def toJS: HashIdJS = HashIdJS(composite3)

  def equals(otherId: HashId): Boolean =
    composite3 == otherId.composite3

  def base64: String = encode64(composite3)
}

object HashId {
  def apply(data: Seq[Byte]): HashId = new HashId(getHashId(data))

  def fromBytes(id: Seq[Byte]): HashId = new HashId(id)
  def fromBase64(id: String): HashId = fromBytes(decode64(id))

  def fromJS(serialized: BossCase): BossSerializable =
    new HashId(serialized.asInstanceOf[HashIdJS])
}
