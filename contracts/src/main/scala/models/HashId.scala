package models

import boss._
import tools.universa.UniversaTools._

case class HashIdJS(composite3: Seq[Byte]) extends BossCase

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

  def fromJS(serialized: BossCase): BossSerializable =
    new HashId(serialized.asInstanceOf[HashIdJS])
}
