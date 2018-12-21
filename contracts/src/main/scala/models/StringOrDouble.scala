package models

import boss.{BossCase, BossSerializable}

case class StringOrDoubleJS(value: Option[Any]) extends BossCase

class StringOrDouble(val value: Option[Any]) extends BossSerializable {

  def toDoubleOpt: Option[Double] = value.flatMap(Option(_)).map(_.toString.toDouble)
  def toStringOpt: Option[String] = value.flatMap(Option(_)).map(_.toString)

  def toDouble: Double = toDoubleOpt.get
  override def toString: String = toStringOpt.orNull

  private def pack: Option[Any] = {
    value.map {
      case null => null
      case v => v match {
        case s: String => s
        case _ => toDouble
      }
    }
  }

  def toJS: Any = {
    StringOrDoubleJS(pack)
  }

}

object StringOrDouble {
  def fromJS(bossCase: BossCase): BossSerializable = {
    val sdJS = bossCase.asInstanceOf[StringOrDoubleJS]
    new StringOrDouble(Some(sdJS.value))
  }
}
