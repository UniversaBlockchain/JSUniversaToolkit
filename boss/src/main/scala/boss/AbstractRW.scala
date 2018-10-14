package boss

import scala.scalajs.js
import scala.collection.mutable

trait AbstractWriter[P] {
  def writeNull(): P
  def writeBoolean(b: Boolean): P
  def writeNumber(x: Double): P
  def writeLong(x: Long): P
  def writeString(s: String): P
  def writeOption(s: Option[_]): P
  def writeArray(elems: P*): P
  def writeObject(fields: (String, P)*): P
  def writeMap(map: Map[String, P]): P
  def writeBytes(list: Seq[P]): P
  def writeDatetime(x: js.Date): P
  def writeType(t: P, obj: P, mapping: mutable.HashMap[String, String]): P
}

trait AbstractReader[P] {
  def isUndefined(x: P): Boolean
  def isNull(x: P): Boolean
  def isPrimitive(x: P): Boolean
  def isArray(x: P): Boolean
  def isBytes(x: P): Boolean
  def isDatetime(x: P): Boolean
  def readPrimitive(x: P): Any
  def readBoolean(x: P): Boolean
  def readNumber(x: P): Double
  def readString(x: P): String
  def readArrayLength(x: P): Int
  def readArrayElem(x: P, index: Int): P
  def readObjectField(x: P, field: String): P
  def readDatetime(x: P): P
  def readType(t: String, mapping: mutable.HashMap[String, String]): String
}
