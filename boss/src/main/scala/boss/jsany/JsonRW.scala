package boss.jsany

import boss.{AbstractReader, AbstractWriter}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

object JSWriter extends AbstractWriter[js.Any] {
  def writeNull(): js.Any = null

  def writeBoolean(b: Boolean): js.Any = b

  def writeNumber(x: Double): js.Any = x

  def writeLong(x: Long): js.Any = x

  def writeString(s: String): js.Any = s

  def writeOption(value: Option[_]): js.Any = {
    if (value.isEmpty) js.undefined
    else value.get.asInstanceOf[js.Any]
  }

  def writeDatetime(d: js.Date): js.Any = d

  def writeArray(elems: js.Any*): js.Any = js.Array(elems: _*)

  def writeObject(fields: (String, js.Any)*): js.Any = {
    val filteredFields = fields.filterNot{case (k,v) =>
//      if (isUndefined(v)) println("Found undefined!!!")
      isUndefined(v)
    }
    js.Dictionary[js.Any](filteredFields:_*)
  }

  def writeType(
    tpe: js.Any,
    obj: js.Any,
    mapping: mutable.HashMap[String, String]
  ): js.Any = {
    var outType = tpe.toString

    if (mapping.keySet.contains(outType)) {
      outType = mapping(outType)
    }

    obj.asInstanceOf[js.Dictionary[js.Any]]("__type") = outType
    obj
  }

  def writeMap(map: Map[String, js.Any]): js.Any = {
    js.Dictionary.empty[js.Any]
  }

  def writeBytes(list: Seq[js.Any]): js.Any = {
    val jsArray = js.Array[Int]()

    for (v <- list.asInstanceOf[Seq[Int]]) jsArray += v

    new Uint8Array(jsArray.asInstanceOf[ArrayBuffer], 0, jsArray.length)
  }
}

object JSReader extends AbstractReader[js.Any] {
  private val Primitives = Set(
    "string",
    "boolean",
    "number"
  )

  def isNull(x: js.Any): Boolean = x eq null
  def isUndefined(x: js.Any): Boolean = x.isInstanceOf[Unit]
  def isPrimitive(x: js.Any): Boolean = Primitives(js.typeOf(x))
  def isArray(x: js.Any): Boolean = {
    js.Array.isArray(x)
  }

  def isBytes(x: js.Any): Boolean = x.isInstanceOf[Uint8Array]

  def isDatetime(x: js.Any): Boolean = x.isInstanceOf[js.Date]

  def readPrimitive(x: js.Any): Any = {
    js.typeOf(x) match {
      case "string" => readString(x)
      case "boolean" => readBoolean(x)
      case "number" => readNumber(x)
      case _ => null
    }
  }

  def readBoolean(x: js.Any): Boolean = x.asInstanceOf[Boolean]

  def readNumber(x: js.Any): Double = x.asInstanceOf[Double]

  def readString(x: js.Any): String = x.asInstanceOf[String]

  def readArrayLength(x: js.Any): Int =
    x.asInstanceOf[js.Array[_]].length

  def readArrayElem(x: js.Any, index: Int): js.Any =
    x.asInstanceOf[js.Array[js.Any]].apply(index)

  def readObjectField(x: js.Any, field: String): js.Any =
    x.asInstanceOf[js.Dynamic].selectDynamic(field)

  def readDatetime(x: js.Any): js.Date =
    x.asInstanceOf[js.Date]

  def readType(
    x: String,
    mapping: mutable.HashMap[String, String]
  ): String = {
    mapping.getOrElse(x, x)
  }

  // def readMap(x: js.Dictionary[js.Any]): mutable.HashMap[String, Any] = {
  //   val map: mutalbe.HashMap.empty[String, Any] = mutable.HashMap.empty[String,Any]

  //   for ((k, v) <- value) {
  //     map += (k -> Boss.write(v))
  //   }
  // }
}
