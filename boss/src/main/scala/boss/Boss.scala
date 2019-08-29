package boss

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Try

abstract class BossSerializable {
  def toJS: Any
}

abstract class BossCase {}

trait AbstractBoss {
  def write[P](value: Any)(
    implicit builder: AbstractWriter[P],
    registry: AbstractBoss = this
  ): P

  def read[P](write: P, field: String = null, mainClassName: String = null, isOptional: Boolean = false)(
    implicit reader: AbstractReader[P],
    registry: AbstractBoss = this
  ): Any
}

/**
  * Represents facade for universajs Boss class
  */
@js.native
@JSGlobal("Universa.boss")
class BossJS extends js.Object {
  /**
    * Instantiates Formatter class and returns encoded byte sequence
    */
  def dump(items: Any*): js.Array[Byte] = js.native
  /**
    * Instantiates Parser class and returns decoded data
    */
  def load(data: js.Array[Byte]): js.Any = js.native
}

/**
  * Represents facade for universajs BossReader class
  */
@js.native
@JSGlobal("Universa.boss.reader")
class BossReaderJS extends js.Object {
  /**
    * Constructs reader instance
    */
  def this(stream: js.Array[Byte] = js.native) = this()
  /**
    * Returns next part of data from sequence
    */
  def read(): js.Any = js.native
}

/**
  * Represents facade for universajs BossWriter class
  */
@js.native
@JSGlobal("Universa.boss.writer")
class BossWriterJS extends js.Object {
  /**
    * Writes another part of data to sequence
    */
  def write(data: Any): Unit = js.native
  /**
    * Returns result encoded sequence of bytes
    */
  def get(): js.Array[Byte] = js.native
}

class BossReader(source: Seq[Byte]) {
  val jsReader = new BossReaderJS(source.toJSArray)

  def read(): Any = {
    var result = jsReader.read()
    if (result.isInstanceOf[Uint8Array]) {
      result.asInstanceOf[js.Array[Byte]].toSeq
    } else result
  }
}

class BossWriter {
  val jsWriter = new BossWriterJS()

  def write[P](data: Any)(
    implicit builder: AbstractWriter[P]
  ): Unit = {
    val encoded = Boss.write[P](data)
    jsWriter.write(encoded)
  }

  def get(): Seq[Byte] = {
    jsWriter.get().toSeq
  }
}

object Boss extends AbstractBoss {
  def dump[P](value: Any)(
    implicit builder: AbstractWriter[P]
  ): Seq[Byte] = {
    val boss = new BossJS()
    val converted = write[P](value)

    boss.dump(converted)
  }

  def load(encoded: Seq[Byte])(
    implicit reader: AbstractReader[js.Any]
  ): Any = {
    val boss = new BossJS()
//    Try{boss.load(encoded.toJSArray)}.recover{case e: Throwable => println(UniversaToolsJS.asByteString(encoded.toJSArray))}
    val decoded = boss.load(encoded.toJSArray)
    read(decoded)
  }

  private val Primitives = Set(
    "java.lang.Boolean",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Byte",
    "java.lang.Short",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.String"
  )

  private val writers = new mutable.HashMap[String, Writer[_]]
  private val readers = new mutable.HashMap[String, Reader[_]]
  private val aliasToInternal = new mutable.HashMap[String, String]
  private val internalToAlias = new mutable.HashMap[String, String]
  private val constructors = new mutable.HashMap[
    String,
    BossCase => BossSerializable
    ]

  registerPrimitives()

  private def registerInternal(
                                clazz: Class[_], writer: Writer[_], reader: Reader[_]
                              ): Unit = {
    writers(clazz.getName) = writer
    readers(clazz.getName) = reader
  }

  def register[A : ClassTag](implicit writer: Writer[A], reader: Reader[A]): Unit = {
    val className = implicitly[ClassTag[A]].runtimeClass
    registerInternal(className, writer, reader)
  }

  def registerAlias(internal: String, alias: String): Unit = {
    // register class name with pattern "com.icodici.---.className"
    aliasToInternal(alias) = internal

    val shortName = alias.substring(alias.lastIndexOf(".") + 1)
    // register class name with pattern "className"
    aliasToInternal(shortName) = internal

    internalToAlias(internal) = alias
  }

  def registerClass[JSC: ClassTag](
                                    constructor: BossCase => BossSerializable,
                                    alias: String)(
                                    implicit writer: Writer[JSC],
                                    reader: Reader[JSC]
                                  ): Unit = {
    register[JSC]
    val jsClassName = implicitly[ClassTag[JSC]].runtimeClass.getName
    registerAlias(jsClassName, alias)
    constructors(jsClassName) = constructor
  }

  def getClassName(value: Any): String = {
    if (value.isInstanceOf[js.Date]) return "Date"

    value.getClass.getName match {
      case "java.lang.Byte" | "java.lang.Short"   => "java.lang.Integer"
      case "java.lang.Float"                      => "java.lang.Double"
      case "scala.collection.mutable.HashMap"     => "HashMap"
      case "scala.collection.mutable.ListBuffer"  => "Array"
      case "scala.collection.mutable.ArrayBuffer" => "Bytes"
      case "scala.scalajs.js.WrappedArray"        => "Bytes"
      case "scala.None$" | "scala.Some"           => "Option"
      case name                                   => name
    }
  }

  def write[P](value: Any)(
    implicit builder: AbstractWriter[P],
    registry: AbstractBoss
  ): P = {
    if (value == null) return builder.writeNull()

    getClassName(value) match {
      case "HashMap" =>
        val fields = value.asInstanceOf[mutable.HashMap[String, P]]
          .mapValues(v => Boss.write(v))
        builder.writeObject(fields.toList:_*)
      case "Bytes" => builder.writeBytes(value.asInstanceOf[Seq[P]])
      case "Date" => builder.writeDatetime(value.asInstanceOf[js.Date])
      case "Array" =>
        val fields = value.asInstanceOf[mutable.ListBuffer[P]]
          .map(v => Boss.write(v))
        builder.writeArray(fields:_*)
      case "Option" =>
        builder.writeOption(value.asInstanceOf[Option[P]])
      case className =>
        if (!writers.keySet.contains(className)) {
          Boss.write(value.asInstanceOf[BossSerializable].toJS)
        } else {
          val writer = writers(className)
          val packed = writer.write[P](value.asInstanceOf[writer.ScalaType])

          if (Primitives.contains(className)) {
            packed
          } else {
            builder.writeType(builder.writeString(className), packed, internalToAlias)
          }
        }
    }
  }

  def read[P](value: P,
              fieldName: String = null,
              mainClassName: String = null,
              isOptionalValue: Boolean = false
             )(
    implicit reader: AbstractReader[P],
    registry: AbstractBoss
  ): Any = {
    val v = readInternal(value)

    if (isOptionalValue) Option(v)
    else v
  }

  def readInternal[P](value: P,
              fieldName: String = null,
              mainClassName: String = null,
              isOption: Boolean = false
             )(
    implicit reader: AbstractReader[P],
    registry: AbstractBoss
  ): Any = {
    if (reader.isNull(value)) return null
    if (reader.isUndefined(value)) return null

    var typeField = reader.readObjectField(value, "__type")
    if (reader.isUndefined(typeField)) typeField = reader.readObjectField(value, "__t")

    def readObject(data: P): mutable.HashMap[String, Any] = {
      val map = data.asInstanceOf[js.Dictionary[P]].map{
        case (k, v) => (k, Boss.read(v))
      }
      mutable.HashMap[String, Any](map.toSeq: _*)
    }

    if (reader.isUndefined(typeField)) {

      if (reader.isPrimitive(value)) {
        return reader.readPrimitive(value)
      }

      if (reader.isBytes(value)) {
        val mutSeq: mutable.Seq[Byte] = value.asInstanceOf[js.Array[Byte]]

        return mutSeq
      }

      if (reader.isDatetime(value)) {
        return reader.readDatetime(value)
      }

      if (reader.isArray(value)) {
        val list = mutable.ListBuffer[Any]()
        value.asInstanceOf[js.Array[P]].map(v => Boss.read(v)).copyToBuffer(list)
        return list
      }

      readObject(value)
    } else {
      val typeFieldString = reader.readString(typeField)
      val shortType = typeFieldString.substring(typeFieldString.lastIndexOf(".") + 1)
      val className = reader.readType(shortType, aliasToInternal)
      val mayBeClassReader = readers.get(className)

      mayBeClassReader match {
        case Some(classReader) => {
          val jsVal = classReader.read[P](value)
          if (!constructors.keySet.contains(className)) return jsVal

          val constructor = constructors(className)
          constructor(jsVal.asInstanceOf[BossCase])
        }
        case None => readObject(value)
      }
    }
  }

  private def registerPrimitive[P : ClassTag, W : ClassTag](
                                                             implicit writer: Writer[P],
                                                             reader: Reader[P]
                                                           ): Unit = {
    register[P]
    registerInternal(implicitly[ClassTag[W]].runtimeClass, writer, reader)
  }

  private def registerPrimitives(): Unit = {
    registerPrimitive[Boolean, java.lang.Boolean]
    registerPrimitive[Int, java.lang.Integer]
    registerPrimitive[Long, java.lang.Long]
    registerPrimitive[Byte, java.lang.Byte]
    registerPrimitive[Short, java.lang.Short]
    registerPrimitive[Float, java.lang.Float]
    registerPrimitive[Double, java.lang.Double]

    register[String]
  }
}
