package boss

trait Reader[A] {
  def read[P](dump: P)(implicit registry: AbstractBoss,
                       reader: AbstractReader[P]): A
}

object Reader extends BossMacro {
  implicit object BooleanReader extends Reader[Boolean] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Boolean = reader.readBoolean(dump)
  }

  implicit object ByteReader extends Reader[Byte] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Byte = reader.readNumber(dump).toByte
  }

  implicit object ShortReader extends Reader[Short] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Short = reader.readNumber(dump).toShort
  }

  implicit object IntReader extends Reader[Int] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Int = reader.readNumber(dump).toInt
  }

  implicit object LongReader extends Reader[Long] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Long = reader.readString(dump).toLong
  }

  implicit object FloatReader extends Reader[Float] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Float = reader.readNumber(dump).toFloat
  }

  implicit object DoubleReader extends Reader[Double] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): Double = reader.readNumber(dump).toDouble
  }

  implicit object StringReader extends Reader[String] {
    def read[P](dump: P)(implicit registry: AbstractBoss,
                         reader: AbstractReader[P]): String = reader.readString(dump)
  }
}
