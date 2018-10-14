package boss


trait Writer[A] {
  type ScalaType = A

  def write[P](obj: ScalaType)(implicit registry: AbstractBoss,
                               builder: AbstractWriter[P]): P
}

object Writer extends BossMacro {
  implicit object BooleanWriter extends Writer[Boolean] {
    def write[P](x: Boolean)(implicit registry: AbstractBoss,
                             builder: AbstractWriter[P]): P = builder.writeBoolean(x)
  }

  implicit object ByteWriter extends Writer[Byte] {
    def write[P](x: Byte)(implicit registry: AbstractBoss,
                          builder: AbstractWriter[P]): P = builder.writeNumber(x)
  }

  implicit object ShortWriter extends Writer[Short] {
    def write[P](x: Short)(implicit registry: AbstractBoss,
                           builder: AbstractWriter[P]): P = builder.writeNumber(x)
  }

  implicit object IntWriter extends Writer[Int] {
    def write[P](x: Int)(implicit registry: AbstractBoss,
                         builder: AbstractWriter[P]): P = builder.writeNumber(x)
  }

  implicit object LongWriter extends Writer[Long] {
    def write[P](x: Long)(implicit registry: AbstractBoss,
                          builder: AbstractWriter[P]): P = builder.writeLong(x)
  }

  implicit object FloatWriter extends Writer[Float] {
    def write[P](x: Float)(implicit registry: AbstractBoss,
                           builder: AbstractWriter[P]): P = builder.writeNumber(x)
  }

  implicit object DoubleWriter extends Writer[Double] {
    def write[P](x: Double)(implicit registry: AbstractBoss,
                            builder: AbstractWriter[P]): P = builder.writeNumber(x)
  }

  implicit object StringWriter extends Writer[String] {
    def write[P](x: String)(implicit registry: AbstractBoss,
                            builder: AbstractWriter[P]): P = builder.writeString(x)
  }
}
