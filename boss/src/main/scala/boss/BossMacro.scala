package boss

import scala.language.experimental.macros

import scala.reflect.macros._


object BossMacroImpl {
  def createWriter[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Writer[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass

    if (!sym.isCaseClass) {
      c.error(c.enclosingPosition, "Cannot create writer for non-case class")

      return c.Expr[Writer[T]](q"null")
    }

    val accessors = (tpe.decls collect {
      case acc: MethodSymbol if acc.isCaseAccessor => acc
    }).toList

    val fields = for { accessor <- accessors } yield {
      val fieldName = accessor.name.toTermName
      val fieldNameStr = fieldName.toString()

      q"""
        ($fieldNameStr, registry.write(value.$fieldName))
      """
    }

    val writeLogic = q"""
      builder.writeObject(..$fields)
    """

    val result = q"""
      implicit object AutoWriter extends boss.Writer[$tpe] {
        import boss._

        override def write[P](value: $tpe)(
          implicit registry: AbstractBoss,
          builder: AbstractWriter[P]
        ): P = $writeLogic
      }
      AutoWriter
    """

    c.Expr[Writer[T]](result)
  }

  def createReader[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Reader[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass
    val mainClassName = tpe.typeSymbol.name.toString

    if (!sym.isCaseClass) {
      c.error(c.enclosingPosition, "Cannot create writer for non-case class")

      return c.Expr[Reader[T]](q"null")
    }

    val accessors = (tpe.decls collect {
      case acc: MethodSymbol if acc.isCaseAccessor => acc
    }).toList

    val fields = for {
      accessor <- accessors
    } yield {
      val fieldName = accessor.name
      val fieldString = fieldName.toString
      val fieldTpe = accessor.returnType
      val isOptional = fieldTpe.typeSymbol.name.toString == "Option"

      if (fieldTpe.toString == "Seq[Byte]") {
        q"""
          readBytesSafe(registry.read(
            reader.readObjectField(write, $fieldString), $fieldString, $mainClassName, $isOptional
          )).asInstanceOf[$fieldTpe]
        """
      } else {
        q"""
          registry.read(
            reader.readObjectField(write, $fieldString), $fieldString, $mainClassName, $isOptional
          ).asInstanceOf[$fieldTpe]
        """
      }

    }

    val readLogic = q"""
      new $tpe(..$fields)
    """

    val result = q"""
      implicit object AutoReader extends boss.Reader[$tpe] {
        import boss._
        import tools.universa.ImplicitConverters.readBytesSafe

        override def read[P](write: P)(
          implicit registry: AbstractBoss,
          reader: AbstractReader[P]
        ): $tpe = $readLogic
      }
      AutoReader
    """

    c.Expr[Reader[T]](result)
  }
}

trait BossMacro {
  implicit def createWriter[T]: Writer[T] = macro BossMacroImpl.createWriter[T]
  implicit def createReader[T]: Reader[T] = macro BossMacroImpl.createReader[T]
}
