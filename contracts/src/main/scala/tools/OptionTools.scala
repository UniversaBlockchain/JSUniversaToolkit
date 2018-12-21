package tools

object OptionTools {

  implicit class OptTools[T](val opt: Option[T]) extends AnyVal {

    @inline
    def ifEmpty(f: => Unit): Unit = if (opt.isEmpty) f

    @inline
    def let(f: T => Unit): Unit = opt match {
      case Some(x) => f(x)
      case _ =>
    }
  }
}


