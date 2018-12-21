package html

import scala.collection.mutable

trait HtmlBuilder {

  class HtmlTag(val parent: Option[HtmlTag],
                val tagName: String,
                _text: Option[String],
                attributes: Seq[(String, String)]) {

    parent.foreach { par => par.children += this }

    val text = new StringBuilder(_text.getOrElse(""))

    private val children = mutable.Buffer[HtmlTag]()

    def apply[T](f: => T): HtmlTag = {
      inContext(this, f)
      this
    }

    def toHtml: String = {
      val sb = new StringBuilder(s"<$tagName")
      if (attributes.nonEmpty) {
        sb ++= " " + attributes.map { nv => s"${nv._1}=${nv._2}" }.mkString(" ")
      }
      s"$sb>$text${
        children.map {
          _.toHtml
        }.mkString
      }</$tagName>"
    }

  }

  class TagBuilder(name: String) {

    def apply: HtmlTag = {
      putTag(name)
    }

    def apply(text: String): HtmlTag = {
      putTag(name, Some(text))
    }
  }

  private var parent = Option.empty[HtmlTag]

  private def putTag(name: String,
                     text: Option[String] = None,
                     block: Option[() => Unit] = None,
                     attrs: Option[Seq[(String, String)]] = None
                    ): HtmlTag = {
    val lastParent = parent
    try {
      val newTag = new HtmlTag(parent, name, text, attrs.getOrElse(Seq()))
      block.foreach { b => inContext(newTag, b) }
      newTag
    }
    finally {
      parent = lastParent
    }
  }

  protected def inContext[T](tag: HtmlTag, f: => T): T = {
    val currentParent = parent
    parent = Some(tag)
    try {
      f
    }
    finally {
      parent = currentParent
    }
  }

  def createTag(tagName: String) = new TagBuilder(tagName)

  def t(text: String): Unit = parent.foreach {
    _.text ++= text
  }

  val P = new TagBuilder("p")
  val div = new TagBuilder("div")
  val h1 = new TagBuilder("h1")


  implicit class TextContent(text: String) {
    @inline
    def unary_+() = {
      parent.foreach {
        _.text ++= text
      }
      // this is an important hac otherwise compiler does not
      // call it in a proper context
      HtmlBuilder.this
    }
  }

}
