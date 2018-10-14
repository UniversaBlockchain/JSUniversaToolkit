package boss

package object jsany {
  implicit val builder: JSWriter.type = JSWriter
  implicit val reader: JSReader.type = JSReader
}
