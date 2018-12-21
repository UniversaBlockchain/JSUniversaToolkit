package tools

import java.util.UUID

object ProcessUtils {

  private var processUUID = "new"
  private var processName = "????"

  def uuid: String = processUUID

  def setUUID(newId: String): Unit = {
    processUUID = newId
    processName = newId.substring(0, 4)
  }

  def name: String = processName

  def setName(newName: String): Unit = {
    processName = newName
  }

  def setIndex(newIndex: Int): Unit = {
    processName = "%04d".format(newIndex)
  }
}
