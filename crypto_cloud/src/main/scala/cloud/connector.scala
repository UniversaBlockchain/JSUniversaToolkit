package cloud

import scala.collection.mutable.{HashMap, ListBuffer}

/** Represents connection handler for user and his parties
 *
 * @constructor creates connector with service provider and connection data
 * @param service - cloud access API provider
 * @param data - connection info hashmap
 */
class Connector(val service: Api, data: HashMap[String, Any]) {
  var id = data.getOrElse("id", 0).asInstanceOf[Int]
  var connectorType = data.getOrElse("type", "").asInstanceOf[String]
  var parties = data.getOrElse("parties", ListBuffer[HashMap[String, Any]]()).asInstanceOf[ListBuffer[HashMap[String, Any]]]
  var state = data.getOrElse("state", -1).asInstanceOf[Int]

  private var _otherIds = None: Option[ListBuffer[Int]]
  private var _otherId = None: Option[Int]

  /** Updates connector with new connection info
   *
   * @param data - new connection info
   */
  def update(data: HashMap[String, Any]): Unit = {
    connectorType = data("type").asInstanceOf[String]
    parties = data("parties").asInstanceOf[ListBuffer[HashMap[String, Any]]]
    state = data("state").asInstanceOf[Int]
  }

  /** Returns party id of user's party (only for P2P connections) */
  def otherId: Int = {
    if (connectorType != "P2PConnector")
      throw new ApiError(s"otherId is undefined for connector of type $connectorType")

    _otherId = Some(_otherId match {
      case None => otherIds.head
      case Some(id) => id
    })

    _otherId.get
  }

  /** Returns list of user's parties ids */
  def otherIds: ListBuffer[Int] = {
    _otherIds = Some(_otherIds match {
      case None => parties.map(_("id").asInstanceOf[Int]).filter(_ != service.partyId)
      case Some(list) => list
    })

    _otherIds.get
  }
}
