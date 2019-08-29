package node

import models.{ HashId, PrivateKey, PrivateKeyExported, PublicKey, TransactionPack, Parcel }
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

import boss._
import boss.jsany._

import util.control.Breaks._

case class Node(url: String, key: PublicKey)

@JSExportTopLevel("Node")
class NodeApiExported(val nodeApi: NodeApi) extends js.Object {
  type jsDict = js.Dictionary[Any]
  type jsBytes = js.Array[Byte]

  def command(
                 name: String,
                 params: jsDict = (new js.Object).asInstanceOf[js.Dictionary[Any]]
               ): js.Promise[jsDict] = nodeApi.command(name, params)

  def checkContract(id: jsBytes): js.Promise[jsDict] =
    nodeApi.checkContract(HashId.fromBytes(id))

  def checkParcel(id: jsBytes): js.Promise[jsDict] =
    nodeApi.checkParcel(HashId.fromBytes(id))

  def registerContract(packed: jsBytes): js.Promise[jsDict] =
    nodeApi.registerContract(packed)

  def registerParcel(packed: jsBytes): js.Promise[jsDict] =
    nodeApi.registerParcel(packed)
}

/** Represents node API
 *
 * @param privateKey - PrivateKey instance to authorize with
 * @param debugMode - if true, will call localhost
 */
class NodeApi(val privateKey: PrivateKey, options: HashMap[String, Any] = HashMap[String, Any](), debugMode: Boolean = false) {
  val connections = HashMap[Int, NodeConnection]()
  var nodes = ListBuffer[Node]()
  var ready = Promise[Boolean]()
  var version: Option[String] = None
  var startUrl: Option[String] = None
  var nodeKey: Option[PublicKey] = None

  val useProxy = options contains "proxyURL"

  type jsDict = js.Dictionary[Any]
  type jsBytes = js.Array[Byte]

  /** Will try to connect Node until success */
  def connect: Future[NodeApi] = {
    ready = Promise[Boolean]()
    val self = this

    val nodesList = ListBuffer(
      ("http://node-1-com.utoken.io:8080", "HggcAQABxAABoJHPqusDZXM/24+65xno0cagoe/0+EvMZ96lPxPtEFVbcgy3D2smKFVNhPFjZoBe+GxqUQEPsb4YY3T8ovUFVUBPYv8dwXGlahnpXiKPJvlTsOrTyK/jUuDNEI64MvtON2+/8EbF02B5jWF4zY276GLfUoSE/h+PLKo49fXHV/uVe73P/4fONqt8NQFKNXU/y2izFHt1TSY7H3PywZqY9o+0y2lwi+cNPHnB1ZCCur5uqgnR81ZEaejNFz1Xa5+B2OOChOT8VFFtu4vVAFxuigX9sPoz2/EXF/9H6/8k7+mxc+BVW8WH6A7yCMGaBowJRo8i6FBchAl88776edXJow=="),
      ("http://node-2-com.utoken.io:8080", "HggcAQABxAABlBTsd5kGqDGrjPs3gsefSQdcv4Wbg3+C3Jt3AQo7WLJslHwYVRJOjH103dsbHG3mT+EoRNZ9IVBn9GQRizYtHjFCldLfI+rt7EhuXsdGI59XszyFNbBf0CgW111lqoWlbULf/K3wjUtQ8IEtltmaQ63ZyAg5X2j0gvO2Pl9tt8sS53ejLB5EfuPhoUURqnA8D0H+5FATEADAWQe4mDGuSnwl2QdVGgGkzhyOcKf6CH+s+BrsEW+DTkC6jz6RMFLnYi2t8H9wDm04Dkbd5PLbkGn6Ywr4PGu7h5zSIhJ4UJLBbOmltKlHyHDXZSDP/nINeaQMDpnhE77Z6xVqGARQ4w=="),
      ("http://node-3-com.utoken.io:8080", "HggcAQABxAABlBTsd5kGqDGrjPs3gsefSQdcv4Wbg3+C3Jt3AQo7WLJslHwYVRJOjH103dsbHG3mT+EoRNZ9IVBn9GQRizYtHjFCldLfI+rt7EhuXsdGI59XszyFNbBf0CgW111lqoWlbULf/K3wjUtQ8IEtltmaQ63ZyAg5X2j0gvO2Pl9tt8sS53ejLB5EfuPhoUURqnA8D0H+5FATEADAWQe4mDGuSnwl2QdVGgGkzhyOcKf6CH+s+BrsEW+DTkC6jz6RMFLnYi2t8H9wDm04Dkbd5PLbkGn6Ywr4PGu7h5zSIhJ4UJLBbOmltKlHyHDXZSDP/nINeaQMDpnhE77Z6xVqGARQ4w=="),
      ("http://node-4-com.utoken.io:8080", "HggcAQABxAAB07TWfunHzQT7l2uVKPbHvt/2b1dLHyK2j3WpQlI/NDtEHtFgBhcb0EE2YMM61oJElho8gfPZb6TUgWq8RoyA0EyAoHBPAbDu4+CYPm3HoIgvLDVs8ycgOtmmw6wf6TYJDubsTS+r9AooPED2Js2GIc85PGz9bKkdCcTrkuTsRe6dmCMMY2GL9tkD4ZEYoFRU6iVxQbL7qmRreaKIN2xhRbEfOl5w+4S/cCpJ1dx4ngwiax2iJtQDcJWNpOgqX8UzE8m83xxwAboL3itSUlvvU3Bvfn/LYxWJ2pG4XlW6YKMwblOq1V85GSOsRTee0I2Na1AI/3kusuGzy5ECBbNhlQ=="),
      ("http://node-5-com.utoken.io:8080", "HggcAQABxAABtBIXp+RqbpBFtR2oY+bmJJWXytg0gf77WnrntbyQsxhhmS8WnoohBFn5IYzOd1poW4zDJM00MWZmV8VZdCrPp8jt3QCaay4axTT6XEc/JvARJ2KAkO0iQNKgsoPnZUVlO8DO1+X7R+8DkNnlXvld0xexyI0/yKDBGrK8bE0w4oets+4X9oFF1BW9WEvy11+/bc1vS9n8nvX9XCvKh6e+bRF71625fcCzsxap/vdCjDmNOsjjipn8l6BnCS/ygQjfBitqzD9mmu1kRlrQifpbs74UutwFgkGHjq3g7lrrNUikqPjg61XUIm5hfjnHmhJMEuFV5bGmJOVjL/ZYjDxWEQ=="),
      ("http://node-6-com.utoken.io:8080", "HggcAQABxAAB2PGl3X3pXussqGHuI59lnxBlJ8njiolzhBrh3nuN6P8AWMWAr9Rlx11B3MrO9dIusWhp9JwGLiGJbKEL9CHIYuPkjpSmhe8wH+Rvik8Lma+haCM8pAUU74NK3mZ0QxmXsV9SYb2phenoQquArIfovR/erMsDvsVIVJlDPzvrqied/0quE5fJ7XvGuYlb7bAho6HOhMFdpGj3dZwYm5EIAYwXHUWeYVse+ZR8DcS5AKnk4Cz140zd2Rl+/uucfrkgiJet+LTx5YhfOKZ9kpjclw682eJnSswlBxJLBsloks8VBJf8dsSGaZxT29cRaRecAoTy9zpMjHPd4VBxpldDew=="),
      ("http://node-7-com.utoken.io:8080", "HggcAQABxAAB4VjBNTN0zm9IzgGXAiqukvMphRxO3Eywf0Lqff4JfYDY3Nc3Jg5IjQID/vriCErumnxdQnOL0ci6uJpo6tgDGkvJ4uC7z3Sic+Tsoe6b1vmUSJcNEaBXlGH6kPaNkd2ZlTvBDZ/Iv1v2LwlMDHxK+NrPVHBhEXGguvz7u73fqpC9g3GPpThZwRG+X76YFoeK3P54iwwN127SB6GDV9hFp2XjuEvpLURT0+fnWgqfoFrfW0AMfA/7V6SRb8yF+P45ncOvi3sKJ6owK+/au2dlsHAIE+SCfIOg1iaQBKc3egBlliEZOK/9JSrIBrRxQfqvLCytnbYj7Fed2jmqa9IBaQ=="),
      // ("http://node-8-com.utoken.io:8080", "HggcAQABxAABnMPCqr/rXOHIYej+/BOniOOkreL0lWbCU7k1GmC3xM9zSSPxL5yZCcmhygttNq6AYoRAjROVAN7ioSF0tORYiqAzzT1GWe55KVFzRsAM6cOJmk0bfYySOiL5qLxjmqnbcCCHOIBkLUjVE8l6TSL284quuQ3iYtNmHdHvC6cb6lzDwqizIsGHaZpyp7A2N52saSUcVaZOPXrqTZP6bRjqOj5KEVqOwR3ZDtXkIUvXZtnrIkI/EoIhjvhv/oThRHdJOExFiPTfA7xqpuOZlK2PazKAwE2+46oaGk1oXEGnx4+XYhXGpArB6hAAhaUJxFxeNnJ2uXKmE6+M38pEQd28Uw=="),
      ("http://node-9-com.utoken.io:8080", "HggcAQABxAABt9V2b1nxYGjgBGC03GI1JyG+T6w/QzgGW3GAtVGIQkzMhgpFJAmP+bk6ld4nN4qBjiP0m7oXRpsF/wEPR/BVl8SRtXSEhJ0YdNALg6muK8Qvi7o2mqpOFjlnH9hKbJk/we3je9etwsKcx2iXQN07lOXKr3eihSGIM9nIzxHCN2WAbGEX6v9AIxab1FkqIDdu5jq4DcFhP66SXHyU2U9Ryx9SU3MexvOWkfrQis3wlkGr9Oru4B6yaA3gKt9kekHzqZOjVv/Xp555bcH6OAo05TTgIJ74u1M24DR3LwqUNp50d5DusaxRxoneXTCwoCztNMLn4OXcjdT2Z+ePL+gOjw=="),
      ("http://node-10-com.utoken.io:8080", "HggcAQABxAABtu8fYvlDoM4+aNKnGfVegXWatzrlWKEmFhtLIjIfmAuZaXnyQh64hl3VFrOysfuAXSiesKj/ODa5t0LiRe+y/dQi8Zv4X4aNUYQPzOmp6nOvkmfpsASastlKYspNvp2jhrUrNQh0h7Z01R7GGhAuttkDQjAGchCQ/MrqeYMM0PkE30HyRhpb9TGi5WsAH42kQgmFwMKskomTXr7jDc0uCFjkT3lMGwtSOn68qn7Uh0kvwUNdHYAJ0inGPx5O3N82zh6zQCP6Qxn8RWj56iVhk+iTmYjGHH4PiQve4JXFkaqaURt70gb3CynG2JzhEvZ/YB1bFDGBqoTBPjlNcGrBbw==")
    )

    def randomNode: (String, String) = {
      val idx = randomInRange(0, nodesList.size - 1)

      nodesList(idx)
    }

    var selectedNode = randomNode
    val selectedNodeURL = NodeConnection.https(selectedNode._1)

    def tryAgain(e: Throwable): Future[NodeApi] = {
      for {
        wakeUp <- sleep(1000)
        api <- connect
      } yield api
    }

    val mayFail = for {
      networkData <- NodeConnection.request(
        "GET",
        s"$selectedNodeURL/netsigned",
        HashMap[String, Any](),
        HashMap[String, String](),
        options
      )
    } yield {
      val signature = networkData("signature").asInstanceOf[Seq[Byte]]
      val nodesPacked = networkData("nodesPacked").asInstanceOf[Seq[Byte]]
      val nodeKey = new PublicKey(decode64(selectedNode._2))
      // val nodeKey = new PublicKey(decode64(nodesList(2)._2))
      val isValid = nodeKey.verifyExt(signature, nodesPacked)
      if (!isValid) return throw new Exception("invalid node signature")

      val givenVersion = networkData("version").asInstanceOf[String]
      version = Some(givenVersion)

      nodes = ListBuffer[Node]()

      val nodesReceived = Boss.load(nodesPacked)

      for (n <- nodesReceived.asInstanceOf[ListBuffer[HashMap[String, Any]]]) {
        breakable {
          val nodeURL = n("url").asInstanceOf[String]

          if (nodeURL.contains("-8-") || nodeURL.contains("-12-")) break

          val nodeKey = new PublicKey(n("key").asInstanceOf[Seq[Byte]])
          nodes += Node(NodeConnection.https(nodeURL), nodeKey)
        }
      }

      val nodesTotal = nodes.size

      println(s"Connecting to the Universa network v$givenVersion")
      println(s"Loaded network configuration, $nodesTotal nodes")

      ready.success(true)
      self
    }

    mayFail recoverWith { case e => tryAgain(e) }
  }

  def getNode(idx: Int): Future[NodeConnection] = {
    def connectToNode: Future[NodeConnection] = {
      val info = nodes(idx)
      val url = info.url.replace("http:", "https:").replace(":8080", ":443")
      val nodeConnection = Some(new NodeConnection(url, info.key, privateKey, options))

      nodeConnection.get.connect.map(conn => {
        connections(idx) = conn
        conn
      })
    }

    if (connections.contains(idx)) return Future.successful(connections(idx))
    else {
      for {
        isReady <- ready.future
        con <- connectToNode
      } yield con
    }
  }

  def connection: Future[NodeConnection] = {
    for {
      isReady <- ready.future
      con <- getNode(randomInRange(0, nodes.size - 1))
    } yield con
  }

  /** Sends command to Node
   *
   * @param name - command's name
   * @param params - command's options
   */
  def command(
    name: String,
    params: HashMap[String, Any] = HashMap[String, Any]()
  ): Future[HashMap[String, Any]] = {
    def tryAgain(e: Throwable, con: NodeConnection): Future[HashMap[String, Any]] = {
      if (e.isInstanceOf[ApiError]) {
        e.asInstanceOf[ApiError].details("node_url") = con.url
        e.asInstanceOf[ApiError].details("command") = name
      }
      println(s"$e, command will be sent again")

      for {
        _ <- sleep(1000)
        _ <- connect
        api <- command(name, params)
      } yield api
    }

    for {
      con <- connection
      response <- con.command(name, params) recoverWith { case e => tryAgain(e, con) }
    } yield response
  }

  /** Sends command to Node with specific connection
   *
   * @param name - command's name
   * @param con - established node connection
   * @param params - command's options
   */
  private def protectedCommand(
    name: String,
    con: NodeConnection,
    params: HashMap[String, Any] = HashMap[String, Any]()
  ): Future[HashMap[String, Any]] = {
    def tryAgain(e: Throwable): Future[HashMap[String, Any]] = {
      if (e.isInstanceOf[ApiError]) {
        e.asInstanceOf[ApiError].details("node_url") = con.url
        e.asInstanceOf[ApiError].details("command") = name
      }

      println(s"$e, command will be sent again")
      for {
        _ <- sleep(1000)
        _ <- connect
        api <- protectedCommand(name, con, params)
      } yield api
    }

    for {
      response <- con.command(name, params) recoverWith { case e => tryAgain(e) }
    } yield response
  }

  /** Register the contract with the Universa network
   *
   * @param packed - BOSS encoded contract
   */
  def registerContract(packed: Seq[Byte]): Future[HashMap[String, Any]] =
    command("approve", HashMap(("packedItem", packed)))

  /** Register the parcel with the Universa network
   *
   * @param packed - BOSS encoded parcel
   */
  def registerParcel(packed: Seq[Byte]): Future[HashMap[String, Any]] =
    command("approveParcel", HashMap(("packedItem", packed)))

  def registerParcelFinal(packed: Seq[Byte]): Future[HashMap[String, Any]] = {
    val parcel = Boss.load(packed).asInstanceOf[Parcel]
    val paymentPack = Boss.load(parcel.paymentPack).asInstanceOf[TransactionPack]
    val payloadPack = Boss.load(parcel.payloadPack).asInstanceOf[TransactionPack]
    val paymentId = paymentPack.mainCapsule.hashId
    val payloadId = payloadPack.mainCapsule.hashId

    for {
      con <- connection
      res <- protectedCommand("approveParcel", con, HashMap(("packedItem", packed)))
      paymentState <- getFinalState(paymentId, con)
      payloadState <- getFinalState(payloadId, con)
    } yield HashMap(("payment", paymentState), ("payload", payloadState))
  }

  def getFinalState(
    id: HashId,
    con: NodeConnection
  ): Future[HashMap[String, Any]] = {

    def checkState(
      state: HashMap[String, Any]
    ): Future[HashMap[String, Any]] = {
      val itemResult = state("itemResult").asInstanceOf[HashMap[String, Any]]
      val st = itemResult("state").asInstanceOf[String]

      if (!st.startsWith("PENDING")) Future.successful(itemResult)

      else {
        for {
          _ <- sleep(100)
          newState <- getFinalState(id, con)
        } yield newState
      }
    }

    for {
      state <- protectedCommand("getState", con, HashMap(("itemId", id)))
      finalState <- checkState(state)
    } yield finalState
  }

  /** Checks contract state by id
   *
   * @param id - contract ID
   */
  def checkContract(id: HashId): Future[HashMap[String, Any]] =
    command("getState", HashMap(("itemId", id)))

  /** Checks parcel state by id
   *
   * @param id - parcel ID
   */
  def checkParcel(id: HashId): Future[HashMap[String, Any]] =
    command("getParcelProcessingState", HashMap(("parcelId", id)))

  implicit def bytesToHashId(id: Seq[Byte]): HashId = HashId.fromBytes(id)
}

/** Factory for [[node.NodeApiExported]] instances. */
object NodeApiExported {
  /** Creates instance by PrivateKey
   *
   * @param privateKey - PrivateKey to authorize with
   */
  def apply(
    privateKey: PrivateKey,
    options: HashMap[String, Any]
  ): Future[NodeApiExported] = {
    val api = new NodeApi(privateKey, options)
    api.connect.map(nodeapi => new NodeApiExported(nodeapi))
  }

  // @JSExportStatic("connectProxy")
  // def createProxyJS(privateKey: PrivateKeyExported, options: js.Dictionary[String]): js.Promise[NodeApiExported] = {
  //   apply(privateKey.privateKey).toJSPromise
  // }

  @JSExportStatic("connect")
  def createJS(
    privateKey: PrivateKeyExported,
    options: js.Dictionary[Any] = js.Dictionary.empty[Any]
  ): js.Promise[NodeApiExported] =
    apply(privateKey.privateKey, options).toJSPromise
}
