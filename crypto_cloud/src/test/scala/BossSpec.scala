import boss._
import boss.jsany._
import org.specs2._
import tools.universa.UniversaTools._

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js

class BossSpec extends mutable.Specification {

  case class Fruit (name: String, size: Int)

  Boss.register[Fruit]

  "encode case class" >> {
    val apple = Fruit("Apple", 3)
    val dump = Boss.dump(apple)
    encode64(dump) === "HyNuYW1lK0FwcGxlI3NpemUYM19fdHlwZXNCb3NzU3BlYyRGcnVpdA=="
  }

  "decode case class" >> {
    val encoded = decode64("HyNuYW1lM09yYW5nZSNzaXplIDNfX3R5cGVzQm9zc1NwZWMkRnJ1aXQ=")
    val orange = Boss.load(encoded).asInstanceOf[Fruit]
    orange.name === "Orange" && orange.size === 4
  }

  "process Int" >> {
    val prim = 5
    val dumped = Boss.dump(prim)
    val loaded = Boss.load(dumped).asInstanceOf[Int]
    loaded === prim
  }

  "encode string" >> {
    val dump = Boss.dump("abc")
    encode64(dump) === "G2FiYw=="
  }

  "encode HashMap" >> {
    val map: HashMap[String, Any] = HashMap(("size", 1), ("name", "test"))
    val dump = Boss.dump(map)

    encode64(dump) === "FyNzaXplCCNuYW1lI3Rlc3Q="
  }

  "encode multilevel HashMap" >> {
    val map: HashMap[String, Any] = HashMap(("size", 1), ("name", "test"))
    val map2: HashMap[String, Any] = HashMap(("inner", map), ("prop", 123))
    val dump = Boss.dump(map2)

    encode64(dump) === "FyNwcm9wuHsraW5uZXIXI3NpemUII25hbWUjdGVzdA=="
  }

  "decode HashMap" >> {
    val encoded = decode64("FyNzaXplCCNuYW1lI3Rlc3Q=")
    val decoded = Boss.load(encoded)
    val map = decoded.asInstanceOf[HashMap[String, Any]]

    map("size") == 1 && map("name") == "test"
  }

  "decode multilevel HashMap" >> {
    val encoded = decode64("FyNwcm9wuHsraW5uZXIXI3NpemUII25hbWUjdGVzdA==")
    val decoded = Boss.load(encoded)
    val map = decoded.asInstanceOf[HashMap[String, Any]]
    val map2 = map("inner").asInstanceOf[HashMap[String, Any]]

    map2("name") == "test" && map("prop") == 123
  }

  "encode List" >> {
    val list = ListBuffer[Any]()
    list += "abc"
    list += 123

    val encoded = Boss.dump(list)
    encode64(encoded) == "FhthYmO4ew=="
  }

  "decode List" >> {
    val encoded = decode64("FhthYmO4ew==")
    val decoded = Boss.load(encoded)
    val list = decoded.asInstanceOf[ListBuffer[Any]]
    list(0) === "abc"
  }

  "encode Date" >> {
    val d = new js.Date("1523287443000".toLong)

    encode64(Boss.dump(d)) == "eRMLLlaF"
  }

  "encode Long" >> {
    encode64(Boss.dump("1693900294".toLong)) == "0Abe9mQ="
  }

  "decode Date" >> {
    val d = Boss.load(decode64("eRMLLlaF")).asInstanceOf[js.Date]

    d.getTime == "1523287443000".toLong
  }

  "read stream" >> {
    val source = decode64("GAUQuCAAyKCGAQ==")
    val reader = new BossReader(source)

    val algorithmIndex = reader.read()
    val tag = reader.read()

    algorithmIndex mustEqual 3
    tag mustEqual null
  }

  "read custom error" >> {
    val encoded = decode64("DytlcnJvciczX190eXBlFRVzQ09NTUFORF9GQUlMRUQ7bWVzc2FnZbsmcGFyYW1ldGVyIGNhbid0IGJlIGNvbnZlcnRlZCB0byBieXRlW10zb2JqZWN0Aw==")
    val loaded = Boss.load(encoded).asInstanceOf[HashMap[String, Any]]
    loaded("error").asInstanceOf[HashMap[String, Any]]("error") == "COMMAND_FAILED"
  }

  "read item result" >> {
    type dict = HashMap[String, Any]
    val encoded = decode64("DzNyZXN1bHQPU2l0ZW1SZXN1bHRPS2NyZWF0ZWRBdHkMJT5WhStleHRyYQczX190eXBlU0l0ZW1SZXN1bHRLaXNUZXN0bmV0aStzdGF0ZUNBUFBST1ZFRENoYXZlQ29weWlTbG9ja2VkQnlJZAVLZXhwaXJlc0F0eRAgHTeRM2Vycm9ycwU=")
    val loaded = Boss.load(encoded).asInstanceOf[HashMap[String, Any]]
    loaded("result").asInstanceOf[dict]("itemResult").asInstanceOf[dict]("state") == "APPROVED"
  }

  "write stream" >> {
    val writer = new BossWriter()

    writer.write(3)
    writer.write(null)
    writer.write(2)
    writer.write(32)
    writer.write(0)
    writer.write(100000)

    encode64(writer.get()) === "GAUQuCAAyKCGAQ=="
  }

  "sequence of bytes as capsule" >> {
    val binary = decode64("J1NzaWduYXR1cmVzBiN0eXBlU3VuaWNhcHN1bGUjZGF0YcSZAh9DcmV2b2tpbmcGQ2NvbnRyYWN0L0thcGlfbGV2ZWwYU2RlZmluaXRpb24nW3Blcm1pc3Npb25zHzNxQmNTNkcfI25hbWVjY2hhbmdlX293bmVyI3JvbGUfbWtAY2hhbmdlX293bmVyW3RhcmdldF9uYW1lK293bmVyM19fdHlwZUNSb2xlTGlua6WrQ2hhbmdlT3duZXJQZXJtaXNzaW9uM2FNN1pMRj9tU3NwbGl0X2pvaW59H21bQHNwbGl0X2pvaW6VnaWtU2ZpZWxkX25hbWUzYW1vdW50S21pbl92YWx1ZRswLjFDbWluX3VuaXS9H4tqb2luX21hdGNoX2ZpZWxkcw5jc3RhdGUub3JpZ2lupZtTcGxpdEpvaW5QZXJtaXNzaW9uM3dCVUNySh9tM3Jldm9rZX0fbTtAcmV2b2tllZ2lraWDUmV2b2tlUGVybWlzc2lvblNjcmVhdGVkX2F0eVN1TFuFI2RhdGEfS3VuaXRfbmFtZWtNeSB0ZXN0IHRva2Vue3VuaXRfc2hvcnRfbmFtZRtNVEtrdGVtcGxhdGVfbmFtZWtVTklUX0NPTlRSQUNUM2lzc3VlcidtvTQja2V5cx1LYWRkcmVzc2VzDhdDdWFkZHJlc3O8JRAaW8O73TiHxyzOzOXqc5mwDawNqZn+9AGd+LRQhic8iQPJtY6lU0tleUFkZHJlc3OlU1NpbXBsZVJvbGUrc3RhdGU3vSt5U3VMW4VTY3JlYXRlZF9ieR9tO2NyZWF0b3KVvTSlrb0sD70dGzEwMJ0fbZ2VvTSlrUNyZXZpc2lvbghTZXhwaXJlc19hdHljJTJfhWt0cmFuc2FjdGlvbmFsBaWDVW5pdmVyc2FDb250cmFjdBtuZXcdO3ZlcnNpb24Y")
    val loaded = Boss.load(binary)
    val unicapsule = loaded.asInstanceOf[scala.collection.mutable.HashMap[String, Any]]
//    println(unicapsule)

    unicapsule("version") === 3
  }
}
