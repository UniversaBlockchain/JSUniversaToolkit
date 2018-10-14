import cloud.Capsule
import cloud.Tools.syntex1
import models.{KeyInfo, SymmetricKey}
import org.specs2._
import tools.universa.UniversaToolsJS._
import tools.universa.{HMAC, SHA, UniversaTools}

import scala.collection.mutable.HashMap
import scala.scalajs.js.JSConverters._


class CryptoCloudSpec extends mutable.Specification {

  val privateKey1 = new models.PrivateKey(decode64("""
    JgAcAQABxAAB2ZLz9pA2qlUys9oomId1YF8u8n8T98ekEv8gYAyBQfnHnhqciPcTe4AoZb+r
    4h1sBgwhZ39pXXNOZDBOMd+e2UHIHYAZvi6R7lNnOm0waLCHH7rNXJLCzPHpp7vhAhwVao9p
    u5U3Maw6dwAVvb4XBoQs2YyMjpSApQJOPizGqf6l4D5HW1AxLbWhlKvcs+wBapb9H7266Kzv
    f2mK2HARi7aQHO5fA/+YGXweTLjt+iLB2TSOvl4juz6w7nmV18QF88FP1DkMWVxyHnFDaIB9
    E2XCe80Qr9dhGOfJcWefvJcdsvgtJMeEYm87IGt0yI/MlpyWFzjMj7VzT+NtQUBEN8QAAdqS
    4PEfNqpVMrPaKJiHdWCMAtUXsa1VMki4p0wG02pCp4h+ByPqzZx7BgNZxgYMIWd/VF0zQvCZ
    4r05BJNtYrG9Of9XSzyGBmd9Nyjj0TKLLYnt60QZx3Wpu+E9JEHlJGm7lTcxpSASABW9qzIz
    B/SEIs1roH3kSStMiSIWrGyhlIXcs+wBapY6SKNwuujh5Ha/9W/+G3HDzZiv0VNZtjNcYSkF
    HD6RyHcjjJdbe2xEzjxnIcbN7UGLsydve5TJPsKtaDxQAjJk0JpVMzruxdhiOb+Otq9IvZtV
    0weA4cMUNr4N5GOL6TeprGf8TFwpJ7DpgxpZYoYMO6r62hn8pIU=
  """))

  val privateKey2 = new models.PrivateKey(decode64("""
    JgAcAQABxAABzSj/76kVC9Oo6bBBDkEZIO2FPHl+QQOOkyUSW7X+wyWOq6gW/McbqGokidqW
    XYJfwKPauzH1GQ7oDCoOenPJEi4Jm4oAwKgZ53ngsssHynGs+2IJ2NYH61jtMUUp7O0A3lWf
    MgG9M0amTBGcuSKQ1IalP3cIeiMuo/2zeUatjm3GSY2o3vxjwwI40mIrzVjzGG5uPSD5socv
    0yEnI21utLV/opfJgUsqOIH+KlOO2NRZ9/BPrdv/wUP24Cs31rZIs8nfUap/JCXkcP/hBdWD
    xQ0aLaLIn5E6OvkKca4vr6/5AOCe1EJfKhx1K35PDFSxVkCumt/Ryc/NdXF9RacTgcQAAf7z
    J5RIYuMi9trkZJNUryi9Mtk4sK64olv5GG5uPSD5socv0yEnI21utLV/omwmZK+IWp4IMM8K
    Skth1ONlCz4N1FB/IfKUVHmZPWp9Z9lgAXpC2iXkcP8i/WME6AO8s2zx7HkboeTMCoiGPKbY
    4Xily6cbsJUE4t7P3tWoG7sB5DwC6ornlUhYZPkeNZH4ZMQ9p3pZk9ITurJM2flaJkj0y6il
    ZCPnuZm3L6SCT0vZXF1h9EMOIwtxRz7wzFMJJGVNDLjHH03bpQB865O4CkZuxUZl+f57nDgE
    1+vRPwdory0ozvpyQFRM0usYFfvUDfhk3MSti/SsBTg8VpYLF8c=
  """))

  // "sleep for second" >> {
  //   println("Start sleep (1 sec)")
  //   val readyLater = for {
  //     delayed <- sleep(1000)
  //   } yield {
  //     println("Done sleep (1 sec)")
  //   }
  //   true
  // }

  "calculate syntex1" >> {
    encode64(scala.collection.mutable.Seq(syntex1("Hello world"):_*).toJSArray) == "wGxN2mdczbpIQLys/Um73LCqvpxjNHDiu2WUJaEoFsGOCsUL"
  }

  "Read/Write key info" >> {
    var ki = KeyInfo(decode64("GAUAuCA="))
    ki.toString mustEqual "AES256, tag=null kLength=32"

    ki = KeyInfo(ki.pack)
    ki.toString mustEqual "AES256, tag=null kLength=32"

    ki = KeyInfo(decode64("CCw9x/cXRwDAAAI="))
    ki.toString mustEqual "RSA_PUBLIC, tag=Pcf3F0c= kLength=512"

    ki = KeyInfo(ki.pack)
    ki.toString mustEqual "RSA_PUBLIC, tag=Pcf3F0c= kLength=512"

    ki = KeyInfo(decode64("GAUQuCAAyKCGAQ=="))
    ki.toString mustEqual "PBKDF2(HMAC_SHA256,100000), AES256, tag=null kLength=32"

    ki = KeyInfo(ki.pack)
    ki.toString mustEqual "PBKDF2(HMAC_SHA256,100000), AES256, tag=null kLength=32"

    ki = KeyInfo(HashMap(
      ("tag", UniversaTools.decode64Seq("1234")),
      ("algorithm", "AES256"),
      ("keyLength", 32)
    ))

    ki.toString mustEqual "AES256, tag=1234 kLength=32"

    ki = KeyInfo(ki.pack)
    ki.toString mustEqual "AES256, tag=1234 kLength=32"
  }

  "Match key info" >> {
    val kaes = KeyInfo(HashMap(
      ("tag", UniversaTools.decode64Seq("123")),
      ("algorithm", "AES256"),
      ("keyLength", 32)
    ))

    val kpub = KeyInfo(HashMap(
      ("tag", UniversaTools.decode64Seq("123")),
      ("algorithm", "RSA_PUBLIC"),
      ("keyLength", 32)
    ))

    val kpriv = KeyInfo(HashMap(
      ("tag", UniversaTools.decode64Seq("123")),
      ("algorithm", "RSA_PRIVATE"),
      ("keyLength", 32)
    ))

    val kpass = KeyInfo(HashMap(
      ("tag", UniversaTools.decode64Seq("123")),
      ("algorithm", "AES256"),
      ("keyLength", 32),
      ("prf", "HMAC_SHA256"),
      ("rounds", 4000)
    ))

    kaes.matchType(kaes) must beTrue
    kaes.matchType(kpass) must beTrue
    kaes.matchType(kpriv) must beFalse
    kaes.matchType(kpub) must beFalse

    kpass.matchType(kaes) must beTrue
    kpass.matchType(kpass) must beTrue
    kpass.matchType(kpriv) must beFalse
    kpass.matchType(kpub) must beFalse

    kpriv.matchType(kaes) must beFalse
    kpriv.matchType(kpass) must beFalse
    kpriv.matchType(kpriv) must beTrue
    kpriv.matchType(kpub) must beTrue

    kpub.matchType(kaes) must beFalse
    kpub.matchType(kpass) must beFalse
    kpub.matchType(kpriv) must beFalse
    kpub.matchType(kpub) must beFalse
  }

  "PBKDF2" >> {
    val dk = UniversaTools.pbkdf2(new SHA(256), HashMap(
      ("password", "password"),
      ("salt", "salt"),
      ("iterations", 4096),
      ("keyLength", 32)
    ))

    encode64(dk.toJSArray) == "xeR41ZKIyEGqUw22hFxMjZYok6ABzk4RpJY4c6qYE0o="
  }

  "Symmetric Key PRF" >> {
    val sk = new SymmetricKey(decode64("SYGT28WTY4xRjXPB+PoKt2G3HxdN4okkXJAvSJ8mvjk="))
    val src = decode64("Zm9vIGJhciBiYXouIHRoaXMgaXMgYSB0ZXN0IGRhdGEgdGhhdCBpcyByZWFsbHkgbG9uZ2VyIHRoYW4gYSBibG9jayBzaXplLiDQktC20YPRhSE=")
    val ee = decode64("p/fvKVz+vIrgWEED8oUtmKmfsNO+P+LUnnFUQy9xtYt7oxRWDsyEfF2Ik67lbE/7by745P25aT5sYSG6LjZV/dEaUqtb0cGdwqdRYS5prfda/23kSfTAFt6rpZSlZzPh5BgE/Z/SaIYhkmMZgSHslTzePR0JXs5Dowq1lTWg6xyoqTE=")
    val etaEncrypted = ee

    val decrypted = sk.etaDecrypt(etaEncrypted)

    decrypted.size mustEqual src.size
    encode64(decrypted.toJSArray) mustEqual encode64(src)

    val encrypted = sk.etaEncrypt(src)
    val decrypted2 = sk.etaDecrypt(encrypted)

    encode64(decrypted2.toJSArray) mustEqual encode64(src)
  }

  "HMAC" >> {
    val key = "1234567890abcdef1234567890abcdef"
    val data = "a quick brown for his done something disgusting"
    val hmac = new HMAC(new SHA(256), key).get(data)

    encode64(hmac.toJSArray) mustEqual "la3Xl78Z3ktK2JLoDpPKthhqVilUX6+e6a0WultI9f8="
  }

  "rsadecrypt" >> {
    val cipherText = decode64("""
      aP2vtiA3oDNKFXRCsG6yV3e+S8rYyP3pV6Kfinl5dv/YNcEZx7FH8AHw5rOWdBWDSQGemp4A
      9OpasvFIkXJB/BTdVFlAabzIS2mpXsnx8y0hxK7V49+uGYL1SwtThK1ifenVzM7t6dBvQqhy
      qJ9OVhCXa4FPDvOd4DTn/+SSwyJZtDIex7c4nAV+7YXS1fhZVJ1pKOAh+qUPWhShFidOBaxc
      kS2MeUUV+O3hidRPTtbY6kEG5i/0swLm04+Zu+rkOhgmZ71wGDXKMxdY+IlbQu6RLQvvVRwM
      WoivOli8UzKyzifJp9QlR6obHw63N2HdAhdjbPxVZuX2UKtDmFc2wLiAl83A6A4azjWlVXiH
      ZjX1+FhZMd3Qn412cf4YtamUTzFLjJEtRihE0wZfG2sWCErNfWIYg8/a9EJHR6SiqP5Jz+sR
      +AE3VFqli9kG3nEZCJU+5eJTzRofGp7PvgN8vvO3owfVJk+dvdPXe4rrf97UCu+Fj81gnS29
      dsMZ53Y0yCWNKZ40tEXerbKMBEW/mO4nkloS5U47/xSy1FB5gTiM5crKIudQZLROu0yC1X0R
      fhQ/M4M7Ifz4GNs4RB2eLRCoX90wEOGE4BeJ4OJ9tcR37ofBd7jQiaYyedL20uFzQYQZ6cCY
      nZM19nBU9t9COGYDg1mhrhuWR9WMdLVvfQc=
    """)

    val packedPrivateKey = decode64("""
      JgAcAQABxAAB5eaGe+7EifujmPC+dEj5zqGbmuT5pUGMqnqV0WK8sf4I+6OILEVQgmISMeHb
      U2frotdX5SjJ2qtZnH27JLKFtzK9T0uMozGAPHyLqlJ0HX7UF3lJhSiCpSki1Uu4J43wdl/s
      w8DgdDW31fRSaLnNQ3d/aukALzG8puEXdlT9o++W64E5Pb1QhNsQwakgzW7cCDBEKfryAIf5
      obgOWaj8hPO4AifxUT2Eyy6qjA9UgQcDeqPDc9OrL2SFNe5qXFVmFslPTT9UTjdtJ8vJIE8z
      yytI4rt+Zk09JnOsMIw0ptJ2fYoqMcgvblcTTB1k4tVwKDmQC1VSOaLRk7KWdGSLB8QAAcTW
      K3O9pYC869epQFNDtTFug7QapIYKzDNcGFBc2clyMfLa9T/szBh+qui7+YWsyK5k3MsC5xxi
      z0C/oJ+u9Qk9bh8ATRDSM4M162V19xhb/3RkMj1G+VpVak0k2Pvlyt5CbSj/1JgD2z69zpXn
      W24zo+3A/E3AjPQHSI2NPHm8oP0daTQgAKvSVET5GEmcEcPGChUG4SRULIXS27RHSkrKSs40
      r8G7fLIx6hkZjhdr4a754nG746fAb6GJyWf6h6WzVjStwnazgQHtPXY82SKDiCxjy4eectnz
      aeAtto52xdaXTaN33MS1v/wPL99WwA48lv/K+wmYyVo56Ok8bGM=
    """)

    val key = new models.PrivateKey(packedPrivateKey)
    val plain = key.decrypt(cipherText)

    encode64(plain.toJSArray) mustEqual "xQ05fry7sDV6qMgT6MM1i14AFcRFNeUmjuLu31w/rPE="
    encode64(key.decrypt(key.publicKey.encrypt(plain)).toJSArray) mustEqual "xQ05fry7sDV6qMgT6MM1i14AFcRFNeUmjuLu31w/rPE="
  }

  "key info of keys" >> {
    val packedPrivateKey = decode64("""
      JgAcAQABxAAB6X7cH9NdSxJ1rR/7QeRmDCWM0qNIJkQnI/T8kIAFt2VElm+7XeOEpN7tJC85
      dddWN6hegqW5FrJ8Ug2w8wBuseb/nZpEPeXzKjnAGpd7vrx3qfrvQirjCKyVE6OyseLGG1RX
      vcMTseqdCLAJz/a00SdgqRjK5zH6BhCJiRzV8tBsycopGrtPDbHbiSpgmYqvk63nLAxUrD6K
      /ZdfIN2P7HYekN9Um16L8e9URo8oqxTAv5kVLr04pA0GajBXl6jUa5Kp/xawSJmOeWY7Hpoi
      3u2zUa/sMs7ORivG9Hbvmj/S89wCjyFd0etLsdT1DH5bnZqWY34pFNuSqOvUKF8AdcQAAfjR
      0ILGcT0oRWN+oa5veOJy0icrk+KpCtDOcDSBLB6glU2HuS75WDJhlWDKcjBCm+JdpDRvc+6I
      SiDs3uUwoMz49mOkGriGJgMwUAnn+o2k+4aL6f2xfOLpGOiokKwGXg86zQLFD20qqToxfrZF
      vzjmVtM9msuNxeJjJtt/2tx6iMogaql8B6CqJLTYuKdb+aJPp8oGNit2ofsp7nbzSKSAXWAX
      3d25H8HAhJ+xDCJ3r0gmRCcj9PydN2XsqHGWb7G9Rs4H0HgXndP9/fHjyiPLa/15BuiluRay
      4VJnmhFR0TjrEL+nURLBubWit2VY/I0GxfDMdlwz3qi00lLW7ss=
    """)

    val privateKey = new models.PrivateKey(packedPrivateKey)
    val publicKey = privateKey.publicKey

    encode64(privateKey.publicKey.fingerprint.toJSArray) mustEqual "B5l9sd9iOGaGVJk+3dTD//7a+heSx600CP04k8uSPTlk"
    encode64(publicKey.fingerprint.toJSArray) mustEqual "B5l9sd9iOGaGVJk+3dTD//7a+heSx600CP04k8uSPTlk"

    val prki = privateKey.info
    val pubki = publicKey.info

    prki.algorithm mustEqual "RSA_PRIVATE"
    pubki.algorithm mustEqual "RSA_PUBLIC"

    prki.keyLength mustEqual 512
    pubki.keyLength mustEqual 512

    encode64(prki.tag.get.toJSArray) mustEqual encode64(pubki.tag.get.toJSArray)
    encode64(prki.tag.get.toJSArray) mustEqual "mX2x3w=="

    prki.matchType(pubki) mustEqual true

    val ski = new SymmetricKey().info

    ski.algorithm mustEqual "AES256"
    ski.keyLength mustEqual 32
  }

  "capsule public" >> {
    val c = new Capsule()
    c.publicData = HashMap(("hello", "world"))
    c.addSigner(privateKey1, HashMap(("name", "first")))
    c.addSigner(privateKey2)
    val packed = c.pack
    val c1 = new Capsule(packed)
    c1.publicData("hello") mustEqual "world"
    c1.isSigned mustEqual true
    c1.privateData mustEqual None
  }

  "capsule private asymmetric" >> {
    val c = new Capsule()
    c.publicData = HashMap(("hello", "world"))
    c.privateData = Some(HashMap(("foo", "bar")))
    c.addSigner(privateKey1, HashMap(("name", "only")))
    c.addEncryptor(privateKey1.publicKey)
    val packed = c.pack
    val c1 = new Capsule(packed)

    c1.publicData("hello") mustEqual "world"
    c1.isSigned mustEqual true
    val x = c1.decrypt(privateKey1)
    val priv = c1.privateData.get
    priv("foo") mustEqual "bar"
  }

  "capsule private symmetric" >> {
    val sk = new SymmetricKey()
    val c = new Capsule()
    c.publicData = HashMap(("hello", "world"))
    c.privateData = Some(HashMap(("foo", "bar")))
    c.addSigner(privateKey1, HashMap(("name", "only")))
    c.addEncryptor(sk)

    val packed = c.pack
    val c1 = new Capsule(packed)
    c1.publicData("hello") mustEqual "world"
    c1.isSigned mustEqual true
    val x = c1.decrypt(sk)
    val priv = c1.privateData.get
    priv("foo") mustEqual "bar"
  }

  "capsule private password" >> {
    val binary = decode64("""
      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
      UgmlBw==
    """)

    val capsule = new Capsule(binary)
    capsule.decrypt("qwer") mustNotEqual false
  }
}
