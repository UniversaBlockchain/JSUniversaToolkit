import boss._
import boss.jsany._
import models._
import org.specs2._
import tools.universa.UniversaTools._

class KeysSpec extends mutable.Specification {
  Boss.registerClass[PublicKeyJS](PublicKeyExported.fromJS, "RSAPublicKey")

  "read private key" >> {
    val encoded = "JgAcAQABvIEAxIA1enr/js+6wCO5AfidA7cxOAGF6un36HGCAlv8WiUwkZV8kUsN4KdCmEcjfDr6WBdnxfftD8uv3LbT3LzTo4rpMv0EkBVfC3s/HnK/kLrPyrnt3cycZdbF6P939DMFAyiUGmD1HFFlIfPCtGVszlIU4aRvPNLRfuSno52Iqe28gQCoPVqU6HYVl6RPbMxM0MwQr6mUNOOEh4TvxHdTMw6O1Qv+wTPu0bHocV6CcLx132FEKoVfcLrTj/bkjNiTpwSzkIlQIWhokljvhSD9ILniAkOgtbKGgDuQ2ikrcPHSSe+uRJXGlBkyxDVKR5i+6ZVQunJk4vdhfwRLOk2KYgEyIQ=="
    val privateKey = new PrivateKey(decode64(encoded))
    privateKey.bits === 2048
  }

  "write PublicKey with helper" >> {
    val encoded = "JgAcAQABvIEAxIA1enr/js+6wCO5AfidA7cxOAGF6un36HGCAlv8WiUwkZV8kUsN4KdCmEcjfDr6WBdnxfftD8uv3LbT3LzTo4rpMv0EkBVfC3s/HnK/kLrPyrnt3cycZdbF6P939DMFAyiUGmD1HFFlIfPCtGVszlIU4aRvPNLRfuSno52Iqe28gQCoPVqU6HYVl6RPbMxM0MwQr6mUNOOEh4TvxHdTMw6O1Qv+wTPu0bHocV6CcLx132FEKoVfcLrTj/bkjNiTpwSzkIlQIWhokljvhSD9ILniAkOgtbKGgDuQ2ikrcPHSSe+uRJXGlBkyxDVKR5i+6ZVQunJk4vdhfwRLOk2KYgEyIQ=="
    val privateKey = new PrivateKey(decode64(encoded))
    val publicKey = privateKey.publicKey

    val encodedPub = Boss.dump(publicKey)

    encode64(encodedPub) == "FzNwYWNrZWTECQEeCBwBAAHEAAGBIzssfkHSzN4PKtznvxc+M341Xw+2jxiYcVEvWEc/Cg7Jb6f4uC5n10wFfOQqUUeL8XOsORx7Wck6jheFZSx9hwuaohdpK5Dv160ObeOtN26A0XnMT15f6jfjfJGH9faPIu0ASUcqWk+ZRSXYXTFJ1/ytYxSCZWf85CKIHu4yh31sXFqdZXcpFpyqSfezoXeoRQR9/vgD/uuj2IsIS5I9e44suSwJl/ODVs2sw5KOwc2GsK/XyUrx3SthVrfKWIfjFnRUHX8eRN9hKX7kEjS3pL6rAClzdHcjkqu/BfsyjvbXAzIr/xkJORH6gmrZEbMCHj6MpYyo40OjYjNjuzGNM19fdHlwZWNSU0FQdWJsaWNLZXk="
  }

  "write PublicKey with Boss" >> {
    val encoded = "JgAcAQABvIEAxIA1enr/js+6wCO5AfidA7cxOAGF6un36HGCAlv8WiUwkZV8kUsN4KdCmEcjfDr6WBdnxfftD8uv3LbT3LzTo4rpMv0EkBVfC3s/HnK/kLrPyrnt3cycZdbF6P939DMFAyiUGmD1HFFlIfPCtGVszlIU4aRvPNLRfuSno52Iqe28gQCoPVqU6HYVl6RPbMxM0MwQr6mUNOOEh4TvxHdTMw6O1Qv+wTPu0bHocV6CcLx132FEKoVfcLrTj/bkjNiTpwSzkIlQIWhokljvhSD9ILniAkOgtbKGgDuQ2ikrcPHSSe+uRJXGlBkyxDVKR5i+6ZVQunJk4vdhfwRLOk2KYgEyIQ=="
    val privateKey = new PrivateKey(decode64(encoded))
    val publicKey = privateKey.publicKey

    encode64(Boss.dump(publicKey)) == "FzNwYWNrZWTECQEeCBwBAAHEAAGBIzssfkHSzN4PKtznvxc+M341Xw+2jxiYcVEvWEc/Cg7Jb6f4uC5n10wFfOQqUUeL8XOsORx7Wck6jheFZSx9hwuaohdpK5Dv160ObeOtN26A0XnMT15f6jfjfJGH9faPIu0ASUcqWk+ZRSXYXTFJ1/ytYxSCZWf85CKIHu4yh31sXFqdZXcpFpyqSfezoXeoRQR9/vgD/uuj2IsIS5I9e44suSwJl/ODVs2sw5KOwc2GsK/XyUrx3SthVrfKWIfjFnRUHX8eRN9hKX7kEjS3pL6rAClzdHcjkqu/BfsyjvbXAzIr/xkJORH6gmrZEbMCHj6MpYyo40OjYjNjuzGNM19fdHlwZWNSU0FQdWJsaWNLZXk="
  }

  "get fingerprint" >> {
    val encoded = "JgAcAQABvIEAxIA1enr/js+6wCO5AfidA7cxOAGF6un36HGCAlv8WiUwkZV8kUsN4KdCmEcjfDr6WBdnxfftD8uv3LbT3LzTo4rpMv0EkBVfC3s/HnK/kLrPyrnt3cycZdbF6P939DMFAyiUGmD1HFFlIfPCtGVszlIU4aRvPNLRfuSno52Iqe28gQCoPVqU6HYVl6RPbMxM0MwQr6mUNOOEh4TvxHdTMw6O1Qv+wTPu0bHocV6CcLx132FEKoVfcLrTj/bkjNiTpwSzkIlQIWhokljvhSD9ILniAkOgtbKGgDuQ2ikrcPHSSe+uRJXGlBkyxDVKR5i+6ZVQunJk4vdhfwRLOk2KYgEyIQ=="
    val privateKey = new PrivateKey(decode64(encoded))
    val publicKey = privateKey.publicKey
    encode64(publicKey.fingerprint) == "B5vMBq43yqq8lA5Pd7nCzA15BKjGGoaDLu5GHEcOAWnm"
  }

  "get short address by private key" >> {
    val encoded = "JgAcAQABvIEAxIA1enr/js+6wCO5AfidA7cxOAGF6un36HGCAlv8WiUwkZV8kUsN4KdCmEcjfDr6WBdnxfftD8uv3LbT3LzTo4rpMv0EkBVfC3s/HnK/kLrPyrnt3cycZdbF6P939DMFAyiUGmD1HFFlIfPCtGVszlIU4aRvPNLRfuSno52Iqe28gQCoPVqU6HYVl6RPbMxM0MwQr6mUNOOEh4TvxHdTMw6O1Qv+wTPu0bHocV6CcLx132FEKoVfcLrTj/bkjNiTpwSzkIlQIWhokljvhSD9ILniAkOgtbKGgDuQ2ikrcPHSSe+uRJXGlBkyxDVKR5i+6ZVQunJk4vdhfwRLOk2KYgEyIQ=="
    val privateKey = new PrivateKey(decode64(encoded))

    println(encode58(privateKey.shortAddress))

    encode58(privateKey.shortAddress) == "ZW7iu3eea1SQpxpncDgemUisT5uiqUeCqcu8HdFzmmt6QV8bYs"
  }

  "read PublicKey with Boss" >> {
    val encoded = "FzNwYWNrZWTECgEeCBwBAAHEAQEAgSM7LH5B0szeDyrc578XPjN+NV8Pto8YmHFRL1hHPwoOyW+n+LguZ9dMBXzkKlFHi/FzrDkce1nJOo4XhWUsfYcLmqIXaSuQ79etDm3jrTdugNF5zE9eX+o343yRh/X2jyLtAElHKlpPmUUl2F0xSdf8rWMUgmVn/OQiiB7uMod9bFxanWV3KRacqkn3s6F3qEUEff74A/7ro9iLCEuSPXuOLLksCZfzg1bNrMOSjsHNhrCv18lK8d0rYVa3yliH4xZ0VB1/HkTfYSl+5BI0t6S+qwApc3R3I5KrvwX7Mo721wMyK/8ZCTkR+oJq2RGzAh4+jKWMqONDo2IzY7sxjTNfX3R5cGVjUlNBUHVibGljS2V5"
    val loaded = Boss.load(decode64(encoded))
    val publicKey = Boss.load(decode64(encoded)).asInstanceOf[PublicKey]

    encode64(publicKey.fingerprint) == "B5vMBq43yqq8lA5Pd7nCzA15BKjGGoaDLu5GHEcOAWnm"
  }
}
