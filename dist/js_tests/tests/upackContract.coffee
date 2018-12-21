test = window.unitjs || {}

dec64 = Universa.utils.v2.decode64
enc64 = Universa.utils.v2.encode64
Contract = Universa.Contract

Unitest.addTestModule ->
  CONTRACT_KEY = dec64 """
    JgAcAQABvIEAxAgalqIUgnCbe3Cc34xSBkjIl7bbE6TZSDj+rmgM8yjP7UbpLbB85aY2ZxH0OItYwZQH/cawVJGAkTcxTH1A5A7l6P/4o97yP4Qkb7XYpDlwXhvWlHTpvODrI8eT5KIBW+JFHSmHk3NSCQg6XoA7X0+t0vZXr3aMvP4ACw2R8gW8gQC93hQfbkJaNMAJZ+wQMQIVr6Amy1Me4H6v83wDv4gH7TfgGJlJj5MY3HwHtXO5Ac0vcPxVlRgX0VUe5jM1kfB3Q+nV5md4O/oZZMs0NKxusShPckEE5tQnHqt3UNcMw8LL7UVWfCFbmB535hD8RbxODYdnz+R2JDHUEYL8ZlmLRQ==
  """

  CONTRACT_BIN = dec64 """
    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
  """

  U_CONTRACT_BIN = dec64 """
    JyNkYXRhxC0GHxtuZXcGQ3Jldm9raW5nDhczX190eXBlM0hhc2hJZFNjb21wb3NpdGUzvGD9tVYDOBLZXut+OXcpO20+EySr2wFDwjOncTtTUVdT/tTcsKqNybVcMf3YX5uMx7iqw0ZmV6ZfGBMv+zgQtB2qCY4hJlm0ICpHwR/CbXQ6iljYY1ZHRNS61SdUlgeLsQpDY29udHJhY3QvS2FwaV9sZXZlbBg9g1VuaXZlcnNhQ29udHJhY3RTZGVmaW5pdGlvbjcjZGF0YRdTaXNzdWVyTmFtZbsXVW5pdmVyc2EgUmVzZXJ2ZSBTeXN0ZW0jbmFtZbN0cmFuc2FjdGlvbiB1bml0cyBwYWNrU3JlZmVyZW5jZXMdW3Blcm1pc3Npb25zJzNKdG9qUHg3raNkZWNyZW1lbnRfcGVybWlzc2lvbkttaW5fdmFsdWUAI3JvbGUfW3RhcmdldF9uYW1lK293bmVyPUNSb2xlTGlua60zb3duZXIyQ21heF9zdGVwClNmaWVsZF9uYW1ls3Rlc3RfdHJhbnNhY3Rpb25fdW5pdHM9u0Bjb20uaWNvZGljaS51bml2ZXJzYS5jb250cmFjdC5wZXJtaXNzaW9ucy5DaGFuZ2VOdW1iZXJQZXJtaXNzaW9uM3BXNDUzcTetvRy9HQC9Hr0fvSQKvSWLdHJhbnNhY3Rpb25fdW5pdHM9vSczbGFVbGFrH70eL0thZGRyZXNzZXMdI2tleXMOFxtrZXkXPWNSU0FQdWJsaWNLZXkzcGFja2VkxAkBHggcAQABxAABxSSWfXW20wGsRn9khVZDtvcCtUqP/scN3oVPU3r0152Fu62pfx9Mjc1cmQnRYSkeZzWA50RYQTU3FlXC5iIN7w+Lm6TGPQxWe+uYGMgKLCbAmyMXPWupvzeB5SEMtylQ5ml12iwFQkamqOiFE2KWMYz/UGhW87/ELPckmpoanZUa8OGCACUfFGALAZV0G+rQ/8xiW3hkGHmOFP0kejZaCZGPO/XGVay+2q0V2cw6CHar+D9F9FomXYA4bAInlY3zOLHdG8ddUTzhHQWOKmzoF9eIW67U9rd9qIR04U9ls9wGLQchqlG/kxHHfR4Js86mwYNgUKW49fQRppig+SsrjT1LS2V5UmVjb3JkPVNTaW1wbGVSb2xlrTNpc3N1ZXI7YW5vbklkcx09q0NoYW5nZU93bmVyUGVybWlzc2lvbq1jY2hhbmdlX293bmVyM1RiakZPRy+tvRy9HQC9Hh+9IL05Pb0irTtpc3N1ZXIyvSW9Jj29J1NjcmVhdGVkX2F0eR03IleFvTm9LStyb2xlcx0rc3RhdGVfvSEvvS4dvS8OF70yFz29NL01xAkBHggcAQABxAABh78/u0UMqu7hxOzj0IawiH/8u95EEGErTKuQYLpude5qjEWouwK0r7dy/gtzAv6oS/W+7s3v5i9NdLsOEmbX7vnqd0+odV02bcQj2mrmdsGYH3b9pLeEPwvkTCxh/t7iqjX+CAwCeaYTpUCyXUfpRbCCd0S5JmtgFDhNqc5MW5wHw9X/xRca54vWjcbeUbsP18rCSQfrf13zJ8QxaTYZataiGca7p16smZA5LXwgbFB7uV5SitJ0Tq/Tuwv3mFzM+OTq3RuL7Nhc9x2PhLJO9SwAdDtti5NinmWDZl7QVWjdd8ZdlAlloxKTZLWMIK1Z4PwLyOhJc9ILYS6ogXuN/T29Nz29OK29Ib06HTNwYXJlbnQ1U2V4cGlyZXNfYXR5HR4nZoWNF70quDa9JsiZhgG9Fx1LYnJhbmNoX2lkBTNvcmlnaW4XPUVNvGD4iaZmW8v6PEuUxHKv/UEjnGknHWWqDrsLXzvBFZVoEN/O80fsHumBvGjQSJ4yvZJPQwLMdtSiXHBMT34RApxO6cCE0yNK5uaOUImY88KlRQHNOSK2mp41MM0GtPBbyBK9QXlybShXhVNjcmVhdGVkX2J5Hz29OK07Y3JlYXRvcr0uDhc9U0tleUFkZHJlc3NDdWFkZHJlc3O8JRCUuKrDe2VkjFZfPL8ANdiRO2jg+ylqKjtdj02jWU6eFKVKcM1DcmV2aXNpb25ovUIda3RyYW5zYWN0aW9uYWwFI3R5cGVTdW5pY2Fwc3VsZTt2ZXJzaW9uGFNzaWduYXR1cmVzDsSoAhcjZXh0c8SXAScba2V5vCEHDjGDk1tq6hLXQgGtR6ZKDdFqZMB3RlPBTpZnLWyIwKMzc2hhNTEyvEAOqMTyDK+Bd5BnV7m16DkvpEWmch3A//gp5k5x8vQCTwj5W8Pz6HU5AmKHUKu+rti0EpMjFP0y0MEzv9bGo4sGU2NyZWF0ZWRfYXR5cm0oV4U7cHViX2tlecQKAR4IHAEAAcQBAQCHvz+7RQyq7uHE7OPQhrCIf/y73kQQYStMq5Bgum517mqMRai7ArSvt3L+C3MC/qhL9b7uze/mL010uw4SZtfu+ep3T6h1XTZtxCPaauZ2wZgfdv2kt4Q/C+RMLGH+3uKqNf4IDAJ5phOlQLJdR+lFsIJ3RLkma2AUOE2pzkxbnAfD1f/FFxrni9aNxt5Ruw/XysJJB+t/XfMnxDFpNhlq1qIZxrunXqyZkDktfCBsUHu5XlKK0nROr9O7C/eYXMz45OrdG4vs2Fz3HY+Esk71LAB0O22Lk2KeZYNmXtBVaN13xl2UCWWjEpNktYwgrVng/AvI6Elz0gthLqiBe439I3NpZ27EAAFvgK7CPwYtGUyU9JlmpLGZgu93rMbdJyn0Jm1f/spTYHuF4E2xtxuBg1y2xnaUs31XP+g2xy4NNiKTa5g8dmw/7HQXbiFCNXJ87WjyNu7tHobZItzdOm8zyKb5z3oiyfAOaQPqJ28n6AxxfyVqxRMfpNlwJ9VRy1suKRJrDVdQlOOhfADt+1dz1wbrx1AnTz7UmhhTaOV7lS+ItxmQ9UJbxMOpduM9cu/pkAS91+Z538E/vAUHgNIkA1gOuKVg6kMgYIAN4+yN7N7RqvIKFwqyLBWB9dQTLWFGvtPD58/SvUUWKR3I1jCEcY/dnPyk/KJR1F5cWDWDNzXQiArNQlgS
  """

  U_KEY = dec64 """
    JgAcAQABvIEAvLMxfM+DoEsA8dAJhe+k5RsESPAH+HM++ike3BOJQD+RwXBzRJ3i7FpkJy0m1Q+SNPBBpC9G08O41mJAaVHzNntIBiId0y6dJkQVb5FgdGFX0fi3ZmQo/e3DNErbmBqA30vPL6bhg9cN/Jel4QSKr7hFBENdFEgomiiSa19yp+28gQC4KVCrzd7cUlksW3eq8X2hjQ/+BsRRfEm/00S9Wjj73tXKKLQxbzgniSiusMgEqwPiH/ijPltncChOXd9bqdN7F2YwkCUY94OXMfRUvVKP6rp+L3IwcQdx5iLnEj/7MCGdspkND8hBmIgkIF3FoJg+l2mY/t+9Tfib5R+Tc2CcUQ==
  """

  SOME_PRIVATE_KEY_2 = dec64 'JgAcAQABvIEA96FWTEq/Wjmd7kYbx04I/Ax3OQV+6e4YWu7xBr8k/SexvYvFEE3c9dRsZSsEj7KzYrNpIXezCsxO+j1sHADmeojwuveDdQQM6M6fbvygHq/mxKGllDoJTpzX/CnkuXDwC+lpRkkMTIF48GaYDM525951HYW1pAIYaVr+V5ctVQW8gQDGM71OC1yBLv6n23dEuu9Vqna2lvDpEWNO7MgKY0Ri9vUPwp+F+dUiKsRTbjnukDFqiYiNj+jkcWgiXqnjqdAGf9LUWHfF80W1PwUhkFw7torLJfaAr6bZ6FRzGcxTMad0x7Rz+5lkBjliKqOt8AUAIvMVe45G0c0StJSoqdqc4Q=='

  Unitest.addTest 'pack/unpack U', ->
    privKey = Universa.PrivateKey.fromBOSS(U_KEY)
    pubKey = privKey.publicKey

    contract = Contract.fromBOSS(U_CONTRACT_BIN)
    contract.temp.lockData
    contract.temp.lock
    contract2 = Contract.fromBOSS(contract.temp.currentBinary)
    balance1 = contract.getBalance()
    balance2 = contract2.getBalance()

    # main balance
    #console.log(contract.getBalance(false))
    # test balance
    #console.log(contract.getBalance(true))

    @assertEquals balance1, balance2

  Unitest.addTest 'contract: registerPaid: failed by balance', ->
    contract = Contract.fromBOSS(U_CONTRACT_BIN)
    privKey = Universa.PrivateKey.fromBOSS(U_KEY)
    otherPrivKey = Universa.PrivateKey.fromBOSS(SOME_PRIVATE_KEY_2)
    contract.temp.sign(privKey)

    upack = Contract.fromBOSS(U_CONTRACT_BIN)
    api = await Node.connect(Universa.PrivateKey.fromBOSS(CONTRACT_KEY))

    @assertThrows Universa.BalanceError, ->
        await contract.createParcel(upack, 999930000, [otherPrivKey], true, true)

  Unitest.addTest 'contract: getParcel: failed by key', ->
      contract = Contract.fromBOSS(U_CONTRACT_BIN)
      privKey = Universa.PrivateKey.fromBOSS(U_KEY)
      otherPrivKey = Universa.PrivateKey.fromBOSS(SOME_PRIVATE_KEY_2)
      contract.temp.sign(privKey)

      upack = Contract.fromBOSS(U_CONTRACT_BIN)

      @assertThrows Universa.NoKeyFound, ->
              await contract.createParcel(upack, 1, [otherPrivKey], true)

  Unitest.addTest 'contract: getParcel: failed by creator role', ->
        contract = Contract.fromBOSS(U_CONTRACT_BIN)
        privKey = Universa.PrivateKey.fromBOSS(U_KEY)
        otherPrivKey = Universa.PrivateKey.fromBOSS(SOME_PRIVATE_KEY_2)
        contract.temp.sign(privKey)

        upack = Contract.fromBOSS(U_CONTRACT_BIN)

        contract.setState("created_by", null)
        @assertThrows Universa.NoRoleFound, ->
            await contract.createParcel(upack, 1, [otherPrivKey], true)

  Unitest.addTest 'contract: getParcel: failed by key', ->
          contract = Contract.fromBOSS(U_CONTRACT_BIN)
          privKey = Universa.PrivateKey.fromBOSS(U_KEY)
          contract.temp.sign(privKey)

          upack = Contract.fromBOSS(U_CONTRACT_BIN)

          @assertThrows Universa.NoKeyError, ->
              await contract.createParcel(upack, 1, [], true)

  #   Unitest.addTest 'contract: registerPaid: multi keys', ->
  #         contract = Contract.fromBOSS(U_CONTRACT_BIN)
  #         privKey = Universa.PrivateKey.fromBOSS(U_KEY)
  #         otherPrivKey = Universa.PrivateKey.fromBOSS(SOME_PRIVATE_KEY_2)
  #         contract.temp.sign(privKey)

  #         upack = Contract.fromBOSS(U_CONTRACT_BIN)
  #         api = await Node.connect(Universa.PrivateKey.fromBOSS(CONTRACT_KEY))
  #         parcel = await contract.createParcel(upack, [privKey, otherPrivKey], true)
  #         response = await parcel.register(api)
  #         @assertEquals response.result, true

  # Unitest.addTest 'contract: registerPaid: single key', ->
  #    contract = Contract.fromBOSS(U_CONTRACT_BIN)
  #    privKey = Universa.PrivateKey.fromBOSS(U_KEY)
  #    contract.temp.sign(privKey)

  #    upack = Contract.fromBOSS(U_CONTRACT_BIN)
  #    api = await Node.connect(Universa.PrivateKey.fromBOSS(CONTRACT_KEY))
  #    parcel = await contract.createParcel(upack, [privKey], true)
  #    response = await parcel.register(api)

  #    console.log(response)
  #    @assertEquals response.result, true