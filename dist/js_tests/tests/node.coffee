dec64 = Universa.utils.decode64
enc64 = Universa.utils.encode64

Unitest.addTestModule ->
  PACKED_PRIVATE_KEY_2 = dec64 'JgAcAQABvIEA96FWTEq/Wjmd7kYbx04I/Ax3OQV+6e4YWu7xBr8k/SexvYvFEE3c9dRsZSsEj7KzYrNpIXezCsxO+j1sHADmeojwuveDdQQM6M6fbvygHq/mxKGllDoJTpzX/CnkuXDwC+lpRkkMTIF48GaYDM525951HYW1pAIYaVr+V5ctVQW8gQDGM71OC1yBLv6n23dEuu9Vqna2lvDpEWNO7MgKY0Ri9vUPwp+F+dUiKsRTbjnukDFqiYiNj+jkcWgiXqnjqdAGf9LUWHfF80W1PwUhkFw7torLJfaAr6bZ6FRzGcxTMad0x7Rz+5lkBjliKqOt8AUAIvMVe45G0c0StJSoqdqc4Q=='
  CONTRACT_KEY = dec64 """
    JgAcAQABvIEAxAgalqIUgnCbe3Cc34xSBkjIl7bbE6TZSDj+rmgM8yjP7UbpLbB85aY2ZxH0OItYwZQH/cawVJGAkTcxTH1A5A7l6P/4o97yP4Qkb7XYpDlwXhvWlHTpvODrI8eT5KIBW+JFHSmHk3NSCQg6XoA7X0+t0vZXr3aMvP4ACw2R8gW8gQC93hQfbkJaNMAJZ+wQMQIVr6Amy1Me4H6v83wDv4gH7TfgGJlJj5MY3HwHtXO5Ac0vcPxVlRgX0VUe5jM1kfB3Q+nV5md4O/oZZMs0NKxusShPckEE5tQnHqt3UNcMw8LL7UVWfCFbmB535hD8RbxODYdnz+R2JDHUEYL8ZlmLRQ==
  """

  Unitest.addTest 'connect to node', ->
    conn = await Node.connect(Universa.PrivateKey.fromBOSS(CONTRACT_KEY), true)
    res = await conn.command 'sping'
    @assertEquals 'spong', res.sping

  Unitest.addTest 'check contract', ->
    conn = await Node.connect(Universa.PrivateKey.fromBOSS(CONTRACT_KEY))
    contractId = dec64 "0oN1Wpaf1KqdwV7M/wnIzeslAT12C7kD4Gi5ItQEoZkKqjq0O3PfCQmhaf+0Df1RgmYpbonjI7Yu0ydFdHFaRToZHaTOtR09cy8/dqbiLhOUpGYYpUJEpC/fcRQphI0r"
    res = await conn.checkContract(contractId)
    @assertEquals res.itemResult.state, 'APPROVED'
