test = window.unitjs || {}

dec64 = Universa.utils.v2.decode64
enc64 = Universa.utils.v2.encode64
Contract = Universa.Contract

Unitest.addTestModule ->
      PACKED_PRIVATE_KEY_1 = dec64 'JgAcAQABvIEA/lcxRgJEfLoN0TqJkN6m/q+qiV3wGn3k53EEyIhVwqpWU9XntSCfPfIcTpfGrd2RvUZFEn1eW1Uc4XkDw5DZ++k2+lvbL5udllpbDtG9bSjG8Y0SruiwOUXijQzW/esWhi4b6OCCAcXCgIDDh5607JWAyS+XEK38V3MqfqfHbd+8gQDPVmtdpdownVM95HWwW/SLzy+XdHJA46pd8ROcnMnHJNjYREjFYeWzWSZJ4BviOXkdni82l8sH+szEcMeXT9m5Dl86L7vAkoUPT06OcUKc4aKB5ZtczMy2O4fCU+w1nmW+sEB/LgXleri2KBkDBwXEivvWXj2WCd8r4CRa+fhamQ=='
      PACKED_PRIVATE_KEY_2 = dec64 'JgAcAQABvIEA96FWTEq/Wjmd7kYbx04I/Ax3OQV+6e4YWu7xBr8k/SexvYvFEE3c9dRsZSsEj7KzYrNpIXezCsxO+j1sHADmeojwuveDdQQM6M6fbvygHq/mxKGllDoJTpzX/CnkuXDwC+lpRkkMTIF48GaYDM525951HYW1pAIYaVr+V5ctVQW8gQDGM71OC1yBLv6n23dEuu9Vqna2lvDpEWNO7MgKY0Ri9vUPwp+F+dUiKsRTbjnukDFqiYiNj+jkcWgiXqnjqdAGf9LUWHfF80W1PwUhkFw7torLJfaAr6bZ6FRzGcxTMad0x7Rz+5lkBjliKqOt8AUAIvMVe45G0c0StJSoqdqc4Q=='

      CONTRACT_KEY = dec64 """
        JgAcAQABvIEAxAgalqIUgnCbe3Cc34xSBkjIl7bbE6TZSDj+rmgM8yjP7UbpLbB85aY2ZxH0OItYwZQH/cawVJGAkTcxTH1A5A7l6P/4o97yP4Qkb7XYpDlwXhvWlHTpvODrI8eT5KIBW+JFHSmHk3NSCQg6XoA7X0+t0vZXr3aMvP4ACw2R8gW8gQC93hQfbkJaNMAJZ+wQMQIVr6Amy1Me4H6v83wDv4gH7TfgGJlJj5MY3HwHtXO5Ac0vcPxVlRgX0VUe5jM1kfB3Q+nV5md4O/oZZMs0NKxusShPckEE5tQnHqt3UNcMw8LL7UVWfCFbmB535hD8RbxODYdnz+R2JDHUEYL8ZlmLRQ==
      """

      APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="

      Unitest.addTest 'add files to contract', ->
          privKey = Universa.PrivateKey.fromBOSS(CONTRACT_KEY)
          pubKey = privKey.publicKey

          contract = Universa.NotaryContract.create(pubKey, false)

          api = await CryptoCloud.connect APP_TOKEN, PACKED_PRIVATE_KEY_1

          filecontent = "Hello World";
          data = Universa.utils.v2.asByteArray(filecontent)
          file1 = FileInfo(data, "hello.txt")

          filecontent2 = "Hello World222";
          data2 = Universa.utils.v2.asByteArray(filecontent2)
          file2 = FileInfo(data2, "hello2.txt")

          files = [file1, file2]

          @assertFalse contract.checkFilesInContract(files)

          cloudId = await contract.addFiles(files, api)
          @assertTrue cloudId > 0
          #contract.temp.getDefinition("data.files")

          files1 = [file1]
          files2 = [file1, file2]

          @assertFalse contract.checkFilesInContract(files1)
          @assertTrue contract.checkFilesInContract(files2)

          tempUuid = enc64 contract.temp.hashId()
          localContractIds = Universa.Contract.findById(tempUuid)

          #console.log(localContractIds)


      Unitest.addTest 'add and remove files', ->
            privKey = Universa.PrivateKey.fromBOSS(CONTRACT_KEY)
            pubKey = privKey.publicKey
            contract = Universa.NotaryContract.create(pubKey, false)

            api = await CryptoCloud.connect APP_TOKEN, PACKED_PRIVATE_KEY_1

            filecontent = "Hello World";
            data = Universa.utils.v2.asByteArray(filecontent)
            file1 = FileInfo(data, "hello.txt")

            filecontent2 = "Hello World222";
            data2 = Universa.utils.v2.asByteArray(filecontent2)
            file2 = FileInfo(data2, "hello2.txt")
            files = [file1, file2]

            cloudId1 = await contract.addFiles(files, api)
            @assertTrue cloudId1 > 0

            @assertTrue contract.checkFilesInContract(files)

            filecontent3 = "Hello World333";
            data3 = Universa.utils.v2.asByteArray(filecontent3)
            file3 = FileInfo(data3, "hello3.txt")

            cloudId2 = await contract.addFiles([file3], api)
            @assertTrue cloudId2 > cloudId1
            @assertFalse contract.checkFilesInContract(files)
            @assertTrue contract.checkFilesInContract([file1, file2, file3])

            #check remove files
            cloudId3 = await contract.removeFiles(["hello.txt"], api)
            @assertTrue cloudId3 > cloudId2
            @assertFalse contract.checkFilesInContract(files)
            @assertTrue contract.checkFilesInContract([file2, file3])