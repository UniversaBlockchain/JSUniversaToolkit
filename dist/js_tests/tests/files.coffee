dec64 = Universa.utils.decode64
enc64 = Universa.utils.encode64

Unitest.addTestModule ->
  APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
  PACKED_PRIVATE_KEY_1 = dec64 'JgAcAQABvIEA/lcxRgJEfLoN0TqJkN6m/q+qiV3wGn3k53EEyIhVwqpWU9XntSCfPfIcTpfGrd2RvUZFEn1eW1Uc4XkDw5DZ++k2+lvbL5udllpbDtG9bSjG8Y0SruiwOUXijQzW/esWhi4b6OCCAcXCgIDDh5607JWAyS+XEK38V3MqfqfHbd+8gQDPVmtdpdownVM95HWwW/SLzy+XdHJA46pd8ROcnMnHJNjYREjFYeWzWSZJ4BviOXkdni82l8sH+szEcMeXT9m5Dl86L7vAkoUPT06OcUKc4aKB5ZtczMy2O4fCU+w1nmW+sEB/LgXleri2KBkDBwXEivvWXj2WCd8r4CRa+fhamQ=='
  PACKED_PRIVATE_KEY_2 = dec64 'JgAcAQABvIEA96FWTEq/Wjmd7kYbx04I/Ax3OQV+6e4YWu7xBr8k/SexvYvFEE3c9dRsZSsEj7KzYrNpIXezCsxO+j1sHADmeojwuveDdQQM6M6fbvygHq/mxKGllDoJTpzX/CnkuXDwC+lpRkkMTIF48GaYDM525951HYW1pAIYaVr+V5ctVQW8gQDGM71OC1yBLv6n23dEuu9Vqna2lvDpEWNO7MgKY0Ri9vUPwp+F+dUiKsRTbjnukDFqiYiNj+jkcWgiXqnjqdAGf9LUWHfF80W1PwUhkFw7torLJfaAr6bZ6FRzGcxTMad0x7Rz+5lkBjliKqOt8AUAIvMVe45G0c0StJSoqdqc4Q=='


  Unitest.addTest 'zip and unzip single file', ->
      api = await CryptoCloud.connect APP_TOKEN, PACKED_PRIVATE_KEY_1

      filecontent = "Hello World";
      data = Universa.utils.v2.asByteArray(filecontent)

      file = FileInfo(data, "hello.txt")
      archive = Universa.Archive.FromFiles(Array(file))
      id = await archive.ToCloud(api)

      console.log 'item created!'
      console.log id
      @assert id > 0

      unzipFiles = await Universa.Archive.FromCloud(id, api)
      console.log unzipFiles
      @assertEquals unzipFiles.fileInfos[0].name, 'hello.txt'
      @assertEquals String.fromCharCode.apply(null, unzipFiles.fileInfos[0].content), filecontent


  Unitest.addTest 'zip and unzip multi files', ->
        api = await CryptoCloud.connect APP_TOKEN, PACKED_PRIVATE_KEY_1

        filecontent = "Hello World222";
        data = Universa.utils.v2.asByteArray(filecontent)

        file1 = FileInfo(data, "hello.txt")
        file2 = FileInfo(data, "hello2.txt")
        archive = Universa.Archive.FromFiles(Array(file1, file2))
        id = await archive.ToCloud(api)

        console.log 'item created!'
        console.log id
        @assert id > 0

        unzipFiles = await Universa.Archive.FromCloud(id, api)
        console.log unzipFiles
        @assertEquals unzipFiles.fileInfos[0].name, 'hello.txt'
        @assertEquals String.fromCharCode.apply(null, unzipFiles.fileInfos[0].content), filecontent
        @assertEquals unzipFiles.fileInfos[1].name, 'hello2.txt'
        @assertEquals String.fromCharCode.apply(null, unzipFiles.fileInfos[1].content), filecontent
