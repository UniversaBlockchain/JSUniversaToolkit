dec64 = Universa.utils.decode64
enc64 = Universa.utils.encode64

Unitest.addTestModule ->

   Unitest.addTest 'import contact test', ->

        TEST_APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
        privKey = dec64 '''JgAcAQABxAEBAOsjhh8HIil0hBrXLwsro+OaOkV4F38vjMGGPiTqgSVDpg0VW1smBEcH8YkmZ8V0gwCkqAjfk0HzBAJLJ5mGpVNgrtu1cqAYnslZYEaEbTrSAKTlBJRb4vZbB8dxkO1cbq07G3eSd9bw6bw+rpf589PJNO0UxlyFKuSV+ybRqTGGGm1RK4weqlVFBCyVpl9mFeJ2BrlR/3KC8zF6FMrjQGcqGaSNiljkVI3yw9Olz7RCzaj1fXyf8XmAwhSpRiRMv8n3CTRpnfBLGUuKeI4yt5bg7y+9lINpM3u9yf+/1GP7g7gj0FUlEu0pYBpPljaT7FXlMHynA4jUuvFlli/y7c/EAQEAx0QToND4exRXju5Rcg1HIgJji0e+uT94h3JhDnkwQun3zp0W4LM0jMNykjblBKcr7pEmacIdJNzgC/zOZudbnIVMxlulbeGGScNF9srCPbCZp7X60BNaeDqDR/IYSPm6Qympl/5aGHq7kJDbt82doj/WTi8RCbxUY4CbAe9e16B/Me0MSLniZJmprj9Vz7NgnxGNNsPH1TtFJQbHd20dtyfSCcoVV054WyoQC7KFgpEEH7r733fb/eJyzJDv+zkPmF/JkdX1JgrjVSAGrPOqvcc+K4zmjJ3v9gJMEAPIGwK8xK/f8FtAsGaEphSUzlyQFvh06FrbWc8LWuL944Inew=='''

        chatleApi = await CryptoCloud.connect(TEST_APP_TOKEN, privKey)

        #contacts = await chatleApi.loadItems({ tags: ['contact'], limit: 5 })
        #console.log(contacts)

        res = await Universa.Import.Contacts(chatleApi)
        console.log(res)

  Unitest.addTest 'import notary with bytestring hash id of file', ->
    APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
    az1key = dec64 'JgAcAQABvIEA+Ih4oBmVFmVOb7lwUwQC3CuQBrVlNOMINm86z0XpveQYHPO5LAcHFXND8pJh/EEzeSaRpgFU6sHBbBnSoL1P44vG/cfvlxRZIK5bBqajcQKvdeNeFmZ+olfHmRBg9f6tVRBtwQs2XLZbgD9MZbaymgChbUZdjqCmSiL1iIUswFe8gQDqqQ9ICWHVx9LaaZzCEE67eUBOuPdTQAFUTg/BijhJebCuaOjFj3v+4HWk6Sfzi/YtqROGm76cslfgs9uXuXEVLkFB3MWTBTCPwLtLSDrK4pLc3xG1GybLcPbXJgwfR9yvN0y1YLR63RO+HLTEYkfYp1xsEYtQPT9dv3cXqCQuZQ=='

    chatleApi = await CryptoCloud.connect(APP_TOKEN, az1key)
    res = await Universa.Import.Contracts(chatleApi)
    console.log(res)

   Unitest.addTest 'import contract test', ->

       TEST_APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
       privKey = dec64 '''JgAcAQABxAEBAOsjhh8HIil0hBrXLwsro+OaOkV4F38vjMGGPiTqgSVDpg0VW1smBEcH8YkmZ8V0gwCkqAjfk0HzBAJLJ5mGpVNgrtu1cqAYnslZYEaEbTrSAKTlBJRb4vZbB8dxkO1cbq07G3eSd9bw6bw+rpf589PJNO0UxlyFKuSV+ybRqTGGGm1RK4weqlVFBCyVpl9mFeJ2BrlR/3KC8zF6FMrjQGcqGaSNiljkVI3yw9Olz7RCzaj1fXyf8XmAwhSpRiRMv8n3CTRpnfBLGUuKeI4yt5bg7y+9lINpM3u9yf+/1GP7g7gj0FUlEu0pYBpPljaT7FXlMHynA4jUuvFlli/y7c/EAQEAx0QToND4exRXju5Rcg1HIgJji0e+uT94h3JhDnkwQun3zp0W4LM0jMNykjblBKcr7pEmacIdJNzgC/zOZudbnIVMxlulbeGGScNF9srCPbCZp7X60BNaeDqDR/IYSPm6Qympl/5aGHq7kJDbt82doj/WTi8RCbxUY4CbAe9e16B/Me0MSLniZJmprj9Vz7NgnxGNNsPH1TtFJQbHd20dtyfSCcoVV054WyoQC7KFgpEEH7r733fb/eJyzJDv+zkPmF/JkdX1JgrjVSAGrPOqvcc+K4zmjJ3v9gJMEAPIGwK8xK/f8FtAsGaEphSUzlyQFvh06FrbWc8LWuL944Inew=='''

       chatleApi = await CryptoCloud.connect(TEST_APP_TOKEN, privKey)

       #contracts = await chatleApi.loadItems({ tags: ['contract'], limit: 5 })
       #console.log(contracts)

       res = await Universa.Import.Contracts(chatleApi)
       console.log(res)


  #Unitest.addTest 'import contract from someone account test', ->

      #TEST_APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
      #privKey = dec64 '''JgAcAQABxAEBAOsjhh8HIil0hBrXLwsro+OaOkV4F38vjMGGPiTqgSVDpg0VW1smBEcH8YkmZ8V0gwCkqAjfk0HzBAJLJ5mGpVNgrtu1cqAYnslZYEaEbTrSAKTlBJRb4vZbB8dxkO1cbq07G3eSd9bw6bw+rpf589PJNO0UxlyFKuSV+ybRqTGGGm1RK4weqlVFBCyVpl9mFeJ2BrlR/3KC8zF6FMrjQGcqGaSNiljkVI3yw9Olz7RCzaj1fXyf8XmAwhSpRiRMv8n3CTRpnfBLGUuKeI4yt5bg7y+9lINpM3u9yf+/1GP7g7gj0FUlEu0pYBpPljaT7FXlMHynA4jUuvFlli/y7c/EAQEAx0QToND4exRXju5Rcg1HIgJji0e+uT94h3JhDnkwQun3zp0W4LM0jMNykjblBKcr7pEmacIdJNzgC/zOZudbnIVMxlulbeGGScNF9srCPbCZp7X60BNaeDqDR/IYSPm6Qympl/5aGHq7kJDbt82doj/WTi8RCbxUY4CbAe9e16B/Me0MSLniZJmprj9Vz7NgnxGNNsPH1TtFJQbHd20dtyfSCcoVV054WyoQC7KFgpEEH7r733fb/eJyzJDv+zkPmF/JkdX1JgrjVSAGrPOqvcc+K4zmjJ3v9gJMEAPIGwK8xK/f8FtAsGaEphSUzlyQFvh06FrbWc8LWuL944Inew=='''

      #chatleApi = await CryptoCloud.connectWithPassword(TEST_APP_TOKEN, "alice", "P@55w0rd")

      #contracts = await chatleApi.loadItems({ tags: ['contract'], limit: 5 })
      #console.log(contracts)

      #res = await Universa.Import.Contracts(chatleApi)
      #console.log(res)
      #qw = res[3].contract.toJsObject()
      #wq = Universa.Contract.fromJsObject(qw)
      #console.log(wq)

   Unitest.addTest 'import pair test', ->

       TEST_APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="
       privKey = dec64 '''JgAcAQABxAEBAOsjhh8HIil0hBrXLwsro+OaOkV4F38vjMGGPiTqgSVDpg0VW1smBEcH8YkmZ8V0gwCkqAjfk0HzBAJLJ5mGpVNgrtu1cqAYnslZYEaEbTrSAKTlBJRb4vZbB8dxkO1cbq07G3eSd9bw6bw+rpf589PJNO0UxlyFKuSV+ybRqTGGGm1RK4weqlVFBCyVpl9mFeJ2BrlR/3KC8zF6FMrjQGcqGaSNiljkVI3yw9Olz7RCzaj1fXyf8XmAwhSpRiRMv8n3CTRpnfBLGUuKeI4yt5bg7y+9lINpM3u9yf+/1GP7g7gj0FUlEu0pYBpPljaT7FXlMHynA4jUuvFlli/y7c/EAQEAx0QToND4exRXju5Rcg1HIgJji0e+uT94h3JhDnkwQun3zp0W4LM0jMNykjblBKcr7pEmacIdJNzgC/zOZudbnIVMxlulbeGGScNF9srCPbCZp7X60BNaeDqDR/IYSPm6Qympl/5aGHq7kJDbt82doj/WTi8RCbxUY4CbAe9e16B/Me0MSLniZJmprj9Vz7NgnxGNNsPH1TtFJQbHd20dtyfSCcoVV054WyoQC7KFgpEEH7r733fb/eJyzJDv+zkPmF/JkdX1JgrjVSAGrPOqvcc+K4zmjJ3v9gJMEAPIGwK8xK/f8FtAsGaEphSUzlyQFvh06FrbWc8LWuL944Inew=='''

       chatleApi = await CryptoCloud.connect(TEST_APP_TOKEN, privKey)

       #pairs = await chatleApi.loadItems({ tags: ['pair'], limit: 5 })
       #console.log(pairs)

       res = await Universa.Import.Pairs(chatleApi)
       console.log(res)
