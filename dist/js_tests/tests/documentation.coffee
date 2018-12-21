Unitest.addTestModule ->

   Unitest.addTest 'docTest', ->

     byteArray = Universa.tools.decode64("abc="); # returns Uint8Array(2) [105, 183]
     b64Encoded = Universa.tools.encode64(byteArray); # returns "abc="

     checkType = byteArray instanceof Uint8Array

     @assertTrue checkType

     ########Pair
     privateKey = await Universa.PrivateKey.create(2048); # returns Promise[PrivateKey]
     publicKey = privateKey.publicKey;

     shortAddress = publicKey.shortAddress; # Uint8Array
     console.log(shortAddress)
     longAddress = publicKey.longAddress; # Uint8Array
     console.log(longAddress)

     keyAddress = byteArray
     isMatchingPublic = publicKey.isMatchingAddress(keyAddress)
     console.log(isMatchingPublic)
     isMatchingPrivate = privateKey.isMatchingAddress(keyAddress)
     console.log(isMatchingPrivate)

     privateKeyEncoded = privateKey.toBOSS
     console.log(privateKeyEncoded)
     publicKeyEncoded = publicKey.toBOSS
     console.log(publicKeyEncoded)

     privateKey = Universa.PrivateKey.fromBOSS(privateKeyEncoded)
     console.log(privateKey)
     publicKey = Universa.PublicKey.fromBOSS(publicKeyEncoded)
     console.log(publicKey)

     ########contract
     contract = Universa.Contract.create(publicKey)
     console.log(contract)
     tokenContract = Universa.Contract.createToken(publicKey, "10", 1, "tokenFullName", "tokenShortName", "description", false)
     console.log(tokenContract.contractName)
     console.log(tokenContract)

     contract.setState("state.path", "state.value")
     contract.setDefinition("def.path", "def.value")

     contract.temp.lockData()
     contract.temp.sign(privateKey)

     contract.createRevision()
     contract.setOwner(publicKey)

     keyAddress = publicKey.shortAddress
     contract.setOwner(keyAddress)


     tempBinary = contract.temp.currentBinary
     originalBinary = contract.original.currentBinary

     contract2 = Universa.Contract.fromBOSS(tempBinary)
     contractBlob = contract.original.toFile


     ########Network
     nodeAPI = await Node.connect(privateKey)
     console.log(nodeAPI)

     originalState = contract.checkOriginal(nodeAPI)
     console.log(originalState)
     tempState = contract.checkTemp(nodeAPI)
     console.log(tempState)

     response = await XChange.getCost(contract.original)
     console.log(response.cost)
     console.log(response.testnetCompatible)



     #############Cloud operation
     APP_TOKEN = "ehUEdnfhDqhBAWK77Rndam0d+L6NVhGDGm/ma3SHHZQA4kgs4uf1AnYJKqKHU2qQZchSeieZyquGOuCHeUtULw=="

     chatleApi = await CryptoCloud.connect(APP_TOKEN, privateKey)
     isRegistered = await chatleApi.registerNick("myNickExample", true)
     console.log(isRegistered)

     isRemoved = await chatleApi.registerNick("myNickExample");
     console.log(isRemoved)

     result = await chatleApi.setPassword("myPasswordExample");
     console.log(result)

     #chatleApi = CryptoCloud.connectWithPassword(APP_TOKEN, nick, password)

     party = await chatleApi.getParty({ nick: 'myNickExample' })
     console.log(party.isAccepted)
     console.log(party.canBeAccepted)

     message = await party.newMessage("Hello world!");
     await message.deliver()


     messages = chatleApi.loadItems({ tags: ["comm"], latestSerial: 100 })
     messages = chatleApi.loadItems({ tags: ["comm"], beforeSerial: 100 })
     messages = chatleApi.loadItems({ tags: ["comm"], afterSerial: 100 })
     messages = chatleApi.loadItems({ tags: ["comm"], limit: 500 })


     ###########Store operations
     products = await XChange.store.getProducts()
     console.log(products)

     order = XChange.Order.create(publicKey, { "productCode":"1", "currency":"UTNP", "quantity":1, "returnAddress":"123", "promocode": null})
     response = await order.send()

     #costs = await XChange.getTransactionCost(transactionPack)






