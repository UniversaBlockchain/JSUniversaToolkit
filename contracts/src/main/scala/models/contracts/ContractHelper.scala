package models.contracts

import models._
import models.errors.{BalanceError, NoContractError, NoPermissionError, WalletIDError}

import scala.collection.mutable
import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import tools.universa.UniversaTools.{decode58}
import tools.universa.ImplicitConverters._

import tools.universa.UniversaTools._


object ContractHelper {
  val CurrentCapsuleVersion = 3

  case class CapsuleTuple(main: Capsule, sibling: Capsule)

  @JSExportTopLevel("Universa.joinContracts")
  def joinContracts(
    contracts: js.Array[ContractExported],
    useLongAddress: Boolean
  ): TransactionPackExported = {
    validateContracts(contracts, useLongAddress)

    var innerContracts = ListBuffer.empty[Contract]
    innerContracts ++= contracts.map(_.contract)
    val capsule = joinContracts(innerContracts)
    val tpack = new TransactionPack(capsule.currentBinary)

    tpack.setSubItems(innerContracts.map(_.original.currentBinary))

    new TransactionPackExported(tpack)
  }

  def joinContracts(contracts: Seq[Contract]): Capsule = {
    // val firstContract = contracts.head
    val firstCapsule = contracts.head.original.copy()
    val joinedContract = new Contract(firstCapsule.currentBinary)
    joinedContract.createRevision
    joinedContract.temp.lockDataAndSign()
    val joinedCapsule = joinedContract.temp.copy()

    joinedCapsule.resetTransactional
    joinedCapsule.version = CurrentCapsuleVersion

    val totalAmount = contracts.map(_.temp.getAmount).sum
    if (contracts.size > 1)
      joinedCapsule.setAmount(totalAmount)

    joinedCapsule.newIds = ListBuffer.empty[HashId]
    joinedCapsule.revokingIds = ListBuffer.empty[HashId]

    contracts.foreach(contract =>
      joinedCapsule.revokingIds += contract.original.currentHashId
    )

    joinedCapsule
  }

  def splitCapsule(capsule: Capsule): CapsuleTuple = {
    val main: Capsule = capsule.copy()
    main.resetTransactional

    // FIXME: need to lock capsule before copy
    main.lockDataAndSign()
    val sibling: Capsule = main.copy()
    val revision = main.revision - 1

    main.setState("branch_id", s"$revision:0")
    sibling.setState("branch_id", s"$revision:1")
    CapsuleTuple(main, sibling)
  }

  private def validateContracts(
    contracts: js.Array[ContractExported],
    useLongAddress: Boolean
  ): Unit = {
    if (contracts.isEmpty)
      throw new NoContractError("contract list cannot be empty")

    val innerContracts = contracts.map(_.contract)
    if (innerContracts.exists(_.temp.getSplitJoin.isEmpty))
      throw new NoPermissionError("No SplitJoinPermission in input contracts")

    val headWalletId = innerContracts.head.walletID(useLongAddress)

    if (innerContracts.exists(_.walletID(useLongAddress) != headWalletId))
      throw new WalletIDError("join_match_fields in provided contracts are different")
  }

  private def addDescription(capsule: Capsule, description: String): Unit = {
    val modifyData = capsule.getModifyData
    val isOwner = (p: Permission) =>
      p.role.asInstanceOf[RoleLink].targetName == "owner"
    //TODO double check asInstanceOf[...]
    if (modifyData.fold(false)(isOwner(_)))
      capsule.setState("data.description", description)
  }

  private def selectPaymentContracts(
    sortedContracts: Seq[(Contract, BigDecimal)],
    amount: BigDecimal,
    limited: Boolean
  ): (Seq[Contract], BigDecimal) = {
    val sortedContractLimited =
      if (limited) sortedContracts.take(7)
      else sortedContracts

    var sum = BigDecimal(0)
    var resultList = ListBuffer[Contract]()
    //iterate until sum >= amount

    (0 until sortedContractLimited.size) foreach (i => {
      if (sum.compare(amount) < 0) {
        resultList += sortedContractLimited(i)._1
        sum += sortedContractLimited(i)._2
      }
    })

    (Seq(resultList: _*), sum)
  }

  private def selectContractsForJoin(
    contracts: Seq[Contract],
    amount: BigDecimal
  ): (Seq[Contract], BigDecimal) = {
    val sortedContracts = contracts.sortBy(_.temp.getAmount)
    val sortedAscContractsWithAmount = sortedContracts.map(c => (c, c.temp.getAmount))

    //1. try to find enough contracts between smallest 7
    val res = selectPaymentContracts(sortedAscContractsWithAmount, amount, true)

    if (res._2.compare(amount) >= 0) (res._1, res._2)
    else {
      //2. try to find enough any 7 contracts, started from smallest ones
      val foundList =  sortedAscContractsWithAmount.sliding(7).find(cc => cc.map(_._2).sum >= amount)
      if (foundList.isDefined) (foundList.get.map(_._1), foundList.get.map(_._2).sum)
      else {
        //3. try to find enough any contracts, started from biggest
        val reversedContractsWithAmount = sortedAscContractsWithAmount.reverse
        val res = selectPaymentContracts(reversedContractsWithAmount, amount, false)
        if (res._2.compare(amount) >= 0) (res._1, res._2)
        else throw new BalanceError("Not enough contracts to join for selected amount")
      }
    }
  }


  @JSExportTopLevel("Universa.splitContracts")
  def splitContracts(
                      contracts: js.Array[ContractExported],
                      amount: String,
                      ownerKeys: js.Array[PrivateKeyExported],
                      recipientPublicKey: PublicKeyExported,
                      description: String
                    ): TransactionPackExported = {
    splitContractsJS(contracts, amount, ownerKeys, recipientPublicKey, description, false, true)
  }

  @JSExportTopLevel("Universa.splitContracts")
  def splitContracts(
                      contracts: js.Array[ContractExported],
                      amount: String,
                      ownerKeys: js.Array[PrivateKeyExported],
                      recipientAddress: String,
                      description: String
                    ): TransactionPackExported = {
    splitContractsJS(contracts, amount, ownerKeys, recipientAddress, description, false, true)
  }

  @JSExportTopLevel("Universa.splitContracts")
  def splitContractsJS(
    contracts: js.Array[ContractExported],
    amount: String,
    ownerKeys: js.Array[PrivateKeyExported],
    recipientAddress58: String,
    description: String,
    useLongAddress: Boolean,
    transferAsMain: Boolean
  ): TransactionPackExported = {
    validateContracts(contracts, useLongAddress)

    if (BigDecimal(amount) <= 0)
      throw new BalanceError("amount must be positive")

    val tpack = splitByAmount(
      contracts.map(_.contract),
      BigDecimal(amount),
      ownerKeys.map(_.privateKey),
      new KeyAddress(decode58(recipientAddress58)),
      // recipientPublicKey.publicKey,
      description,
      useLongAddress,
      transferAsMain
    )

    new TransactionPackExported(tpack)
  }

  @JSExportTopLevel("Universa.splitContracts")
  def splitContractsJS(
    contracts: js.Array[ContractExported],
    amount: String,
    ownerKeys: js.Array[PrivateKeyExported],
    recipientPublicKey: PublicKeyExported,
    description: String,
    useLongAddress: Boolean,
    transferAsMain: Boolean
  ): TransactionPackExported = {
    validateContracts(contracts, useLongAddress)

    if (BigDecimal(amount) <= 0)
      throw new BalanceError("amount must be positive")

    val tpack = splitByAmount(
      contracts.map(_.contract),
      BigDecimal(amount),
      ownerKeys.map(_.privateKey),
      recipientPublicKey.publicKey,
      description,
      useLongAddress,
      transferAsMain
    )

    new TransactionPackExported(tpack)
  }

  def splitByAmount(
    contracts: Seq[Contract],
    amount: BigDecimal,
    ownerKeys: Seq[PrivateKey],

    recipientPublicKey: PublicKey,

    description: String,
    useLongAddress: Boolean = false,
    transferAsMain: Boolean
  ): TransactionPack = {
    splitForRole(
      contracts,
      amount,
      ownerKeys,
      new RoleSimple("owner", recipientPublicKey, useLongAddress),
      description,
      useLongAddress,
      transferAsMain
    )
  }

  def splitByAmount(
    contracts: Seq[Contract],
    amount: BigDecimal,
    ownerKeys: Seq[PrivateKey],

    recipientAddress: KeyAddress,

    description: String,
    useLongAddress: Boolean,
    transferAsMain: Boolean
  ): TransactionPack = {
    splitForRole(
      contracts,
      amount,
      ownerKeys,
      new RoleSimple("owner", recipientAddress),
      description,
      useLongAddress,
      transferAsMain
    )
  }

  def splitForRole(
    contracts: Seq[Contract],
    amount: BigDecimal,
    ownerKeys: Seq[PrivateKey],

    newOwner: Role,

    description: String,
    useLongAddress: Boolean,
    transferAsMain: Boolean
  ): TransactionPack = {
    val (sortedContracts, sum) = selectContractsForJoin(contracts, amount)
    var references = ListBuffer.empty[Contract]
    sortedContracts.foreach(contract => references += contract)

    val parent = sortedContracts.head.original
    val parentRevision = parent.revision

    val joinedCapsule = joinContracts(sortedContracts)

    val creator = new RoleSimple("creator", ownerKeys(0).publicKey, useLongAddress)

    var transferCapsule: Capsule = null
    var cashbackCapsule: Capsule = null
    val hasCashback = !sum.equals(amount)

    def seal(cap: Capsule): Unit = {
      cap.lockData
      ownerKeys.foreach(ownerKey => cap.sign(ownerKey))
      cap.lock
    }

    def setCommonDetails(cap: Capsule): Unit = {
      addDescription(cap, description)
      cap.setState("created_by", creator)
      cap.resetRevokingContracts
    }

    if (hasCashback) {
      val split = splitCapsule(joinedCapsule)

      if (transferAsMain) {
        transferCapsule = split.main
        cashbackCapsule = split.sibling
      } else {
        transferCapsule = split.sibling
        cashbackCapsule = split.main
      }

      cashbackCapsule.setAmount(sum - amount)

      setCommonDetails(cashbackCapsule)
    } else {
      transferCapsule = joinedCapsule
    }

    setCommonDetails(transferCapsule)

    if (hasCashback)
      transferCapsule.setAmount(amount)

    transferCapsule.setState("owner", newOwner)

    if (hasCashback) {
      if (transferAsMain) {
        seal(cashbackCapsule)
        transferCapsule.setNewContracts(ListBuffer(new Contract(cashbackCapsule.currentBinary)))
        transferCapsule.setRevokingContracts(references)
        seal(transferCapsule)
      } else {
        seal(transferCapsule)
        cashbackCapsule.setNewContracts(ListBuffer(new Contract(transferCapsule.currentBinary)))
        cashbackCapsule.setRevokingContracts(references)
        seal(cashbackCapsule)
      }
    } else {
      transferCapsule.setRevokingContracts(references)
      seal(transferCapsule)
    }

    var mainCapsule: Capsule = null

    if (transferAsMain || !hasCashback) {
      mainCapsule = transferCapsule
    } else {
      mainCapsule = cashbackCapsule
    }

    val tpack = new TransactionPack(mainCapsule.currentBinary)

    tpack.setSubItems(references.map(_.original.currentBinary))

    if (hasCashback) {
      if (transferAsMain) tpack.addSubItem(cashbackCapsule.currentBinary)
      else tpack.addSubItem(transferCapsule.currentBinary)
    }

    tpack
  }

  def createCompound(
    tpack: TransactionPack,
    definitionData: HashMap[String, Any] = HashMap.empty[String, Any]
  ): TransactionPack = {
    val mainCapsule = tpack.mainCapsule
    val creator = mainCapsule.getRole("creator")

    val rolesList = mainCapsule.getRoles.values.toSeq
    val roles = mutable.Seq[Role](rolesList: _*)

    val compoundCap = Capsule(creator.get.resolve(roles))

    compoundCap.setTTL(14, "day")
    compoundCap.addNewCapsules(ListBuffer(mainCapsule))
    compoundCap.setDefinition("data", definitionData)
    compoundCap.lockData
    compoundCap.lock

    val compoundPack = new TransactionPack(compoundCap.currentBinary)
    compoundPack.addSubItem(mainCapsule.currentBinary)

    tpack.subItems.map(compoundPack.addSubItem(_))

    tpack.referencedItems.map(compoundPack.addReferencedItem(_))

    compoundPack
  }

  def extractCompound(
    tpack: TransactionPack,
    mainCapsuleId: HashId
  ): TransactionPack = {
    val subItems = tpack.subItems
    val referencedItems = tpack.referencedItems
    val mainCapsule = subItems.map(new Capsule(_)).find(cap => cap.hashId.equals(mainCapsuleId))

    if (mainCapsule.isEmpty) throw new Exception("main capsule not found")

    val extracted = new TransactionPack(mainCapsule.get.currentBinary)

    subItems.map(new Capsule(_)).map(item => {
      if (!item.hashId.equals(mainCapsuleId))
        extracted.addSubItem(item.currentBinary)
    })

    referencedItems.map(new Capsule(_)).map(item => {
      extracted.addReferencedItem(item.currentBinary)
    })

    extracted
  }

  @JSExportTopLevel("Universa.createCompound")
  def createCompoundJS(
    tpack: TransactionPackExported,
    definitionData: js.Dictionary[Any] = js.Dictionary[Any]()
  ): TransactionPackExported = {
    new TransactionPackExported(createCompound(tpack.tp, definitionData))
  }

  @JSExportTopLevel("Universa.extractCompound")
  def extractCompound(
    tpack: TransactionPackExported,
    mainCapsuleId64: String
  ): TransactionPackExported = {
    val hashId = HashId.fromBytes(decode64(mainCapsuleId64))

    new TransactionPackExported(extractCompound(tpack.tp, hashId))
  }
}
