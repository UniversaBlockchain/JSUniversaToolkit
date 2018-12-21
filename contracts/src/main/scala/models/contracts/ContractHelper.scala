package models.contracts

import models._
import models.errors.{BalanceError, NoContractError, NoPermissionError, WalletIDError}

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

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

    tpack.setContracts(innerContracts)

    new TransactionPackExported(tpack)
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
//    println("headWalletId:" + headWalletId)
//    if (innerContracts.length > 1) println("(1).WalletId:" + innerContracts(1).walletID)
    if (innerContracts.exists(_.walletID(useLongAddress) != headWalletId))
      throw new WalletIDError("join_match_fields in provided contracts are different")
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
    joinedCapsule.setAmount(totalAmount)

    joinedCapsule.newIds = ListBuffer.empty[HashId]
    joinedCapsule.revokingIds = ListBuffer.empty[HashId]

    contracts.foreach(contract =>
      joinedCapsule.revokingIds += contract.original.currentHashId
    )

    joinedCapsule
  }

  @JSExportTopLevel("Universa.splitContracts")
  def splitContracts(
                      contracts: js.Array[ContractExported],
                      amount: Double,
                      payerPrivateKey: PrivateKeyExported,
                      recipientPublicKey: PublicKeyExported,
                      description: String
                    ): TransactionPackExported = {
    splitContracts(contracts, amount, payerPrivateKey, recipientPublicKey, description, false)
  }

  @JSExportTopLevel("Universa.splitContracts")
  def splitContracts(
    contracts: js.Array[ContractExported],
    amount: Double,
    payerPrivateKey: PrivateKeyExported,
    recipientPublicKey: PublicKeyExported,
    description: String,
    useLongAddress: Boolean
  ): TransactionPackExported = {
    validateContracts(contracts, useLongAddress)
    if (amount <= 0)
      throw new BalanceError("amount must be positive")

    val tpack = splitAmount(
      contracts.map(_.contract),
      BigDecimal(amount),
      payerPrivateKey.privateKey,
      recipientPublicKey.publicKey,
      description,
      useLongAddress)
    new TransactionPackExported(tpack)
  }

  def splitAmount(
    contracts: Seq[Contract],
    amount: BigDecimal,
    payerPrivateKey: PrivateKey,
    recipientPublicKey: PublicKey,
    description: String,
    useLongAddress: Boolean = false
  ): TransactionPack = {

    def findContractsToPay(
      sortedContracts: Seq[(Contract, BigDecimal)],
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

      //alternative0
//      val (contracts, contractsAmounts) = sortedContractLimited.unzip
//      val summed = contractsAmounts.toStream
//        .scanLeft(BigDecimal("0")) {case (e, sum1) => sum1 + e}
//      contracts.take(summed.indexWhere(_ >= amount))
      // alternative1
//       val cc = sortedContractLimited.span(c => {
//         sum += c._2
//         amount.compare(sum) > 0
//       }) match {
//         case (h, t) => h ++ t.take(1)
//       }
//       (cc.map(_._1), sum)
      //alternative2
//      def sumup(nextIndex: Int, sumNow: BigDecimal): (Int, BigDecimal) = {
//        if (sumNow < amount) sumup(nextIndex + 1, sumNow + sortedContractLimited(nextIndex)._2)
//        else (nextIndex, sumNow)
//      }
//      val (contractAmount, sum) = sumup(0, 0)
//      (sortedContractLimited.take(contractAmount).map(_._1), sum)

      (Seq(resultList: _*), sum)
    }

    //returns contracts to join and sum of these contracts
    def findContractsToJoin(): (Seq[Contract], BigDecimal) = {
      val sortedContracts = contracts.sortBy(_.temp.getAmount)
      val sortedAscContractsWithAmount = sortedContracts.map(c => (c, c.temp.getAmount))

      //1. try to find enough contracts between smallest 7
      val res = findContractsToPay(sortedAscContractsWithAmount, true)

      if (res._2.compare(amount) >= 0) (res._1, res._2)
      else {
        //2. try to find enough any 7 contracts, started from smallest ones
        val foundList =  sortedAscContractsWithAmount.sliding(7).find(cc => cc.map(_._2).sum >= amount)
        if (foundList.isDefined) (foundList.get.map(_._1), foundList.get.map(_._2).sum)
        else {
          //3. try to find enough any contracts, started from biggest
          val reversedContractsWithAmount = sortedAscContractsWithAmount.reverse
          val res = findContractsToPay(reversedContractsWithAmount, false)
          if (res._2.compare(amount) >= 0) (res._1, res._2)
          else throw new BalanceError("Not enough contracts to join for selected amount")
        }
      }
    }

    val (selectedContracts, sum) = findContractsToJoin()
    var references = ListBuffer.empty[Contract]
    selectedContracts.foreach(contract => references += contract)
    // println(s"selectedContracts amount: ${selectedContracts.length}, sum: $sum")

    val parent = selectedContracts.head.original
    val parentRevision = parent.revision
    val payerPublicKey = payerPrivateKey.publicKey //previosOwnerPair.getPublicKey() //FIXME use capsule.signByRole method

    val joinedCapsule = joinContracts(selectedContracts)
    // joinedCapsule.setRoleByKey("creator", payerPublicKey, useLongAddress)
    // joinedCapsule.lockData

    val owner = new RoleSimple("owner", recipientPublicKey, useLongAddress)
    val creator = new RoleSimple("creator", payerPublicKey, useLongAddress)
    //    val previousOwnerId = joinedCapsule.ownerPairId //
    //    val previosOwnerPair = Pair.store.get(previousOwnerId) //

    def addDescription(capsule: Capsule): Unit = {
      val modifyData = capsule.getModifyData
      val isOwner = (p: Permission) =>
        p.role.asInstanceOf[RoleLink].targetName == "owner"
      //TODO double check asInstanceOf[...]
      if (modifyData.fold(false)(isOwner(_)))
        capsule.setState("data.description", description)
    }

    var transferCapsule: Capsule = null
    var cashbackCapsule: Capsule = null

    if (sum.equals(amount)) {
      transferCapsule = joinedCapsule
    }
    else {
      val split = splitCapsule(joinedCapsule)

      cashbackCapsule = split.main
      transferCapsule = split.sibling

      cashbackCapsule.setAmount(sum - amount)
      transferCapsule.setAmount(amount)
    }

    addDescription(transferCapsule)
    transferCapsule.setState("owner", owner)
    transferCapsule.setState("created_by", creator)
    transferCapsule.setRevokingContracts(references)
    transferCapsule.lockDataAndSign(payerPrivateKey)

    if (cashbackCapsule != null) {
      addDescription(cashbackCapsule)
      cashbackCapsule.setNewContracts(ListBuffer(new Contract(transferCapsule.currentBinary)))
      cashbackCapsule.setState("created_by", creator)
      cashbackCapsule.setRevokingContracts(references)
      cashbackCapsule.lockData
      cashbackCapsule.sign(payerPrivateKey)
      cashbackCapsule.lock
    } else {
      cashbackCapsule = transferCapsule
      transferCapsule = null
    }

    // cashbackCapsule.setRevokingContracts(references)
    // cashbackCapsule.lock

    val tpack = new TransactionPack(cashbackCapsule.currentBinary)

    tpack.setContracts(references)

    if (transferCapsule != null)
      tpack.addItem(transferCapsule.currentBinary)

    tpack
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

}
