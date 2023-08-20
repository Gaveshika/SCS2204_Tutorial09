
object Question4 extends App {

  var bank: List[BankAccount] = List()

  def addAccount(nic: String, account_id: Int): Unit = {
    val newAccount = new BankAccount(nic, account_id)
    bank = newAccount :: bank
    println(bank)
  }

  val negativeBalances = (accounts: List[BankAccount]) => accounts.filter(account => account.balance < 0.0)
  val totalBalance = (accounts: List[BankAccount]) => accounts.foldLeft(0.0)((total, account) => total + account.balance)
  val applyInterest = (accounts: List[BankAccount]) =>
    accounts.map(account =>
      if (account.balance > 0) new BankAccount(account.nic, account.account_id, account.balance * 1.05)
      else new BankAccount(account.nic, account.account_id, account.balance * 1.1)
    )

  addAccount("1", 1)
  addAccount("2", 2)

  bank(0).deposit(1000)
  println(bank(0))

  bank(0).transfer(bank, 2, 100.0)  // Pass the bank list to the transfer method
  println(bank(1))

  println(negativeBalances(bank))

  println(totalBalance(bank))

  val newBalances = applyInterest(bank)
  println(newBalances)
}

class BankAccount(val nic: String, val account_id: Int, var balance: Double = 0.0) {

  def withdraw(amount: Double): Unit = {
    this.balance -= amount
  }

  def deposit(amount: Double): Unit = {
    this.balance += amount
  }

  def transfer(bank: List[BankAccount], toAccount: Int, amount: Double): Unit = {
    val transferAccount = bank.find(account => account.account_id == toAccount)
    transferAccount match {
      case Some(destAccount) =>
        if (balance >= amount) {
          this.withdraw(amount)
          destAccount.deposit(amount)
        } else {
          println("Insufficient balance for transfer.")
        }
      case None =>
        println(s"Account $toAccount not found for transfer.")
    }
  }

  override def toString: String = s"[$nic:$account_id:$balance]"
}
