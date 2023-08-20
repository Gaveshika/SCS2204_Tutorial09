object Question3 extends App {
  var account_list: List[Account] = List()

  def account_create(nic: String, account_id: Int): Unit = {
    val a = new Account(nic, account_id)
    account_list = account_list ::: a :: Nil
    println(account_list)
  }

  val find = (x: Int, y: List[Account]) => y.filter(account => account.account_id.equals(x))

  account_create("1", 1)
  account_create("2", 2)

  find(1, account_list)(0).deposit(1000)
  println(find(1, account_list)(0))

  find(1, account_list)(0).transfer(find(2, account_list)(0), 100.0)
  println(find(2, account_list)(0))

}

class Account(nic: String, val account_id: Int, var balance: Double = 0.0) {

  def withdraw(amount: Double): Unit = {
    if (balance >= amount) {
      this.balance -= amount
    } else {
      println("Insufficient balance.")
    }
  }

  def deposit(amount: Double): Unit = {
    this.balance += amount
  }

  def transfer(toAccount: Account, amount: Double): Unit = {
    if (balance >= amount) {
      this.withdraw(amount)
      toAccount.deposit(amount)
    } else {
      println("Insufficient balance for transfer.")
    }
  }

  override def toString: String = "[" + nic + ":" + account_id + ":" + balance + "]"
}
