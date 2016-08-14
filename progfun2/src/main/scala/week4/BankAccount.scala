package week4

class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance: Int = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance += amount
      publish()
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance) {
      balance -= amount
      publish()
    }
    else throw new Exception("insufficient funds")
}

class Consolidator (observed: List[BankAccount]) extends Subscriber {
  observed foreach(_ subscribe this)

  private var total: Int = _  // "_" means "uninitialized"
  compute()

  private def compute() = total = (observed map (_.currentBalance)).sum

  def handler(pub: Publisher) = compute()

  def totalBalance = total
}
