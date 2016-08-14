package week4

class BankAccountFRP {
  private val balance = new Var(0)

  def currentBalance: Int = balance()

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      val b = balance()
      balance() = amount + b
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    }
    else throw new Exception("insufficient funds")
}
