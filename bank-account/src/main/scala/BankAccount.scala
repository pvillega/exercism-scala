import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

case class BankAccount() {
  val balance = new AtomicInteger(0)
  val open = new AtomicBoolean(true)

  def getBalance: Option[Int] = if(open.get()) Some(balance.get()) else None

  def incrementBalance(amount: Int): Option[Int] = Option(balance.addAndGet(amount))

  def closeAccount(): Unit = open.compareAndSet(true, false)
}
