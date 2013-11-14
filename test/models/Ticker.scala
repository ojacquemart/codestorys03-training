package models

object Ticker {
  def tick100times(user: User) = {
    for (i <- 1 to 100) user.tick(i)
  }
}
