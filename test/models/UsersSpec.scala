package models

import org.specs2.mutable.Specification

import UserState._

object UsersSpec extends Specification {

  import Ticker._

  "Users" should {

    def threeUsers = {
      val users = new Users()
      users.addWaiter(0, UP)
      users.addWaiter(0, DOWN)
      users.addWaiter(10, DOWN)

      users
    }

    "reset" in {
      val users = threeUsers

      users.size must be >=(0)
      users.reset
      users.size must be equalTo(0)
    }

    "add users and get size" in {
      val users = threeUsers

      users.size must be equalTo(3)
    }


    "remove done users" in {
      val users = new Users()
      users.addWaiter(0, UP)
      users.addWaiter(0, DOWN)
      users.addWaiter(10, DOWN)

      users.size must be equalTo(3)
      users.users.foreach(_.state = UserState.DONE)

      users.removeDone()
      users.size must be equalTo(0)
    }

  }

}
