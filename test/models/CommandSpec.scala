//package models
//
//import org.specs2.mutable.Specification
//
//object CommandSpec extends Specification {
//
//  val MaxFloor = 19
//  val elevator = new Elevator(MaxFloor, 10, new DirectionStrategy())
//
//  "Nothing command" should {
//    "do nothing" in {
//      NothingCommand.to(elevator) must be equalTo("NOTHING")
//    }
//  }
//
//  "Up command" should {
//
//    "do nothing when elevator is at top floor" in {
//      elevator.floor = MaxFloor
//      UpCommand.to(elevator) must be equalTo("NOTHING")
//    }
//
//    "go up one floor in UP direction" in {
//      elevator.floor = 1
//      elevator.direction = DOWN
//      UpCommand.to(elevator) must be equalTo("UP")
//      elevator.floor must be equalTo(2)
//      elevator.direction must be equalTo(UP)
//    }
//  }
//
//
//  "Down command" should {
//
//    "do nothing when elevator is at bottom floor" in {
//      elevator.floor = 0
//      DownCommand.to(elevator) must be equalTo("NOTHING")
//    }
//
//    "go down one floor in DOWN direction" in {
//      elevator.floor = 2
//      elevator.direction = UP
//      DownCommand.to(elevator) must be equalTo("DOWN")
//      elevator.floor must be equalTo(1)
//      elevator.direction must be equalTo(DOWN)
//    }
//  }
//
//  "Open command" should {
//
//    "do nothing when doors are already opened" in {
//      elevator.door = Door.OPEN
//      OpenCommand.to(elevator) must be equalTo("NOTHING")
//      elevator.door must be equalTo(Door.OPEN)
//    }
//
//    "open doors if doors are closed" in {
//      elevator.door = Door.CLOSE
//      OpenCommand.to(elevator) must be equalTo("OPEN")
//      elevator.door must be equalTo(Door.OPEN)
//    }
//  }
//
//  "Close command" should {
//
//    "do nothing when doors are already closed" in {
//      elevator.door = Door.CLOSE
//      CloseCommand.to(elevator) must be equalTo("NOTHING")
//      elevator.door must be equalTo(Door.CLOSE)
//    }
//
//    "close doors if doors are opened" in {
//      elevator.door = Door.OPEN
//      CloseCommand.to(elevator) must be equalTo("CLOSE")
//      elevator.door must be equalTo(Door.CLOSE)
//    }
//  }
//
//  "Direction" should {
//
//    "inverse current direction" in {
//      UP.inverse should be equalTo(DOWN)
//      DOWN.inverse should be equalTo(UP)
//    }
//  }
//
//  "Directions" should {
//
//    "get value from" in {
//      Directions.valueOf("UP") must be equalTo(UP)
//      Directions.valueOf("DOWN") must be equalTo(DOWN)
//    }
//  }
//
//}
