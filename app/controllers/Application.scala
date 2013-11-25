package controllers

import play.api.mvc._

import play.api.Logger
import models._
import play.api.libs.json.Json

import scala.collection.mutable.MutableList

object Application extends Controller {

  var lastResets = MutableList[SimpleReset]()
  var elevator = Elevator(lowerFloor = 0, higherFloor = 42, cabinSize = 10, cabinCount = 2)

  def reset(lowerFloor: Int, higherFloor: Int, cabinSize: Int, cabinCount: Int, cause: String) = Action {
    Logger.info(s"""@@@ RESET
           Reset lower=$lowerFloor, higher=$higherFloor, cabinSize=$cabinSize, cabinCount=$cabinCount cause='$cause'""")
    lastResets += SimpleReset(cause, ElevatorStatus.get(elevator))

    elevator = Elevator(lowerFloor, higherFloor, cabinSize, cabinCount)

    Ok
  }

  def call(atFloor: Int, to: String) = Action {
    Logger.info(s"@@@ CALL $atFloor direction $to")
    elevator.call(atFloor, Directions.valueOf(to))

    Ok
  }

  def go(cabin: Int, floorToGo: Int) = Action {
    Logger.info(s"@@@ GO IN CABIN $cabin TO $floorToGo")
    elevator.go(cabin, floorToGo)

    Ok
  }

  def userHasEntered(cabin: Int) = Action {
    Logger.info(s"@@@ USER ENTERED IN $cabin")
    elevator.userHasEntered

    Ok
  }

  def userHasExited(cabin: Int) = Action {
    Logger.info(s"@@@ USER EXITED FROM $cabin")
    elevator.onUserExited

    Ok
  }

  def nextCommands = Action {
    val nextCommands = elevator.nextCommands()
    Logger.info(s"@@@ NEXT COMMANDS: $nextCommands")
    Logger.info(s"@@@ NEXT COMMAND, debug: ${elevator.getStatus}")

    Ok(nextCommands)
  }

  def status = Action {
    Ok(Json.toJson(ElevatorRecap.get(elevator)))
  }

  def resets = Action {
    Ok(Json.arr(lastResets))
  }


}