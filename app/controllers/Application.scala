package controllers

import play.api.mvc._

import play.api.Logger
import models._

object Application extends Controller {

  val elevator = new SimpleElevator(20, new OpenCloseStrategy())

  def ping = Action {
    Ok
  }

  def reset(lowerFloor: Int, higherFloor: Int, cause: String) = Action {
    Logger.info(s"""@@@ RESET
           Reset $lowerFloor - $higherFloor for '$cause'""")
    elevator.resetToFloor(lowerFloor)

    Ok
  }

  def call(atFloor: Int, to: String) = Action {
    Logger.info(s"@@@ CALL $atFloor direction $to")
    elevator.call(atFloor, Directions.valueOf(to))

    Ok
  }

  def go(floorToGo: Int) = Action {
    Logger.info(s"@@@ GO TO $floorToGo")
    elevator.go(floorToGo)

    Ok
  }

  def userHasEntered = Action {
    Logger.info(s"@@@ USER ENTERED AT ${elevator.floor}")
    elevator.userHasEntered

    Ok
  }

  def userHasExited = Action {
    Logger.info(s"@@@ USER EXITED AT ${elevator.floor}")
    elevator.onUserExited

    Ok
  }

  def nextCommand = Action {
    Logger.info(s"@@@ NEXT COMMAND, status beforeAction: ${elevator.getStatus}")
    val nextCommand = elevator.nextCommand()
    Logger.info(s"@@@ NEXT COMMAND: $nextCommand")
    Logger.info(s"@@@ NEXT COMMAND, status afterAction: ${elevator.getStatus}")

    Ok(nextCommand)
  }


}