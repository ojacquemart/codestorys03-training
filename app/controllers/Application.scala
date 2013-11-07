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
    Logger.info(s"Reset $lowerFloor - $higherFloor for '$cause'")
    elevator.reset(lowerFloor)

    Ok
  }

  def call(atFloor: Int, to: String) = Action {
    Logger.debug(s"Call $atFloor direction $to")
    elevator.call(atFloor, Directions.valueOf(to))

    Ok
  }

  def go(floorToGo: Int) = Action {
    Logger.debug(s"Go to $floorToGo")
    elevator.go(floorToGo)

    Ok
  }

  def userHasEntered = Action {
    Logger.info("User entered")
    elevator.addUser

    Ok
  }

  def userHasExited = Action {
    Logger.info("User exited")
    elevator.removeUser

    Ok
  }

  def nextCommand = Action {
    Logger.info(s"Next command, status beforeAction: ${elevator.getStatus}")
    val nextCommand = elevator.getNextCommand()
    Logger.info(s"Next command: $nextCommand")
    Logger.info(s"Next command, status afterAction: ${elevator.getStatus}")

    Ok(nextCommand)
  }


}