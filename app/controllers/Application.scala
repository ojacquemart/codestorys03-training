package controllers

import play.api.mvc._

import play.api.Logger
import models._

object Application extends Controller {

  val elevator = new SimpleElevator(20, new WithStopStrategy())

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
    elevator.gotTo(atFloor)

    Ok
  }

  def go(floorToGo: Int) = Action {
    Logger.debug(s"Go to $floorToGo")
    elevator.gotTo(floorToGo)

    Ok
  }

  def userHasEntered = Action {
    Logger.info("User entered")
    Ok
  }

  def userHasExited = Action {
    Logger.info("User exited")
    Ok
  }

  def nextCommand = Action {
    val nextCommand = elevator.getNextCommand()
    Logger.info(s"Next command: $nextCommand")
    Logger.info(s"Current status: " +
              s"floor=${elevator.floor}, open=${elevator.opened}, " +
              s"direction=${elevator.direction}, stops=${elevator.getStops()}")

    Ok(nextCommand)
  }


}