package controllers

import play.api.mvc._

import play.api.Logger
import models._

object Application extends Controller {

  val elevator = new SimpleElevator(6, new StopStrategy())

  def ping = Action {
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

  def go(floorToGo: Int) = Action {
    Logger.debug(s"Go to $floorToGo")
    elevator.gotTo(floorToGo)

    Ok
  }


  def call(atFloor: Int, to: String) = Action {
    Logger.debug(s"Call $atFloor direction $to")
    elevator.gotTo(atFloor)

    Ok
  }

  def nextCommand = Action {
    val nextCommand = elevator.nextCommand()

    Ok(nextCommand)
  }

  def reset(cause: String) = Action {
    Logger.info(s"Reset for '$cause'")
    elevator.reset

    Ok
  }



}