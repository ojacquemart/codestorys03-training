package models

import play.api.libs.json.Json

case class UserByFloor(floor: Int, nbUsers: Int) {
  override def toString = {
    s"$floor -> $nbUsers"
  }
}

object UserByFloor {

  implicit val writes = Json.writes[UserByFloor]
}