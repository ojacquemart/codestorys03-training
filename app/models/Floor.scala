package models

import play.api.libs.json.Json

case class Floor(floor: Int, users: Int) {
  override def toString() = s"$floor -> $users"
}

object Floor {
  implicit val writes = Json.writes[Floor]
}
