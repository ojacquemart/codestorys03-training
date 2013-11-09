package models

import org.specs2.mutable.Specification

object DirectionSpec extends Specification {

  "Direction" should {

    "inverse current direction" in {
      UP.inverse should be equalTo(DOWN)
      DOWN.inverse should be equalTo(UP)
    }
  }

  "Directions" should {

    "get value from" in {
      Directions.valueOf("UP") must be equalTo(UP)
      Directions.valueOf("DOWN") must be equalTo(DOWN)
    }
  }

}
