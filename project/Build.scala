import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "codestory-elevator"
  val appVersion      = "1.0"

  val appDependencies = Seq(
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    testOptions in Test += Tests.Argument("junitxml", "console")
  )

}