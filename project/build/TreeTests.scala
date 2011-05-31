import sbt._

class TreeTestsProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val st = "org.scalatest" %% "scalatest" % "1.4.1" % "test"
}