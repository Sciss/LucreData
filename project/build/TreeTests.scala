import sbt._

class TreeTestsProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val st         = "org.scalatest" %% "scalatest" % "1.4.1" % "test"
   val prefuse    = "prefuse" % "prefuse" % "beta-SNAPSHOT" from "http://github.com/downloads/Sciss/ScalaColliderSwing/prefuse-beta-SNAPSHOT.jar"
   val itextpdf   = "com.itextpdf" % "itextpdf" % "5.1.1"

   val itextRepo  = "itextpdf.com" at "http://maven.itextpdf.com"

   override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}