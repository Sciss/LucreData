import sbt._

class TreeTestsProject( info: ProjectInfo ) extends DefaultProject( info ) {
//   val st         = "org.scalatest" %% "scalatest" % "1.6.1" % "test"
   val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"

//   val prefuse    = "prefuse" % "prefuse" % "beta-SNAPSHOT" from "http://github.com/downloads/Sciss/ScalaColliderSwing/prefuse-beta-SNAPSHOT.jar"
   val prefuse    = "de.sciss" % "prefuse" % "0.20"
   val itextpdf   = "com.itextpdf" % "itextpdf" % "5.1.1"

   val itextRepo  = "itextpdf.com" at "http://maven.itextpdf.com"

   override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}