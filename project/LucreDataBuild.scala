import sbt._
import Keys._

object LucreDataBuild extends Build {
   lazy val lucredata: Project = Project(
      id        = "lucredata",
      base      = file( "." ),
      aggregate = Seq( core, views )
   )

   lazy val core = Project(
      id        = "lucredata-core",
      base      = file( "core" ),
      settings     = Project.defaultSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "de.sciss" %% "lucrestm" % "1.2.+"
//            "org.scalatest" %% "scalatest" % "1.7.2" % "test"
         )
      )
   )

   lazy val views = Project(
      id           = "lucredata-views",
      base         = file( "views" ),
      dependencies = Seq( core ),
      settings     = Project.defaultSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "com.itextpdf" % "itextpdf" % "5.3.2"
         )
      )
   )
}
