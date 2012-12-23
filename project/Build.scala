import sbt._
import Keys._

object Build extends sbt.Build {
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
            "de.sciss" %% "lucrestm-core" % "1.6.+",
            "de.sciss" %% "lucrestm-bdb" % "1.6.+" % "test"
         )
      )
   )

   lazy val views = Project(
      id           = "lucredata-views",
      base         = file( "views" ),
      dependencies = Seq( core ),
      settings     = Project.defaultSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "de.sciss" %% "lucrestm-bdb" % "1.6.+" % "test",
            "com.itextpdf" % "itextpdf" % "5.3.2"
         )
      )
   )
}
