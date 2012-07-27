import sbt._
import Keys._

object LucreDataBuild extends Build {
   lazy val lucredata: Project = Project(
      id        = "lucredata",
      base      = file( "." ),
      aggregate = Seq( structsCore, txn, mutable, viewsCore, txnViews, mutableViews )
   )

   lazy val structsCore = Project(
      id        = "lucredata-structs-core",
      base      = file( "structs-core" )
   )

   lazy val mutable = Project(
      id           = "lucredata-mutable",
      base         = file( "mutable" ),
      dependencies = Seq( structsCore )
   )
   
   lazy val txn = Project(
      id           = "lucredata-txn",
      base         = file( "txn" ),
      dependencies = Seq( structsCore ),
      settings     = Project.defaultSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "de.sciss" %% "lucrestm" % "0.34-SNAPSHOT"
//            "org.scalatest" %% "scalatest" % "1.7.2" % "test"
         )
      )
   )

   lazy val viewsCore = Project(
      id           = "lucredata-views-core",
      base         = file( "views-core" ),
      dependencies = Seq( structsCore ),
      settings     = Project.defaultSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "com.itextpdf" % "itextpdf" % "5.1.1"
         )
      )
   )

   lazy val mutableViews = Project(
      id           = "lucredata-mutable-views",
      base         = file( "mutable-views" ),
      dependencies = Seq( viewsCore, mutable )
   )

   lazy val txnViews = Project(
      id           = "lucredata-txn-views",
      base         = file( "txn-views" ),
      dependencies = Seq( viewsCore, txn )
   )
}
