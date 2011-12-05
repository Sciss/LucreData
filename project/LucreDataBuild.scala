import sbt._
import Keys._

object LucreDataBuild extends Build {
   lazy val lucredata = Project(
      id        = "lucredata",
      base      = file( "." ),
      settings  = standardSettings,
      aggregate = Seq( structs, views, /* tests, */ full )
   )

   lazy val structs = Project(
      id        = "lucredata-structs",
      base      = file( "." ),
      settings  = standardSettings,
      aggregate = Seq( mutable, txn )
   )

   lazy val views = Project(
      id        = "lucredata-views",
      base      = file( "." ),
      settings  = standardSettings,
      aggregate = Seq( mutableViews, txnViews )
   )

   lazy val structsCore = Project(
      id        = "lucredata-structs-core",
      base      = file( "structs-core" ),
      settings  = standardSettings
   )

   lazy val mutable = Project(
      id           = "lucredata-mutable",
      base         = file( "mutable" ),
      dependencies = Seq( structsCore ),
      settings     = standardSettings ++ Seq(
         libraryDependencies ++= Seq(
            "org.scalatest" %% "scalatest" % "1.6.1" % "test"
         )
      )
   )
   
   lazy val txn = Project(
      id           = "lucredata-txn",
      base         = file( "txn" ),
      dependencies = Seq( structsCore ),
      settings     = standardSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "de.sciss" %% "lucrestm" % "0.10-SNAPSHOT",
            "org.scalatest" %% "scalatest" % "1.6.1" % "test"
         )
      )
   )

   lazy val viewsCore = Project(
      id           = "lucredata-views-core",
      base         = file( "views-core" ),
      dependencies = Seq( structsCore ),
      settings = standardSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "com.itextpdf" % "itextpdf" % "5.1.1"
         )
      )
   )

   lazy val mutableViews = Project(
      id           = "lucredata-mutable-views",
      base         = file( "mutable-views" ),
      dependencies = Seq( viewsCore, mutable ),
      settings     = standardSettings
   )

   lazy val txnViews = Project(
      id           = "lucredata-txn-views",
      base         = file( "txn-views" ),
      dependencies = Seq( viewsCore, txn ),
      settings     = standardSettings
   )
   
//   lazy val tests = Project(
//      id           = "lucredata-tests",
//      base         = file( "tests" ),
//      dependencies = Seq( structs ),
//      settings     = standardSettings ++ Seq( ... )
//   )

   lazy val full = {
      Project(
         id           = "lucredata-full",
         base         = file( "full" ),
         dependencies = Seq( structs, views )
      )
   }

   lazy val standardSettings = Defaults.defaultSettings ++ Seq(
      organization    := "de.sciss",
      version         := "0.12-SNAPSHOT",
      scalaVersion    := "2.9.1",
      resolvers       += "itextpdf.com" at "http://maven.itextpdf.com",
      retrieveManaged := true,
      
      publishSetting,
      credentialsSetting,
      pomExtra     := pomSettings,
  
      scalacOptions ++= Seq( "-deprecation", "-unchecked" /*, "-no-specialization" */)
   )
   
   lazy val publishSetting = publishTo <<= version { (v: String) =>
      Some( "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/".+(
         if( v.endsWith( "-SNAPSHOT")) "snapshots/" else "releases/"
      ))
   }

   lazy val credentialsSetting = credentials += Credentials( Path.userHome / ".ivy2" / ".credentials" )

   lazy val pomSettings = <licenses>
  <license>
    <name>GPL v2+</name>
    <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>

}