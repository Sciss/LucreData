import sbt._
import Keys._

object LucreDataBuild extends Build {
   lazy val lucredata = Project(
      id        = "lucredata",
      base      = file( "." ),
      settings  = standardSettings,
//      aggregate = Seq( /* structsCore, viewsCore, */ structs, views /*,  tests */ /*, full */ )
      aggregate = Seq( structsCore, txn, mutable, viewsCore, txnViews, mutableViews )
   )

//   lazy val structs = Project(
//      id        = "lucredata-structs",
//      base      = file( "structs" ), // file( "." ),
//      settings  = standardSettings,
////      dependencies = Seq( structsCore, mutable, txn )
////      aggregate = Seq( structsCore, mutable, txn )
//      aggregate = Seq( structsCore, txn, mutable )
//   )
//
//   lazy val views = Project(
//      id        = "lucredata-views",
//      base      = file( "views" ), // file( "." ),
//      settings  = standardSettings,
////      dependencies = Seq( mutableViews, txnViews )
////      aggregate = Seq( structsCore, viewsCore, mutableViews, txnViews )
//      aggregate = Seq( structsCore, txn, mutable, viewsCore, txnViews, mutableViews )
//   )

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
            "org.scalatest" %% "scalatest" % "1.7.1" % "test"
         )
      )
   )
   
   lazy val txn = Project(
      id           = "lucredata-txn",
      base         = file( "txn" ),
      dependencies = Seq( structsCore ),
      settings     = standardSettings ++ Seq(
         libraryDependencies ++= Seq(  
            "de.sciss" %% "lucrestm" % "0.21",
            "org.scalatest" %% "scalatest" % "1.7.1" % "test"
         ),
         scalacOptions ++= Seq( "-no-specialization" )   // SUCKERS!!!!!!
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

//   lazy val full = {
//      Project(
//         id           = "lucredata-full",
//         base         = file( "full" ),
//         dependencies = Seq( structs, views )
//      )
//   }

   lazy val standardSettings = Defaults.defaultSettings ++ Seq(
//      sbtVersion      := "0.12.0-M1",  // scaladoc broken in 0.11.2 when publishing

      organization    := "de.sciss",
      description     := "Transactional data structures (skip list, skip octree, total order) for Scala",
      homepage        := Some( url( "https://github.com/Sciss/LucreData" )),
      licenses        := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" )),
      version         := "0.21",
      scalaVersion    := "2.9.1",
      resolvers       ++= Seq(
         "itextpdf.com" at "http://maven.itextpdf.com",
         "Oracle Repository" at "http://download.oracle.com/maven"
      ),
      retrieveManaged := true,

      publishArtifact in (Compile, packageDoc) := false, // scaladoc is broken with sbt 0.11.2 !

      publishMavenStyle := true,
      publishArtifact in Test := false,
      pomIncludeRepository := { _ => false },
      publishSetting,
//      credentialsSetting,
      pomExtra     := pomSettings,

//      traceLevel   := 20,
      testOptions in Test += Tests.Argument( "-oF" ),
      parallelExecution in Test := false,

      scalacOptions ++= Seq( "-deprecation", "-unchecked" /*, "-no-specialization" */)
   )
   
   lazy val publishSetting = publishTo <<= version { (v: String) =>
      Some( if( v.endsWith( "-SNAPSHOT" ))
         "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
      else
         "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
      )
   }

//   lazy val credentialsSetting = credentials += Credentials( Path.userHome / ".ivy2" / ".credentials" )

   lazy val pomSettings =
<scm>
  <url>git@github.com:Sciss/LucreData.git</url>
  <connection>scm:git:git@github.com:Sciss/LucreData.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>

}
