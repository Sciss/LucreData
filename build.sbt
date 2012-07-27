name := "LucreData"

version in ThisBuild := "0.34-SNAPSHOT"

organization in ThisBuild := "de.sciss"

description in ThisBuild := "Transactional data structures (skip list, skip octree, total order) for Scala"

homepage in ThisBuild := Some( url( "https://github.com/Sciss/LucreData" ))

licenses in ThisBuild := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion in ThisBuild := "2.9.2"

crossScalaVersions in ThisBuild := Seq( "2.10.0-M6", "2.9.2" )

resolvers in ThisBuild ++= Seq(
   "Sonatype OSS Releases" at "https://oss.sonatype.org/content/groups/public",
   "itextpdf.com" at "http://maven.itextpdf.com",
   "Oracle Repository" at "http://download.oracle.com/maven"
)

libraryDependencies in ThisBuild <+= scalaVersion { sv =>
   val v = sv match {
      case "2.10.0-M6" => "1.9-2.10.0-M6-B2"
      case _ => "1.8"
   }
   "org.scalatest" %% "scalatest" % v % "test"
}

scalacOptions in ThisBuild ++= Seq( "-deprecation", "-unchecked", "-no-specialization" )

testOptions in Test += Tests.Argument( "-oDF" )

parallelExecution in ThisBuild := false

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild <<= version { (v: String) =>
   Some( if( v.endsWith( "-SNAPSHOT" ))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild :=
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

// ---- ls.implicit.ly ----

seq( lsSettings: _* )

(LsKeys.tags   in LsKeys.lsync) := Seq( "data-structures", "transactional", "spatial", "stm" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "LucreData" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))
