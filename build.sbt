name := "LucreData"

version in ThisBuild := "2.2.0-SNAPSHOT"

organization in ThisBuild := "de.sciss"

description in ThisBuild := "Transactional data structures (skip list, skip octree, total order) for Scala"

homepage in ThisBuild <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

licenses in ThisBuild := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion in ThisBuild := "2.10.2"

resolvers in ThisBuild ++= Seq(
  "itextpdf.com" at "http://maven.itextpdf.com",
  "Oracle Repository" at "http://download.oracle.com/maven"
)

libraryDependencies in ThisBuild ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

retrieveManaged in ThisBuild := true

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature")  // "-optimize" -> crashes scalac

scalacOptions in ThisBuild += "-no-specialization"    // specialization still broken in Scala 2.10

scalacOptions in ThisBuild ++= Seq("-Xelide-below", "INFO")     // elide debug logging!

testOptions in Test += Tests.Argument("-oDF")

parallelExecution in ThisBuild := false

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
)

buildInfoPackage := "de.sciss.lucre.data"

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild <<= version { v =>
  Some(if (v endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )
}

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild <<= name { n =>
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

// ---- ls.implicit.ly ----

seq(lsSettings: _*)

(LsKeys.tags   in LsKeys.lsync) := Seq("data-structures", "transactional", "spatial", "stm")

(LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")

(LsKeys.ghRepo in LsKeys.lsync) <<= name(Some(_))
