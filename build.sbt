name := "treetests"

version := "0.11-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.9.1"

resolvers += "itextpdf.com" at "http://maven.itextpdf.com"

libraryDependencies ++= Seq(
   "org.scala-tools" %% "scala-stm" % "0.3",
   "de.sciss" % "prefuse" % "0.20",
   "com.itextpdf" % "itextpdf" % "5.1.1",
   "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- publishing ----

publishTo <<= version { (v: String) =>
   Some( "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/".+(
      if( v.endsWith( "-SNAPSHOT")) "snapshots/" else "releases/"
   ))
}

pomExtra :=
<licenses>
  <license>
    <name>GPL v2+</name>
    <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

