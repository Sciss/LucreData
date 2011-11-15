name := "treetests"

version := "0.11-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.9.1"

resolvers += "itextpdf.com" at "http://maven.itextpdf.com"

libraryDependencies ++= Seq(
   "org.scala-tools" %% "scala-stm" % "0.4",
   "de.sciss" %% "lucrestm" % "0.10-SNAPSHOT",
   "de.sciss" % "prefuse" % "0.20",
   "com.itextpdf" % "itextpdf" % "5.1.1",
   "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)

retrieveManaged := true

// the guy who is responsible for specialization should be decapitated

scalacOptions ++= Seq( "-deprecation", "-unchecked" ) // , "-Ylog:icode", "-Ydebug" ) // "-no-specialization"

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

