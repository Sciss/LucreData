lazy val baseName       = "LucreData"
lazy val baseNameL      = baseName.toLowerCase
lazy val projectVersion = "2.4.0-SNAPSHOT"

lazy val stmVersion       = "2.1.1"
lazy val pdflitzVersion   = "1.2.1"
lazy val scalaTestVersion = "2.2.5"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Transactional data structures (skip list, skip octree, total order) for Scala",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  licenses            := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalaVersion        := "2.11.6",
  crossScalaVersions  := Seq("2.11.6", "2.10.5"),
  resolvers          ++= Seq(
    "itextpdf.com"      at "http://maven.itextpdf.com",
    "Oracle Repository" at "http://download.oracle.com/maven"
  ),
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture"),  // "-optimize" -> crashes scalac
  scalacOptions      ++= Seq("-Xelide-below", "INFO"),     // elide debug logging!
  testOptions in Test += Tests.Argument("-oDF"),
  parallelExecution in ThisBuild := false
) ++ publishSettings

lazy val root = Project(id = baseNameL, base = file(".")).
  aggregate(core, views).
  dependsOn(core, views). // i.e. root = full sub project. if you depend on root, will draw all sub modules.
  settings(commonSettings).
  settings(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings).
  settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucrestm-core" % stmVersion,
      "de.sciss" %% "lucrestm-bdb"  % stmVersion % "test"
    ),
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.lucre.data"
  )

lazy val views = Project(id = s"$baseNameL-views", base = file("views")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucrestm-bdb" % stmVersion     % "test",
      "de.sciss" %% "pdflitz"      % pdflitzVersion % "test"
    )
  )

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
<scm>
  <url>git@github.com:Sciss/{baseName}.git</url>
  <connection>scm:git:git@github.com:Sciss/{baseName}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
  }
)

// ---- ls.implicit.ly ----

// seq(lsSettings: _*)
// (LsKeys.tags   in LsKeys.lsync) := Seq("data-structures", "transactional", "spatial", "stm")
// (LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")
// (LsKeys.ghRepo in LsKeys.lsync) := Some(name.value)
