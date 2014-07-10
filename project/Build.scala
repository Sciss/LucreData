import sbt._
import Keys._

object Build extends sbt.Build {
  lazy val stmVersion     = "2.1.0"
  lazy val pdflitzVersion = "1.1.0"

  lazy val root: Project = Project(
    id            = "lucredata",
    base          = file("."),
    aggregate     = Seq(core, views),
    dependencies  = Seq(core, views), // i.e. root = full sub project. if you depend on root, will draw all sub modules.
    settings      = Project.defaultSettings ++ Seq(
      publishArtifact in (Compile, packageBin) := false, // there are no binaries
      publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
      publishArtifact in (Compile, packageSrc) := false  // there are no sources
    )
  )

  lazy val core = Project(
    id        = "lucredata-core",
    base      = file("core"),
    settings  = Project.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "de.sciss" %% "lucrestm-core" % stmVersion,
        "de.sciss" %% "lucrestm-bdb"  % stmVersion % "test"
      )
    )
  )

  lazy val views = Project(
    id            = "lucredata-views",
    base          = file("views"),
    dependencies  = Seq(core),
    settings      = Project.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "de.sciss" %% "lucrestm-bdb" % stmVersion     % "test",
        "de.sciss" %% "pdflitz"      % pdflitzVersion % "test"
      )
    )
  )
}
