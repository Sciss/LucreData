import sbt._
import Keys._

object Build extends sbt.Build {
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

  // convert the base version to a compatible version for
  // library dependencies. e.g. `"1.3.1"` -> `"1.3.+"`
  object Compatible {
    def unapply(v: String) = {
      require(v.count(_ == '.') == 2)
      val i = v.lastIndexOf('.') + 1
      val c = v.substring(0, i) + "+"
      Some(c)
    }
  }

  lazy val core = Project(
    id        = "lucredata-core",
    base      = file("core"),
    settings  = Project.defaultSettings ++ Seq(
      libraryDependencies <++= version { case Compatible(v) => Seq(
        "de.sciss" %% "lucrestm-core" % v,
        "de.sciss" %% "lucrestm-bdb"  % v % "test"
      )}
    )
  )

  lazy val views = Project(
    id            = "lucredata-views",
    base          = file("views"),
    dependencies  = Seq(core),
    settings      = Project.defaultSettings ++ Seq(
      libraryDependencies <++= version { case Compatible(v) => Seq(
        "de.sciss" %% "lucrestm-bdb" % v % "test",
        "com.itextpdf" % "itextpdf" % "5.3.2"
      )}
    )
  )
}
