package de.sciss.lucre.data.gui

import java.awt.EventQueue
import java.io.File
import de.sciss.lucre.stm.{Sys, InMemory, Durable}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.geom.IntPoint2D

/**
 * Options:
 * - "--db" launch with database on desktop.
 * - "--dbtmp" launch with database in temporary folder.
 * - "--memory" launch with in-memory stm
 * - "--thesis" launch with thesis chapter 5 approximate point set
 */
object InteractiveSkipOctreeApp extends App with Runnable {
  import InteractiveSkipOctreePanel._

  EventQueue.invokeLater(this)

  def run() {
    val a       = args.headOption.getOrElse("")
    val thesis  = args.contains("--thesis")

    if (a.startsWith("--db")) {
      val dir = if (a == "--dbtmp") {
        File.createTempFile("octree", "_database")
      } else {
        new File(sys.props("user.home"), "octree_database")
      }
      dir.delete()
      dir.mkdir()
      val f = new File(dir, "data")
      println(f.getAbsolutePath)
      implicit val system: Durable = Durable(BerkeleyDB.open(f))
      val model = makeModel2D(system) {
        system.step(implicit tx => system.debugListUserRecords()).foreach(println _)
      }
      if (thesis) system.step(implicit tx => addThesisPoints(model))
      makeFrame(model)

    } else {
      implicit val system: InMemory = InMemory()
      val model = makeModel2D(system) {
        println("(Consistency not checked)")
      }
      if (thesis) system.step(implicit tx => addThesisPoints(model))
      makeFrame(model)
    }
  }

  def addThesisPoints[S <: Sys[S]](model: Model2D[S])(implicit tx: S#Tx) {
    val allPoints = Vector(
      ('a',  57.5, 509.5),
      ('b', 161.5, 463.5),
      ('c', 383.5, 441.5),
      ('d', 477.5, 425.5),
      ('e', 400.5, 360.5),
      ('f', 186.5, 267.5),
      ('g', 346.5, 249.5),
      ('h', 493.5, 408.5),
      ('i', 519.5, 391.5),
      ('j', 361.5, 232.5),
      ('k', 445.5, 343.5),
      ('m', 415.5, 309.5),
      ('n', 202.5, 213.5),
      ('o', 277.5, 196.5),
      ('p', 292.5, 179.5),
      ('q', 330.5, 163.5),
      ('r', 307.5, 138.5),
      ('s', 217.5, 121.5),
      ('t', 247.5, 104.5),
      ('u', 262.5,  87.5),
      ('v', 232.5,  64.5),
      ('w', 430.5, 292.5)
    )

    val markedPoints = Vector(
      ('a',  57.5, 509.5),
      ('b', 161.5, 463.5),
      ('g', 366.5, 285.5),
      ('m', 439.5, 361.5),
      ('n', 190.5, 239.5)
    )

    val offx    = 34.5
    val offy    = 12.0
    val scale   = 500.0/512
    val marked  = true

    def adjust(p: Vector[(Char, Double, Double)]) = p.map { case (_, x, y) =>
      IntPoint2D(((x - offx) * scale + 0.5).toInt, 512 - ((y - offy) * scale + 0.5).toInt)
    }

    val ins = adjust(if (marked) markedPoints else allPoints)

    ins.foreach(model.tree += _)
  }
}