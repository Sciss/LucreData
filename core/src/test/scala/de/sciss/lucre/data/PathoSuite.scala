package de.sciss.lucre.data

import de.sciss.lucre.stm.InMemory
import de.sciss.lucre.geom.{IntSquare, IntPoint2D, IntSpace}
import org.scalatest.FunSpec

/*
 To run only this suite

test-only de.sciss.lucre.data.PathoSuite

  */
class PathoSuite extends FunSpec {
  type S  = InMemory
  type D  = IntSpace.TwoDim
  implicit val view = (p: IntPoint2D, _: S#Tx) => p
  val i   = InMemory()
  val j   = 8

  i.step { implicit tx =>
    val cube  = IntSquare(j >> 1, j >> 1, j >> 1)
    val tree  = SkipOctree.empty[S, D, IntPoint2D](cube)
    val ins   = (0 until j).map { i => IntPoint2D(j >> 1, i) }
    // val v     = IntPoint2D(j - 1, 0)

    //val ins   = Vector.fill(j)(IntPoint2D(r.nextInt(j), r.nextInt(j)))
    ins.foreach(tree += _)
    // tree.nearestNeighbor(v, IntDistanceMeasure2D.euclideanSq)
  }
}
