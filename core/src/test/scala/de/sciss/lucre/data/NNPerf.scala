package de.sciss.lucre
package data

import stm.InMemory
import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D, IntSquare, IntSpace}

object NNPerf extends App {
  type S  = InMemory
  type D  = IntSpace.TwoDim
  implicit val view = (p: IntPoint2D, _: S#Tx) => p
  val i   = InMemory()
  var j   = 32
  val r   = new util.Random(0L)
  while (j <= 262144) {
    i.step { implicit tx =>
      val cube  = IntSquare(j >> 1, j >> 1, j >> 1)
      val tree  = SkipOctree.empty[S, D, IntPoint2D](cube)
      //val ins   = (0 until j by 1).map { i => IntPoint2D(i, j >> 1) }
      val ins   = Vector.fill(j)(IntPoint2D(r.nextInt(j), r.nextInt(j)))
      ins.foreach(tree += _)
      println(s"\nSize = $j")
      tree.nearestNeighbor(IntPoint2D(0, j - 1), IntDistanceMeasure2D.euclideanSq)
    }

    j <<= 1
  }
}