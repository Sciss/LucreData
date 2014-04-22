package de.sciss.lucre
package data

import org.scalatest.FunSpec
import de.sciss.lucre.geom.{IntSquare => Q, IntPoint2D => P}

/*
 To run only this suite

test-only de.sciss.lucre.data.InterestingSquareSuite

  */
class InterestingSquareSuite extends FunSpec {
  val p = Q(64, 64, 64)
  import p.{greatestInteresting => gi}

  for (shift <- 0 to 5) {
    val ext = 1 << shift
    val q   = Q(ext, ext, ext)
    for (x1 <- 0 until ext; y1 <- 0 until ext; x2 <- 0 until ext; y2 <- 0 until ext) {
      assert(gi(P(x1, y1), P(x2 + ext, y2      )) === q)
      assert(gi(P(x1, y1), P(x2,       y2 + ext)) === q)
      assert(gi(P(x1, y1), P(x2 + ext, y2 + ext)) === q)

      assert(gi(P(x2 + ext, y2      ), P(x1, y1)) === q)
      assert(gi(P(x2,       y2 + ext), P(x1, y1)) === q)
      assert(gi(P(x2 + ext, y2 + ext), P(x1, y1)) === q)
    }
  }
}