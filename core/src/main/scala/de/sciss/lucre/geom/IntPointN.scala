/*
 *  IntPointN.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.geom

import collection.immutable.{IndexedSeq => Vec}
import IntSpace.NDim

trait IntPointNLike {
  def components: Vec[Int]

  final def apply(idx: Int): Int  = components(idx)
  final def dim: Int              = components.size

  final def distanceSq(that: NDim#PointLike): BigInt = {
    var sqrSum = Space.bigZero
    var idx = 0
    while (idx < dim) {
      val delta = that(idx) - this(idx)
      sqrSum += delta * delta
      idx += 1
    }
    sqrSum
  }
}

object IntPointN {
  implicit def serializer = IntSpace.NDim.pointSerializer
}
final case class IntPointN(components: Vec[Int]) extends IntPointNLike {
  // if( components.size != dim ) throw new IllegalArgumentException( "Expected " + dim + " components: " + components )

  def +(that: NDim#Point) = IntPointN(Vector.tabulate(dim)(idx => this(idx) + that(idx)))
  def -(that: NDim#Point) = IntPointN(Vector.tabulate(dim)(idx => this(idx) - that(idx)))
}