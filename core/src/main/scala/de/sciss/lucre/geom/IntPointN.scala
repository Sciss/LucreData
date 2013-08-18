/*
 *  IntPointN.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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