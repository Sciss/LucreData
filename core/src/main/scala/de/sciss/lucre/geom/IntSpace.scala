/*
 *  IntSpace.scala
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

package de.sciss.lucre
package geom

import annotation.tailrec

object IntSpace {
  sealed trait TwoDim extends Space[TwoDim] {
    type PointLike      = IntPoint2DLike
    type Point          = IntPoint2D
    type HyperCubeLike  = IntSquareLike
    type HyperCube      = IntSquare
  }

  implicit object TwoDim extends TwoDim {
    val maxPoint  = IntPoint2D(Int.MaxValue, Int.MaxValue)
    val dim       = 2
    object lexicalOrder extends Ordering[IntPoint2DLike] {
      def compare(a: IntPoint2DLike, b: IntPoint2DLike): Int = {
        val ax = a.x
        val bx = b.x
        if (ax < bx) -1 else if (ax > bx) 1 else {
          val ay = a.y
          val by = b.y
          if (ay < by) -1 else if (ay > by) 1 else 0
        }
      }
    }
  }

  sealed trait ThreeDim extends Space[ThreeDim] {
    type PointLike      = IntPoint3DLike
    type Point          = IntPoint3D
    type HyperCubeLike  = IntCubeLike
    type HyperCube      = IntCube
  }

  implicit object ThreeDim extends ThreeDim {
    val maxPoint  = IntPoint3D(Int.MaxValue, Int.MaxValue, Int.MaxValue)
    val dim       = 3

    object lexicalOrder extends Ordering[IntPoint3DLike] {
      def compare(a: IntPoint3DLike, b: IntPoint3DLike): Int = {
        val ax = a.x
        val bx = b.x
        if (ax < bx) -1 else if (ax > bx) 1 else {
          val ay = a.y
          val by = b.y
          if (ay < by) -1 else if (ay > by) 1 else {
            val az = a.z
            val bz = b.z
            if (az < bz) -1 else if (az > bz) 1 else 0
          }
        }
      }
    }
  }

  final case class NDim(dim: Int) extends Space[NDim] {
    space =>

    type PointLike      = IntPointNLike
    type Point          = IntPointN
    type HyperCubeLike  = IntHyperCubeNLike
    type HyperCube      = IntHyperCubeN
    val maxPoint        = IntPointN(Vector.fill(dim)(Int.MaxValue))

    object lexicalOrder extends Ordering[IntPointNLike] {
      def compare(a: IntPointNLike, b: IntPointNLike): Int = {
        var i = 0
        val d = a.dim
        while (i < d) {
          val ai = a(i)
          val bi = b(i)
          if (ai < bi) return -1 else if (ai > bi) return 1
          i += 1
        }
        0
      }
    }
  }

  /**
   * A helper method which efficiently calculates the unique integer in an interval [a, b] which has
   * the maximum number of trailing zeros in its binary representation (a and b are integers > 0).
   * This is used by the `HyperCube` implementations to find the greatest interesting square for
   * two given children.
   *
   * Thanks to Rex Kerr and Daniel Sobral
   * ( http://stackoverflow.com/questions/6156502/integer-in-an-interval-with-maximized-number-of-trailing-zero-bits )
   */
  def binSplit(a: Int, b: Int): Int = binSplitRec(a, b, 0xFFFF0000, 8)

  @tailrec private def binSplitRec(a: Int, b: Int, mask: Int, shift: Int): Int = {
    val gt = a > (b & mask)
    if (shift == 0) {
      if (gt) mask >> 1 else mask
    } else {
      binSplitRec(a, b, if (gt) mask >> shift else mask << shift, shift >> 1)
    }
  }
}