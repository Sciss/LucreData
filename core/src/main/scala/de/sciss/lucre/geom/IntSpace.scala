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
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}

object IntSpace {
  sealed trait TwoDim extends Space[TwoDim] {
    type PointLike      = IntPoint2DLike
    type Point          = IntPoint2D
    type HyperCubeLike  = IntSquareLike
    type HyperCube      = IntSquare
  }

  implicit object TwoDim extends TwoDim {
    final val maxPoint  = IntPoint2D(Int.MaxValue, Int.MaxValue)
    final val dim       = 2

    implicit object lexicalOrder extends Ordering[IntPoint2DLike] {
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

    implicit object pointSerializer extends ImmutableSerializer[IntPoint2D] {
      def read(in: DataInput): IntPoint2D = {
        val x = in.readInt()
        val y = in.readInt()
        IntPoint2D(x, y)
      }

      def write(p: IntPoint2D, out: DataOutput): Unit = {
        out.writeInt(p.x)
        out.writeInt(p.y)
      }
    }

    implicit object hyperCubeSerializer extends ImmutableSerializer[IntSquare] {
      def read(in: DataInput): IntSquare = {
        val cx = in.readInt()
        val cy = in.readInt()
        val extent = in.readInt()
        IntSquare(cx, cy, extent)
      }

      def write(q: IntSquare, out: DataOutput): Unit = {
        out.writeInt(q.cx)
        out.writeInt(q.cy)
        out.writeInt(q.extent)
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
    final val maxPoint  = IntPoint3D(Int.MaxValue, Int.MaxValue, Int.MaxValue)
    final val dim       = 3

    implicit object lexicalOrder extends Ordering[IntPoint3DLike] {
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

    implicit object pointSerializer extends ImmutableSerializer[IntPoint3D] {
      def read(in: DataInput): IntPoint3D = {
        val x = in.readInt()
        val y = in.readInt()
        val z = in.readInt()
        IntPoint3D(x, y, z)
      }

      def write(p: IntPoint3D, out: DataOutput): Unit = {
        out.writeInt(p.x)
        out.writeInt(p.y)
        out.writeInt(p.z)
      }
    }

    implicit object hyperCubeSerializer extends ImmutableSerializer[IntCube] {
      def read(in: DataInput): IntCube = {
        val cx  = in.readInt()
        val cy  = in.readInt()
        val cz  = in.readInt()
        val ext = in.readInt()
        IntCube(cx, cy, cz, ext)
      }

      def write(q: IntCube, out: DataOutput): Unit = {
        out.writeInt(q.cx)
        out.writeInt(q.cy)
        out.writeInt(q.cz)
        out.writeInt(q.extent)
      }
    }
  }

  object NDim {
    implicit object lexicalOrder extends Ordering[IntPointNLike] {
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

    implicit object pointSerializer extends ImmutableSerializer[NDim#Point] {
      def write(v: NDim#Point, out: DataOutput): Unit = {
        val c = v.components
        out.writeShort(c.size)
        c.foreach(out.writeInt)
      }

      def read(in: DataInput): NDim#Point = {
        val sz = in.readShort()
        val c = Vector.fill(sz)(in.readInt())
        IntPointN(c)
      }
    }

    implicit object hyperCubeSerializer extends ImmutableSerializer[NDim#HyperCube] {
      def write(v: NDim#HyperCube, out: DataOutput): Unit = {
        val c = v.components
        out.writeShort(c.size)
        c.foreach(out.writeInt)
        out.writeInt(v.extent)
      }

      def read(in: DataInput): NDim#HyperCube = {
        val sz  = in.readShort()
        val c   = Vector.fill(sz)(in.readInt())
        val ext = in.readInt()
        IntHyperCubeN(c, ext)
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

    def lexicalOrder        = NDim.lexicalOrder
    def pointSerializer     = NDim.pointSerializer
    def hyperCubeSerializer = NDim.hyperCubeSerializer
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
  def binSplit(a: Int, b: Int): Int = /* if (a <= b) */ binSplitRec(a, b, 0xFFFF0000, 8) // else binSplitRec(b, a, 0xFFFF0000, 8)

  @tailrec private def binSplitRec(a: Int, b: Int, mask: Int, shift: Int): Int = {
    val gt = a > (b & mask)
    if (shift == 0) {
      if (gt) mask >> 1 else mask
    } else {
      binSplitRec(a, b, if (gt) mask >> shift else mask << shift, shift >> 1)
    }
  }
}