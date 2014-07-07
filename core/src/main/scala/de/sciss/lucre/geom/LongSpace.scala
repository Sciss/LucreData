/*
 *  LongSpace.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package geom

import annotation.tailrec
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

/**
 * Provides spaces in which coordinates are expressed using `Long` values.
 * Note that the current implementation due to performance considerations
 * requires that the coordinates are clipped to 62-bit range. That is,
 * they should be >= -0x2000000000000000L and < 0x2000000000000000L
 */
object LongSpace {
  sealed trait TwoDim extends Space[TwoDim] {
    type PointLike      = LongPoint2DLike
    type Point          = LongPoint2D
    type HyperCubeLike  = LongSquareLike
    type HyperCube      = LongSquare
  }

  implicit object TwoDim extends TwoDim {
    final val maxPoint  = LongPoint2D(Long.MaxValue, Long.MaxValue)
    final val dim       = 2

    implicit object lexicalOrder extends Ordering[LongPoint2DLike] {
      def compare(a: LongPoint2DLike, b: LongPoint2DLike): Int = {
        val ax = a.x
        val bx = b.x
        if (ax < bx) -1 else if (ax > bx) 1 else {
          val ay = a.y
          val by = b.y
          if (ay < by) -1 else if (ay > by) 1 else 0
        }
      }
    }


    implicit object pointSerializer extends ImmutableSerializer[LongPoint2D] {
      def read(in: DataInput): LongPoint2D = {
        val x = in.readLong()
        val y = in.readLong()
        LongPoint2D(x, y)
      }

      def write(p: LongPoint2D, out: DataOutput): Unit = {
        out.writeLong(p.x)
        out.writeLong(p.y)
      }
    }

    implicit object hyperCubeSerializer extends ImmutableSerializer[LongSquare] {
      def read(in: DataInput): LongSquare = {
        val cx = in.readLong()
        val cy = in.readLong()
        val extent = in.readLong()
        LongSquare(cx, cy, extent)
      }

      def write(q: LongSquare, out: DataOutput): Unit = {
        out.writeLong(q.cx)
        out.writeLong(q.cy)
        out.writeLong(q.extent)
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
  def binSplit(a: Long, b: Long): Long = binSplitRec(a, b, 0xFFFFFFFF00000000L, 16)

  @tailrec private def binSplitRec(a: Long, b: Long, mask: Long, shift: Int): Long = {
    val gt = a > (b & mask)
    if (shift == 0) {
      if (gt) mask >> 1 else mask
    } else {
      binSplitRec(a, b, if (gt) mask >> shift else mask << shift, shift >> 1)
    }
  }
}