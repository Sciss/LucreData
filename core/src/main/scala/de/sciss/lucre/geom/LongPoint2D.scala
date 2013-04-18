/*
 *  LongPoint2D.scala
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

trait LongPoint2DLike {
  def x: Long
  def y: Long

  final def left: Long = x
  final def top : Long = y

  def distanceSq(that: LongPoint2DLike): BigInt = {
    val dx = BigInt(that.x - x)
    val dy = BigInt(that.y - y)
    dx * dx + dy * dy
  }

  /**
   * Queries the overlap of this shape with a given
   * `LongPoint2D p`. The point is considered to have
   * a side length of 1!
   *
   * @return  `true` if this shape contains or partly overlaps
   *          the given point
   */
  final def contains(p: LongPoint2DLike): Boolean = p.x == this.x && p.y == this.y

  /**
   * Returns the orientation of the given point wrt this point, according
   * to the following scheme:
   *
   * 5   4    6
   * +---+
   * 1 | 0 |  2
   * +---+
   * 9   8   10
   *
   * Therefore the horizontal orientation can be extracted
   * with `_ & 3`, and the vertical orientation with `_ >> 2`,
   * where orientation is 0 for 'parallel', 1 for 'before' and
   * '3' for 'after', so that if the orient is before or
   * after, the sign can be retrieved via `_ - 2`
   *
   * For example, if this is `LongPoint2D(4, 4)` and the query
   * point is `LongPoint2D(4, 5)`, the result is `12`. If the
   * query is `LongPoint2D(0, 0)`, the result is `5`, etc.
   */
  final def orient(b: LongPoint2DLike): Int = {
    val ax = x
    val ay = y
    val bx = b.x
    val by = b.y
    val dx = if (bx < ax) 1 else if (bx > ax) 2 else 0
    val dy = if (by < ay) 4 else if (by > ay) 8 else 0
    dx | dy
  }
}

object LongPoint2D {
  implicit def serializer = LongSpace.TwoDim.pointSerializer
}
final case class LongPoint2D(x: Long, y: Long) extends LongPoint2DLike {
  def +(p: LongPoint2D) = LongPoint2D(x + p.x, y + p.y)
  def -(p: LongPoint2D) = LongPoint2D(x - p.x, y - p.y)
}