/*
 *  LongRectangle.scala
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

/** A 2D rectangular query shape. */
trait LongRectangleLike extends QueryShape[BigInt, LongSpace.TwoDim] {
  import LongSpace.TwoDim._
  import Space.bigZero

  def left  : Long
  def top   : Long
  def width : Long
  def height: Long

  final def bottom: Long = top  + (height - 1)
  final def right : Long = left + (width  - 1)

  def contains(point: PointLike): Boolean = {
    val px = point.x
    val py = point.y
    left <= px && right >= px && top <= py && bottom >= py
  }

  final def overlapArea(q: HyperCube): BigInt = {
    val l = math.max(q.left , left )
    val r = math.min(q.right, right)
    val w = r - l + 1
    if (w <= 0L) return bigZero
    val t = math.max(q.top   , top   )
    val b = math.min(q.bottom, bottom)
    val h = b - t + 1
    if (h <= 0L) return bigZero
    BigInt(w) * BigInt(h)
  }

  final def isAreaGreater(a: HyperCube, b: BigInt): Boolean = a.area > b

  def isAreaNonEmpty(area: BigInt): Boolean = area > bigZero
}

final case class LongRectangle(left: Long, top: Long, width: Long, height: Long) extends LongRectangleLike
