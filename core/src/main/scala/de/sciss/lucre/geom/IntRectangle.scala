/*
 *  IntRectangle.scala
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

package de.sciss.lucre
package geom

/** A 2D rectangular query shape. */
trait IntRectangleLike extends QueryShape[Long, IntSpace.TwoDim] {
  import IntSpace.TwoDim._

  def left : Int
  def top  : Int
  def width: Int
  def height: Int

  final def bottom: Int = top  + (height - 1)
  final def right : Int = left + (width - 1)

  def contains(point: PointLike): Boolean = {
    val px = point.x
    val py = point.y
    left <= px && right >= px && top <= py && bottom >= py
  }

  final def overlapArea(q: HyperCube): Long = {
    val l = math.max(q.left , left ).toLong
    val r = math.min(q.right, right).toLong
    val w = r - l + 1
    if (w <= 0L) return 0L
    val t = math.max(q.top   , top   ).toLong
    val b = math.min(q.bottom, bottom).toLong
    val h = b - t + 1
    if (h <= 0L) return 0L
    w * h
  }

  final def isAreaGreater(a: HyperCube, b: Long): Boolean = a.area > b

  def isAreaNonEmpty(area: Long): Boolean = area > 0L
}

final case class IntRectangle(left: Int, top: Int, width: Int, height: Int) extends IntRectangleLike
