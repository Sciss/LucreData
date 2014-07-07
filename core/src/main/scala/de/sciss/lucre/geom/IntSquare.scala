/*
 *  IntSquare.scala
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

import scala.annotation.switch

trait IntSquareLike extends HyperCube[IntSpace.TwoDim] with QueryShape[Long, IntSpace.TwoDim] {
  import IntSpace.TwoDim._

  /**
   * X coordinate of the square's center
   */
  def cx: Int

  /**
   * Y coordinate of the square's center
   */
  def cy: Int

  /**
   * The extent is the half side length of the square
   */
  def extent: Int

  //   def greatestInteresting( aleft: Int, atop: Int, asize: Int, b: IntPoint2DLike ) : IntSquareLike

  final def orthant(idx: Int): HyperCube = {
    val e = extent >> 1
    (idx: @switch) match {
      case 0 => IntSquare(cx + e, cy - e, e) // ne
      case 1 => IntSquare(cx - e, cy - e, e) // nw
      case 2 => IntSquare(cx - e, cy + e, e) // sw
      case 3 => IntSquare(cx + e, cy + e, e) // se
      case _ => throw new IllegalArgumentException(idx.toString)
    }
  }

  /** The top is center-y minus the extent.
    *
    * __Note therefore,__ that the vertical coordinates
    * are considered top-down as in screen coordinates, not bottom-up!
    */
  final def top : Int = cy - extent
  final def left: Int = cx - extent

  /**
   * The bottom is defined as the center y coordinate plus
   * the extent minus one, it thus designed the 'last pixel'
   * still inside the square. This was changed from the previous
   * definition of 'cy + extent' to be able to use the full
   * 31 bit signed int space for a square without resorting
   * to long conversion.
   */
  final def bottom: Int = cy + (extent - 1)

  /**
   * The right is defined as the center x coordinate plus
   * the extent minus one, it thus designed the 'last pixel'
   * still inside the square. This was changed from the previous
   * definition of 'cx + extent' to be able to use the full
   * 31 bit signed int space for a square without resorting
   * to long conversion.
   */
  final def right: Int = cx + (extent - 1)

  /**
   * The side length is two times the extent. Note that this may overflow if the extent
   * is greater than `0x3FFFFFFF`.
   */
  final def side: Int = extent << 1

  final def contains(point: PointLike): Boolean = {
    val px = point.x
    val py = point.y
    left <= px && right >= px && top <= py && bottom >= py
  }

  /**
   * Checks whether a given square is fully contained in this square.
   * This is also the case if their bounds full match.
   */
  final def contains(quad: HyperCube): Boolean =
    quad.left >= left && quad.top >= top && quad.right <= right && quad.bottom <= bottom

  final def area: Long = {
    val sd = extent.toLong << 1
    sd * sd
  }

  // -- QueryShape --

  final def overlapArea(q: HyperCube): Long = {
    val l = math.max(q.left, left).toLong
    val r = math.min(q.right, right).toLong
    val w = r - l + 1 // (r - l).toLong + 1
    if (w <= 0L) return 0L
    val t = math.max(q.top, top).toLong
    val b = math.min(q.bottom, bottom).toLong
    val h = b - t + 1 // (b - t).toLong + 1
    if (h <= 0L) return 0L
    w * h
  }

  final def isAreaGreater(a: HyperCube, b: Long): Boolean = a.area > b

  final def isAreaNonEmpty(area: Long): Boolean = area > 0L

  /**
   * Calculates the minimum distance to a point in the euclidean metric.
   * This calls `minDistanceSq` and then takes the square root.
   */
  final def minDistance(point: PointLike): Double = math.sqrt(minDistanceSq(point))

  /**
   * Calculates the maximum distance to a point in the euclidean metric.
   * This calls `maxDistanceSq` and then takes the square root.
   */
  final def maxDistance(point: PointLike): Double = math.sqrt(maxDistanceSq(point))

  /**
   * The squared (euclidean) distance of the closest of the square's corners
   * or sides to the point, if the point is outside the square,
   * or zero, if the point is contained
   */
  final def minDistanceSq(point: PointLike): Long = {
    val ax = point.x
    val ay = point.y
    val em1 = extent - 1

    val dx = if (ax < cx) {
      val xmin = cx - extent
      if (ax < xmin) xmin - ax else 0
    } else {
      val xmax = cx + em1
      if (ax > xmax) ax - xmax else 0
    }

    val dy = if (ay < cy) {
      val ymin = cy - extent
      if (ay < ymin) ymin - ay else 0
    } else {
      val ymax = cy + em1
      if (ay > ymax) ay - ymax else 0
    }

    if (dx == 0 && dy == 0) 0L
    else {
      val dxl = dx.toLong
      val dyl = dy.toLong
      dxl * dxl + dyl * dyl
    }
  }

  /**
   * Calculates the maximum squared distance to a point in the euclidean metric.
   * This is the distance (squared) to the corner which is the furthest from
   * the `point`, no matter if it lies within the square or not.
   */
  final def maxDistanceSq(point: PointLike): Long = {
    val ax = point.x
    val ay = point.y
    val em1 = extent - 1
    val axl = ax.toLong
    val ayl = ay.toLong

    val dx = if (ax < cx) {
      (cx + em1).toLong - axl
    } else {
      axl - (cx - extent).toLong
    }

    val dy = if (ay < cy) {
      (cy + em1).toLong - ayl
    } else {
      ayl - (cy - extent).toLong
    }

    dx * dx + dy * dy
  }

  /**
   * Determines the quadrant index of a point `a`.
   *
   * @return  the index of the quadrant (beginning at 0), or -1 if `a` lies
   *          outside of this square.
   */
  final def indexOf(a: PointLike): Int = {
    val ax = a.x
    val ay = a.y
    if (ay < cy) {
      // north
      if (ax >= cx) {
        // east
        if (right >= ax && top <= ay) 0 else -1 // ne
      } else {
        // west
        if (left <= ax && top <= ay) 1 else -1 // -2   // nw
      }
    } else {
      // south
      if (ax < cx) {
        // west
        if (left <= ax && bottom >= ay) 2 else -1 // -3   // sw
      } else {
        // east
        if (right >= ax && bottom >= ay) 3 else -1 // -4   // se
      }
    }
  }

  /**
   * Determines the quadrant index of another internal square `aq`.
   *
   * @return  the index of the quadrant (beginning at 0), or -1 if `aq` lies
   *          outside of this square.
   */
  final def indexOf(aq: HyperCube): Int = {
    val atop = aq.top
    if (atop < cy) {
      // north
      if (top <= atop && aq.bottom < cy) {
        val aleft = aq.left
        if (aleft >= cx) {
          // east
          if (right >= aq.right) 0 else -1 // ne
        } else {
          // west
          if (left <= aleft && aq.right < cx) 1 else -1 // nw
        }
      } else -1
    } else {
      // south
      if (bottom >= aq.bottom && atop >= cy) {
        val aleft = aq.left
        if (aleft < cx) {
          // west
          if (left <= aleft && aq.right < cx) 2 else -1 // sw
        } else {
          // east
          if (right >= aq.right) 3 else -1 // se
        }
      } else -1
    }
  }

  final def greatestInteresting(a: PointLike, b: PointLike): HyperCube = gi(a.x, a.y, 1, b)

  final def greatestInteresting(a: HyperCube, b: PointLike): HyperCube =
    gi(a.left, a.top, a.extent << 1, b) // a.extent << 1 can exceed 31 bit -- but it seems to work :-/

  private def gi(aleft: Int, atop: Int, asize: Int, b: PointLike): HyperCube = {
    val tlx = cx - extent
    val tly = cy - extent
    val akx = aleft - tlx
    val aky = atop  - tly
    val bkx = b.x   - tlx
    val bky = b.y   - tly

    // var x0 = 0  // smallest of akx and bkx
    var x1 = 0  //
    var x2 = 0  // largest  of akx and bkx
    if (akx <= bkx) {
      // x0 = akx
      x1 = akx + asize
      x2 = bkx
    } else {
      // x0 = bkx
      x1 = bkx + 1
      x2 = akx
    }
    val mx = IntSpace.binSplit(x1, x2)  // bitmask for x coordinate

    var y0 = 0
    var y1 = 0
    var y2 = 0
    if (aky <= bky) {
      y0 = aky
      y1 = aky + asize
      y2 = bky
    } else {
      y0 = bky
      y1 = bky + 1
      y2 = aky
    }
    val my = IntSpace.binSplit(y1, y2)

    // a smaller mask means that more lsb's are zero, hence it's a coarser mask
    if (mx <= my) {
      // IntSquare(tlx + (x2 & mx),             tly + (y0 & (mx << 1)) - mx, -mx)
      val mx2 = mx << 1
      IntSquare(tlx + (x2 & mx2) - mx, tly + (y2 & mx2) - mx, -mx)
    } else {
      // IntSquare(tlx + (x0 & (my << 1)) - my, tly + (y2 & my),             -my)
      val my2 = my << 1
      IntSquare(tlx + (x2 & my2) - my, tly + (y2 & my2) - my, -my)
    }
  }
}

final case class IntSquare(cx: Int, cy: Int, extent: Int) extends IntSquareLike
