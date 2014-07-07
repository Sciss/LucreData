/*
 *  IntCube.scala
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

import IntSpace.ThreeDim

/**
 * A three dimensional cube.
 *
 * Wikipedia: "Usually, the octant with all three positive coordinates is
 * referred to as the first octant. There is no generally used naming
 * convention for the other seven octants."
 *
 * However the article suggests (given that we count from zero):
 * - 0 (binary 000) - top-front-left
 * - 1 (binary 001) - top-back-right
 * - 2 (binary 010) - top-back-left
 * - 3 (binary 011) - top-front-left
 * - 4 (binary 100) - bottom-front-left
 * - 5 (binary 101) - bottom-back-left
 * - 6 (binary 110) - bottom-back-right
 * - 7 (binary 111) - bottom-front-right
 *
 * Obviously there is no clear connection between the orientation
 * and the binary representation. We thus prefer to chose the
 * the octants numbering in a binary fashion, assigning bit 0
 * to the x-axis, bit 1 to the y-axis, and bit 2 to
 * the z-axis, where top-front-left is 000, hence:
 *
 * - 0 (binary 000) - left-top-front
 * - 1 (binary 001) - right-top-front
 * - 2 (binary 010) - left-bottom-front
 * - 3 (binary 011) - right-bottom-front
 * - 4 (binary 100) - left-top-back
 * - 5 (binary 101) - right-top-back
 * - 6 (binary 110) - left-bottom-back
 * - 7 (binary 111) - right-bottom-back
 */
trait IntCubeLike extends HyperCube[ThreeDim] with QueryShape[BigInt, ThreeDim] {
  import ThreeDim._
  import Space.bigZero

  /**
   * X coordinate of the cube's center
   */
  def cx: Int

  /**
   * Y coordinate of the cube's center
   */
  def cy: Int

  /**
   * Z coordinate of the cube's center
   */
  def cz: Int

  /**
   * The extent is the half side length of the cube
   */
  def extent: Int

  final def top   : Int = cy - extent
  final def left  : Int = cx - extent
  final def front : Int = cz - extent
  final def bottom: Int = cy + (extent - 1)
  final def right : Int = cx + (extent - 1)
  final def back  : Int = cz + (extent - 1)

  final def orthant(idx: Int): HyperCube = {
    val e   = extent >> 1
    val dx  = if ((idx & 1) == 0) -e else e
    val dy  = if ((idx & 2) == 0) -e else e
    val dz  = if ((idx & 4) == 0) -e else e
    IntCube(cx + dx, cy + dy, cz + dz, e)
  }

  final def contains(point: PointLike): Boolean = {
    val em1 = extent - 1
    val px  = point.x
    val py  = point.y
    val pz  = point.z

    (cx - extent <= px) && (cx + em1 >= px) &&
    (cy - extent <= py) && (cy + em1 >= py) &&
    (cz - extent <= pz) && (cz + em1 >= pz)
  }

  final def contains(cube: HyperCube): Boolean = {
    val bcx   = cube.cx
    val bcy   = cube.cy
    val bcz   = cube.cz
    val be    = cube.extent
    val bem1  = be - 1
    val em1   = extent - 1

    (bcx - be >= cx - extent) && (bcx + bem1 <= cx + em1) &&
      (bcy - be >= cy - extent) && (bcy + bem1 <= cy + em1) &&
      (bcz - be >= cz - extent) && (bcz + bem1 <= cz + em1)
  }

  final def area: BigInt = {
    val s = extent.toLong << 1
    BigInt(s * s) * BigInt(s)
  }

  // -- QueryShape --

  final def overlapArea(b: HyperCube): BigInt = {
    val bcx   = b.cx
    val bcy   = b.cy
    val bcz   = b.cz
    val be    = b.extent
    val bem1  = be - 1
    val em1   = extent - 1

    val xmin = math.max(cx - extent, bcx - be).toLong
    val xmax = math.min(cx + em1, bcx + bem1).toLong
    val dx = xmax - xmin + 1
    if (dx <= 0L) return bigZero

    val ymin = math.max(cy - extent, bcy - be).toLong
    val ymax = math.min(cy + em1, bcy + bem1).toLong
    val dy = ymax - ymin + 1
    if (dy <= 0L) return bigZero

    val zmin = math.max(cz - extent, bcz - be).toLong
    val zmax = math.min(cz + em1, bcz + bem1).toLong
    val dz = zmax - zmin + 1
    if (dz <= 0L) return bigZero

    BigInt(dx * dy) * BigInt(dz)
  }

  final def isAreaGreater(a: HyperCube, b: BigInt): Boolean = a.area > b

  final def isAreaNonEmpty(area: BigInt): Boolean = area > bigZero

  final def minDistance(point: PointLike): Double = {
    math.sqrt(minDistanceSq(point).toDouble) // or use this: http://www.merriampark.com/bigsqrt.htm ?
  }

  final def maxDistance(point: PointLike): Double = {
    math.sqrt(maxDistanceSq(point).toDouble)
  }

  /**
   * The squared (euclidean) distance of the closest of the cube's corners
   * or sides to the point, if the point is outside the cube,
   * or zero, if the point is contained
   */
  final def minDistanceSq(point: PointLike): BigInt = {
    val ax  = point.x
    val ay  = point.y
    val az  = point.z
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

    val dz = if (az < cz) {
      val zmin = cz - extent
      if (az < zmin) zmin - az else 0
    } else {
      val zmax = cz + em1
      if (az > zmax) az - zmax else 0
    }

    if (dx == 0 && dy == 0 && dz == 0) bigZero
    else {
      val dxl = dx.toLong
      val dyl = dy.toLong
      val dzl = dz.toLong
      BigInt(dxl * dxl + dyl * dyl) + BigInt(dzl * dzl)
    }
  }

  /**
   * Calculates the maximum squared euclidean
   * distance to a point in the euclidean metric.
   * This is the distance (squared) to the corner which is the furthest from
   * the `point`, no matter if it lies within the hyper-cube or not.
   */
  final def maxDistanceSq(point: PointLike): BigInt = {
    val ax  = point.x
    val ay  = point.y
    val az  = point.z
    val em1 = extent - 1
    val axl = ax.toLong
    val ayl = ay.toLong
    val azl = az.toLong

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

    val dz = if (az < cz) {
      (cz + em1).toLong - azl
    } else {
      azl - (cz - extent).toLong
    }

    BigInt(dx * dx + dy * dy) + BigInt(dz * dz)
  }

  final def indexOf(a: PointLike): Int = {
    val ax  = a.x
    val ay  = a.y
    val az  = a.z
    val em1 = extent - 1

    val xpos = if (ax < cx) {
      if (ax >= cx - extent) 0 else return -1
    } else {
      if (ax <= cx + em1) 1 else return -1
    }

    val ypos = if (ay < cy) {
      if (ay >= cy - extent) 0 else return -1
    } else {
      if (ay <= cy + em1) 2 else return -1
    }

    val zpos = if (az < cz) {
      if (az >= cz - extent) 0 else return -1
    } else {
      if (az <= cz + em1) 4 else return -1
    }

    xpos | ypos | zpos
  }

  final def indexOf(b: HyperCube): Int = {
    val bcx   = b.cx
    val bcy   = b.cy
    val bcz   = b.cz
    val be    = b.extent
    val bem1  = be - 1
    val em1   = extent - 1

    val bxmin = bcx - be
    val bxmax = bcx + bem1
    val xpos  = if (bcx < cx) {
      // left?
      // not particular elegant to return in an assignment, but well, it's allowed :)
      if ((bxmin >= cx - extent) && (bxmax < cx)) 0 else return -1
    } else {
      // right?
      if ((bxmin >= cx) && (bxmax <= cx + em1)) 1 else return -1
    }

    val bymin = bcy - be
    val bymax = bcy + bem1
    val ypos  = if (bcy < cy) {
      // top?
      if ((bymin >= cy - extent) && (bymax < cy)) 0 else return -1
    } else {
      // bottom?
      if ((bymin >= cy) && (bymax <= cy + em1)) 2 else return -1
    }

    val bzmin = bcz - be
    val bzmax = bcz + bem1
    val zpos = if (bcz < cz) {
      // front?
      if ((bzmin >= cz - extent) && (bzmax < cz)) 0 else return -1
    } else {
      // back?
      if ((bzmin >= cz) && (bzmax <= cz + em1)) 4 else return -1
    }

    xpos | ypos | zpos
  }

  final def greatestInteresting(a: PointLike, b: PointLike): HyperCube = gi(a.x, a.y, a.z, 1, b)

  final def greatestInteresting(a: HyperCube, b: PointLike): HyperCube = {
    val ae = a.extent
    gi(a.cx - ae, a.cy - ae, a.cz - ae, ae << 1, b)
  }

  private def gi(aleft: Int, atop: Int, afront: Int, asize: Int, b: PointLike): HyperCube = {
    val tlx = cx - extent
    val tly = cy - extent
    val tlz = cz - extent
    val akx = aleft - tlx
    val aky = atop - tly
    val akz = afront - tlz
    val bkx = b.x - tlx
    val bky = b.y - tly
    val bkz = b.z - tlz

    var x0 = 0
    var x1 = 0
    var x2 = 0
    if (akx <= bkx) {
      x0 = akx
      x1 = akx + asize
      x2 = bkx
    } else {
      x0 = bkx
      x1 = bkx + 1
      x2 = akx
    }
    val mx = IntSpace.binSplit(x1, x2)

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

    var z0 = 0
    var z1 = 0
    var z2 = 0
    if (akz <= bkz) {
      z0 = akz
      z1 = akz + asize
      z2 = bkz
    } else {
      z0 = bkz
      z1 = bkz + 1
      z2 = akz
    }
    val mz = IntSpace.binSplit(z1, z2)

    // that means the x extent is greater (x grid more coarse).
    if (mx <= my) {
      if (mx <= mz) {
        val mxs = mx << 1
        IntCube(tlx + (x2 & mx), tly + (y0 & mxs) - mx, tlz + (z0 & mxs) - mx, -mx)
      } else {
        val mzs = mz << 1
        IntCube(tlx + (x0 & mzs) - mz, tly + (y0 & mzs) - mz, tlz + (z2 & mz), -mz)
      }
    } else {
      if (my <= mz) {
        val mys = my << 1
        IntCube(tlx + (x0 & mys) - my, tly + (y2 & my), tlz + (z0 & mys) - my, -my)
      } else {
        val mzs = mz << 1
        IntCube(tlx + (x0 & mzs) - mz, tly + (y0 & mzs) - mz, tlz + (z2 & mz), -mz)
      }
    }
  }
}

object IntCube {
  implicit def serializer = IntSpace.ThreeDim.hyperCubeSerializer
}
final case class IntCube(cx: Int, cy: Int, cz: Int, extent: Int)
  extends IntCubeLike