/*
 *  Quad.scala
 *  (TreeTests)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.collection.geom

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
trait CubeLike extends HyperCube[ Space.ThreeDim ] with QueryShape[ Space.ThreeDim ] {
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
}

final case class Cube( cx: Int, cy: Int, cz: Int, extent: Int )
extends CubeLike {

   import Space.ThreeDim.bigZero

   def orthant( idx: Int ) : CubeLike = {
      val e    = extent >> 1
      val dx   = if( (idx & 1) == 0 ) -e else e
      val dy   = if( (idx & 2) == 0 ) -e else e
      val dz   = if( (idx & 4) == 0 ) -e else e
      Cube( cx + dx, cy + dy, cz + dz, e )
   }

//   /**
//    * The side length is two times the extent.
//    */
//   def side : Int    = extent << 1

   def contains( point: Point3DLike ) : Boolean = {
      val em1  = extent - 1
      val px   = point.x
      val py   = point.y
      val pz   = point.z

      (cx - extent <= px) && (cx + em1 >= px) &&
      (cy - extent <= py) && (cy + em1 >= py) &&
      (cz - extent <= pz) && (cz + em1 >= pz)
   }

   def contains( cube: CubeLike ) : Boolean = {
      val bcx  = cube.cx
      val bcy  = cube.cy
      val bcz  = cube.cz
      val be   = cube.extent
      val bem1 = be - 1
      val em1  = extent - 1

      (bcx - be >= cx - extent) && (bcx + bem1 <= cx + em1) &&
      (bcy - be >= cy - extent) && (bcy + bem1 <= cy + em1) &&
      (bcz - be >= cz - extent) && (bcz + bem1 <= cz + em1)
   }

   def area : BigInt = {
      val s    = extent.toLong << 1
      BigInt( s * s ) * BigInt( s )
   }

   def overlapArea( b: CubeLike ) : BigInt = {
      val bcx  = b.cx
      val bcy  = b.cy
      val bcz  = b.cz
      val be   = b.extent
      val bem1 = be - 1
      val em1  = extent - 1

      val xmin = math.max( cx - extent, bcx - be   ).toLong
      val xmax = math.min( cx + em1,    bcx + bem1 ).toLong
      val dx   = xmax - xmin + 1
      if( dx <= 0L ) return bigZero

      val ymin = math.max( cy - extent, bcy - be   ).toLong
      val ymax = math.min( cy + em1,    bcy + bem1 ).toLong
      val dy   = ymax - ymin + 1
      if( dy <= 0L ) return bigZero

      val zmin = math.max( cz - extent, bcz - be   ).toLong
      val zmax = math.min( cz + em1,    bcz + bem1 ).toLong
      val dz   = zmax - zmin + 1
      if( dz <= 0L ) return bigZero

      BigInt( dx * dy ) * BigInt( dz )
   }

   def minDistance( point: Point3DLike ) : Double = {
      math.sqrt( minDistanceSq( point ).toDouble ) // or use this: http://www.merriampark.com/bigsqrt.htm ?
   }

   def maxDistance( point: Point3DLike ) : Double = {
      math.sqrt( maxDistanceSq( point ).toDouble )
   }


   def minDistanceSq( point: Point3DLike ) : BigInt = {
//      val px   = point.x
//      val py   = point.y
//      val l    = left
//      val t    = top
//      if( px < l ) {
//         val dx   = l.toLong - px.toLong // (l - px).toLong
//         val dxs  = dx * dx
//         if( py < t ) {
//            val dy = t.toLong - py.toLong // (t - py).toLong
//            return dxs + dy * dy
//         }
//         val b = bottom
//         if( py > b ) {
//            val dy = py.toLong - b.toLong // (py - b).toLong
//            return dxs + dy * dy
//         }
//         return dxs
//      }
//      val r = right
//      if( px > r ) {
//         val dx   = px.toLong - r.toLong // (px - r).toLong
//         val dxs  = dx * dx
//         if( py < t ) {
//            val dy = t.toLong - py.toLong // (t - py).toLong
//            return dxs + dy * dy
//         }
//         val b = bottom
//         if( py > b ) {
//            val dy = py.toLong - b.toLong // (py - b).toLong
//            return dxs + dy * dy
//         }
//         return dxs
//      }
//      if( py < t ) {
//         val dy   = t.toLong - py.toLong // (t - py).toLong
//         val dys  = dy * dy
//         if( px < l ) {
//            val dx = l.toLong - px.toLong // (l - px).toLong
//            return dx * dx + dys
//         }
//         val r = right
//         if( px > r ) {
//            val dx = px.toLong - r.toLong // (px - r).toLong
//            return dx * dx + dys
//         }
//         return dys
//      }
//      val b = bottom
//      if( py > b ) {
//         val dy   = py.toLong - b.toLong // (py - b).toLong
//         val dys  = dy * dy
//         if( px < l ) {
//            val dx = l.toLong - px.toLong // (l - px).toLong
//            return dx * dx + dys
//         }
//         val r = right
//         if( px > r ) {
//            val dx = px.toLong - r.toLong // (px - r).toLong
//            return dx * dx + dys
//         }
//         return dys
//      }
//      // if we get here, the point is inside the hyperCube
//      0L
      sys.error( "TODO" )
   }

   def maxDistanceSq( point: Point3DLike ) : BigInt = {
//      val px   = point.x
//      val py   = point.y
//      if( px < cx ) {
//         val dx   = right.toLong - px.toLong // (right - px).toLong
//         val dxs  = dx * dx
//         if( py < cy ) {   // bottom right is furthest
//            val dy   = bottom.toLong - py.toLong // (bottom - py).toLong
//            dxs + dy * dy
//         } else {          // top right is furthest
//            val dy   = py.toLong - top.toLong // (py - top).toLong
//            dxs + dy * dy
//         }
//      } else {
//         val dx   = px.toLong - left.toLong // (px - left).toLong
//         val dxs  = dx * dx
//         if( py < cy ) {   // bottom left is furthest
//            val dy   = bottom.toLong - py.toLong // (bottom - py).toLong
//            dxs + dy * dy
//         } else {          // top left is furthest
//            val dy   = py.toLong - top.toLong // (py - top).toLong
//            dxs + dy * dy
//         }
//      }
      sys.error( "TODO" )
   }

   def indexOf( a: Point3DLike ) : Int = {
      val ax = a.x
      val ay = a.y
      val az = a.z

      val xpos = if( ax < cx ) 0 else 1
      val ypos = if( ay < cy ) 0 else 2
      val zpos = if( az < cz ) 0 else 4

      xpos | ypos | zpos
   }

   def indexOf( b: CubeLike ) : Int = {
      val bcx  = b.cx
      val bcy  = b.cy
      val bcz  = b.cz
      val be   = b.extent
      val bem1 = be - 1
      val em1  = extent - 1

      val bxmin   = bcx - be
      val bxmax   = bcx + bem1
      val xpos = if( bcx < cx ) {   // left?
         // not particular elegant to return in an assignment, but well, it's allowed :)
         if( (bxmin >= cx - extent) && (bxmax < cx) ) 0 else return -1
      } else { // right?
         if( (bxmin >= cx) && (bxmax <= cx + em1) ) 1 else return -1
      }

      val bymin   = bcy - be
      val bymax   = bcy + bem1
      val ypos = if( bcy < cy ) {   // top?
         if( (bymin >= cy - extent) && (bymax < cy) ) 0 else return -1
      } else { // bottom?
         if( (bymin >= cy) && (bymax <= cy + em1) ) 2 else return -1
      }

      val bzmin   = bcz - be
      val bzmax   = bcz + bem1
      val zpos = if( bcz < cz ) {   // front?
         if( (bzmin >= cz - extent) && (bzmax < cz) ) 0 else return -1
      } else { // back?
         if( (bzmin >= cz) && (bzmax <= cz + em1) ) 2 else return -1
      }

      xpos | ypos | zpos
   }

   def greatestInteresting( a: Point3DLike, b: Point3DLike ) : CubeLike = {
      sys.error( "TODO" )
   }

   def greatestInteresting( a: CubeLike, b: Point3DLike ) : CubeLike = {
      sys.error( "TODO" )
   }
}