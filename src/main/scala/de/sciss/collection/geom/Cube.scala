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
   def cx: Int
   def cy: Int
   def cz: Int
   def extent: Int
}

final case class Cube( cx: Int, cy: Int, cz: Int, extent: Int )
extends CubeLike {
//   import Cube._

   def orthant( idx: Int ) : CubeLike = {
      val e    = extent >> 1
      val dx   = if( (idx & 1) == 0 ) -e else e
      val dy   = if( (idx & 2) == 0 ) -e else e
      val dz   = if( (idx & 4) == 0 ) -e else e
      Cube( cx + dx, cy + dy, cz + dz, e )
   }

//   def top : Int     = cy - extent
//   def left : Int    = cx - extent
//
//   /**
//    * The bottom is defined as the center y coordinate plus
//    * the extent minus one, it thus designed the 'last pixel'
//    * still inside the quad. This was changed from the previous
//    * definition of 'cy + extent' to be able to use the full
//    * 31 bit signed int space for a quad without resorting
//    * to long conversion.
//    */
//   override def bottom : Int  = cy + (extent - 1)
//   /**
//    * The right is defined as the center x coordinate plus
//    * the extent minus one, it thus designed the 'last pixel'
//    * still inside the quad. This was changed from the previous
//    * definition of 'cx + extent' to be able to use the full
//    * 31 bit signed int space for a quad without resorting
//    * to long conversion.
//    */
//   override def right : Int   = cx + (extent - 1)

   /**
    * The side length is two times the extent.
    */
   def side : Int    = extent << 1

   def contains( point: Point3DLike ) : Boolean = {
      val px = point.x
      val py = point.y
      val pz = point.z
      sys.error( "TODO" )
//      (left <= px) && (right >= px) && (top <= py) && (bottom >= py) && ...
   }

   def contains( quad: CubeLike ) : Boolean = {
//      quad.left >= left && quad.top >= top && quad.right <= right && quad.bottom <= bottom
      sys.error( "TODO" )
   }

   def area : BigInt = {
      val s    = side
      val sd   = s.toLong
      val ssq  = BigInt( sd * sd )
      BigInt( sd * sd ) * BigInt( s )
   }

   def overlapArea( q: CubeLike ) : BigInt = {
      sys.error( "TODO" )
//      val l = math.max( q.left, left ).toLong
//      val r = math.min( q.right, right ).toLong
//      val w = r - l + 1 // (r - l).toLong + 1
//      if( w <= 0L ) return 0L
//      val t = math.max( q.top, top ).toLong
//      val b = math.min( q.bottom, bottom ).toLong
//      val h = b - t + 1 // (b - t).toLong + 1
//      if( h <= 0L ) return 0L
//      w * h
   }

   def minDistance( point: Point3DLike ) : Double = sys.error( "TODO" ) // math.pow( minDistanceSq( point ), 1.0/3 )
   def maxDistance( point: Point3DLike ) : Double = sys.error( "TODO" ) // math.pow( maxDistanceSq( point ), 1.0/3 )

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
//      // if we get here, the point is inside the quad
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
      sys.error( "TODO" )
//      val ax   = a.x
//      val ay   = a.y
//      if( ay < cy ) {      // north
//         if( ax >= cx ) {  // east
//            if( right >= ax && top <= ay ) 0 else -1   // ne
//         } else {             // west
//            if( left <= ax && top <= ay ) 1 else -2   // nw
//         }
//      } else {                // south
//         if( ax < cx ) {   // west
//            if( left <= ax && bottom >= ay ) 2 else -3   // sw
//         } else {             // east
//            if( right >= ax && bottom >= ay ) 3 else -4   // se
//         }
//      }
   }

   def indexOf( aq: CubeLike ) : Int = {
      sys.error( "TODO" )
//      val atop = aq.top
//      if( atop < cy ) {       // north
//         if( top <= atop && aq.bottom <= cy ) {
//            val aleft = aq.left
//            if( aleft >= cx ) {  // east
//               if( right >= aq.right ) 0 else -1  // ne
//            } else {             // west
//               if( left <= aleft && aq.right <= cx ) 1 else -1  // nw
//            }
//         } else -1
//      } else {                // south
//         if( bottom >= aq.bottom && atop >= cy ) {
//            val aleft = aq.left
//            if( aleft < cx ) {   // west
//               if( left <= aleft && aq.right <= cx ) 2 else -1   // sw
//            } else {             // east
//               if( right >= aq.right ) 3 else -1    // se
//            }
//         } else -1
//      }
   }

   def greatestInteresting( a: Point3DLike, b: Point3DLike ) : CubeLike = sys.error( "TODO" )

   def greatestInteresting( a: CubeLike, b: Point3DLike ) : CubeLike = sys.error( "TODO" )
}