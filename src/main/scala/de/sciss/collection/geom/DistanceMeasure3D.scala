/*
*  DistanceMeasure3D.scala
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

object DistanceMeasure3D {
   /**
    * A measure that uses the euclidean squared distance
    * which is faster than the euclidean distance as the square root
    * does not need to be taken.
    */
   val euclideanSq : DistanceMeasure[ Space.ThreeDim ] = new DistanceMeasure3D {
      def distance( a: Point3DLike, b: Point3DLike ) = b.distanceSq( a )
      def minDistance( a: Point3DLike, b: CubeLike ) = b.minDistanceSq( a )
      def maxDistance( a: Point3DLike, b: CubeLike ) = b.maxDistanceSq( a )
   }

   /**
    * A chebychev distance measure, based on the maximum of the absolute
    * distances across the first two dimensions. The 3rd dimension is ignored!
    */
   val chebyshevXY : DistanceMeasure[ Space.ThreeDim ] = new ChebyshevXYLikeDistanceMeasure {
      protected final def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   private class Clip( underlying: DistanceMeasure3D, cube: CubeLike ) extends DistanceMeasure3D {
      def distance( a: Point3DLike, b: Point3DLike ) : BigInt = if( cube.contains( b )) underlying.distance(    a, b ) else maxValue
      def minDistance( a: Point3DLike, b: CubeLike ) : BigInt = if( cube.contains( b )) underlying.minDistance( a, b ) else maxValue
      def maxDistance( a: Point3DLike, b: CubeLike ) : BigInt = if( cube.contains( b )) underlying.maxDistance( a, b ) else maxValue
   }

   private class Approximate( underlying: DistanceMeasure3D, thresh: BigInt ) extends DistanceMeasure3D {
      def minDistance( a: Point3DLike, b: CubeLike ) : BigInt = underlying.minDistance( a, b )
      def maxDistance( a: Point3DLike, b: CubeLike ) : BigInt = underlying.maxDistance( a, b )
      def distance( a: Point3DLike, b: Point3DLike ) : BigInt = {
         val res = b.distanceSq( a )
         if( res > thresh ) res else Space.ThreeDim.bigZero
      }
   }

   private class Orthant( underlying: DistanceMeasure3D, idx: Int ) extends DistanceMeasure3D {
      private val right    = (idx & 1) != 0
      private val bottom   = (idx & 2) != 0
      private val back     = (idx & 4) != 0

      def distance( a: Point3DLike, b: Point3DLike ) : BigInt = {
         if( (if( right  ) b.x >= a.x else b.x <= a.x) &&
             (if( bottom ) b.y >= a.y else b.y <= a.y) &&
             (if( back   ) b.z >= a.z else b.z <= a.z) ) {

            underlying.distance( a, b )
         } else maxValue
      }

      def minDistance( p: Point3DLike, q: CubeLike ) : BigInt = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
             (if( bottom ) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) &&
             (if( back   ) (q.cz + qem1) >= p.z else (q.cz - qe) <= p.z) ) {

            underlying.minDistance( p, q )
         } else maxValue
      }

      def maxDistance( p: Point3DLike, q: CubeLike ) : BigInt = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
             (if( bottom ) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) &&
             (if( back   ) (q.cz - qe) >= p.z else (q.cz + qem1) <= p.z) ) {

            underlying.maxDistance( p, q )
         } else maxValue
      }
   }

   private sealed trait ChebyshevXYLikeDistanceMeasure extends DistanceMeasure3D {
      protected def apply( dx: Long, dy: Long ) : Long

      def distance( a: Point3DLike, b: Point3DLike ) : BigInt = {
         val dx = math.abs( a.x.toLong - b.x.toLong )
         val dy = math.abs( a.y.toLong - b.y.toLong )
         BigInt( apply( dx, dy ))
      }
      def minDistance( a: Point3DLike, q: CubeLike ) : BigInt = {
         val px   = a.x
         val py   = a.y
         val qe   = q.extent
         val qem1 = qe - 1
         val qcx  = q.cx
         val qcy  = q.cy
         val l    = qcx - qe // q.left
         val t    = qcy - qe // q.top
         var dx   = 0L
         var dy   = 0L
         if( px < l ) {
            dx = l.toLong - px.toLong
            if( py < t ) {
               dy = t.toLong - py.toLong
            } else {
               val b = qcy + qem1 // q.bottom
               if( py > b ) {
                  dy = py.toLong - b.toLong
               }
            }
         } else {
            val r = qcx + qem1 // q.right
            if( px > r ) {
               dx   = px.toLong - r.toLong
               if( py < t ) {
                  dy = t.toLong - py.toLong
               } else {
                  val b = qcy + qem1 // q.bottom
                  if( py > b ) {
                     dy = py.toLong - b.toLong
                  }
               }
            } else if( py < t ) {
               dy = t.toLong - py.toLong
               if( px < l ) {
                  dx = l.toLong - px.toLong
               } else {
                  if( px > r ) {
                     dx = px.toLong - r.toLong
                  }
               }
            } else {
               val b = qcy + qem1   // q.bottom
               if( py > b ) {
                  dy = py.toLong - b.toLong
                  if( px < l ) {
                     dx = l.toLong - px.toLong
                  } else {
                     if( px > r ) {
                        dx = px.toLong - r.toLong
                     }
                  }
               }
            }
         }
         BigInt( apply( dx, dy ))
      }

      def maxDistance( a: Point3DLike, q: CubeLike ) : BigInt = {
         val px   = a.x
         val py   = a.y
         val qcx  = q.cx
         val qcy  = q.cy
         val qe   = q.extent
         val qem1 = qe - 1
         BigInt( if( px < qcx ) {
            val dx = (qcx + qem1).toLong - px.toLong
            val dy = if( py < qcy ) {    // bottom right is furthest
               (qcy + qem1).toLong - py.toLong
            } else {                      // top right is furthest
               py.toLong - (qcy - qe).toLong
            }
            apply( dx, dy )
         } else {
            val dx = px.toLong - (qcx - qe).toLong
            val dy = if( py < qcy ) {    // bottom left is furthest
               (qcy + qem1).toLong - py.toLong
            } else {                      // top left is furthest
               py.toLong - (qcy - qe).toLong
            }
            apply( dx, dy )
         })
      }
   }

   private sealed trait DistanceMeasure3D extends DistanceMeasure[ Space.ThreeDim ] {
      final val maxValue : BigInt = BigInt( 0x7FFFFFFFFFFFFFFFL ) * BigInt( 0x7FFFFFFFFFFFFFFFL )
      final def clip( quad: CubeLike ) : DistanceMeasure[ Space.ThreeDim ] = new Clip( this, quad )
      final def approximate( thresh: BigInt ) : DistanceMeasure[ Space.ThreeDim ] = new Approximate( this, thresh )
      final def orthant( idx: Int ) : DistanceMeasure[ Space.ThreeDim ] = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new Orthant( this, idx )
      }
   }
}