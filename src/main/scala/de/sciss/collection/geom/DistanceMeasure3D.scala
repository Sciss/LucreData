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
   import Space.ThreeDim
   import ThreeDim._

   /**
    * A measure that uses the euclidean squared distance
    * which is faster than the euclidean distance as the square root
    * does not need to be taken.
    */
   val euclideanSq : DistanceMeasure[ BigInt, ThreeDim ] = EuclideanSq

   /**
    * A chebychev distance measure, based on the maximum of the absolute
    * distances across the first two dimensions. The 3rd dimension is ignored!
    */
   val chebyshevXY : DistanceMeasure[ Long, ThreeDim ] = ChebyshevXY

   /**
    * An 'inverted' chebychev distance measure, based on the *minimum* of the absolute
    * distances across the first two dimensions. The 3rd dimension is ignored!
    * This is, strictly speaking, only a semi metric.
    */
   val vehsybehcXY : DistanceMeasure[ Long, ThreeDim ] = VehsybehcXY

   private object EuclideanSq extends ImplBigInt {
      override def toString = "DistanceMeasure3D.euclideanSq"
      def distance( a: PointLike, b: PointLike ) = b.distanceSq( a )
      def minDistance( a: PointLike, b: HyperCube ) = b.minDistanceSq( a )
      def maxDistance( a: PointLike, b: HyperCube ) = b.maxDistanceSq( a )
   }

   private sealed trait ClipLike[ @specialized( Long ) Area ] extends DistanceMeasure[ Area, ThreeDim ] {
      protected def underlying: DistanceMeasure[ Area, ThreeDim ]
      protected def clipping: HyperCube
      def distance( a: PointLike, b: PointLike ) : Area = if( clipping.contains( b )) underlying.distance(    a, b ) else maxValue
      def minDistance( a: PointLike, b: HyperCube ) : Area = if( clipping.contains( b )) underlying.minDistance( a, b ) else maxValue
      def maxDistance( a: PointLike, b: HyperCube ) : Area = if( clipping.contains( b )) underlying.maxDistance( a, b ) else maxValue
   }

   private final class ClipBigInt( protected val underlying: ImplBigInt, protected val clipping: HyperCube )
   extends ClipLike[ BigInt ] with ImplBigInt

   private final class ClipLong( protected val underlying: ImplLong, protected val clipping: HyperCube )
   extends ClipLike[ Long ] with ImplLong

   private final class ApproximateBigInt( underlying: ImplBigInt, thresh: BigInt ) extends ImplBigInt {
      def minDistance( a: PointLike, b: HyperCube ) : BigInt = underlying.minDistance( a, b )
      def maxDistance( a: PointLike, b: HyperCube ) : BigInt = underlying.maxDistance( a, b )
      def distance( a: PointLike, b: PointLike ) : BigInt = {
         val res = b.distanceSq( a )
         if( res > thresh ) res else ThreeDim.bigZero
      }
   }

//   private final class ApproximateLong( underlying: ImplLong, thresh: Long ) extends ImplLong {
//      def minDistance( a: Point, b: HyperCube ) : Long = underlying.minDistance( a, b )
//      def maxDistance( a: Point, b: HyperCube ) : Long = underlying.maxDistance( a, b )
//      def distance( a: Point, b: Point ) : Long = {
//         val res = b.distanceSq( a )
//         if( res > thresh ) res else 0L
//      }
//   }

   private sealed trait OrthantLike[ @specialized( Long ) Area ]
   extends DistanceMeasure[ Area, ThreeDim ] {
      protected def underlying: DistanceMeasure[ Area, ThreeDim ]
      protected def idx: Int

      private val right    = (idx & 1) != 0
      private val bottom   = (idx & 2) != 0
      private val back     = (idx & 4) != 0

      def distance( a: PointLike, b: PointLike ) : Area = {
         if( (if( right  ) b.x >= a.x else b.x <= a.x) &&
             (if( bottom ) b.y >= a.y else b.y <= a.y) &&
             (if( back   ) b.z >= a.z else b.z <= a.z) ) {

            underlying.distance( a, b )
         } else maxValue
      }

      def minDistance( p: PointLike, q: HyperCube ) : Area = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
             (if( bottom ) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) &&
             (if( back   ) (q.cz + qem1) >= p.z else (q.cz - qe) <= p.z) ) {

            underlying.minDistance( p, q )
         } else maxValue
      }

      def maxDistance( p: PointLike, q: HyperCube ) : Area = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
             (if( bottom ) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) &&
             (if( back   ) (q.cz - qe) >= p.z else (q.cz + qem1) <= p.z) ) {

            underlying.maxDistance( p, q )
         } else maxValue
      }
   }

   private final class OrthantBigInt( protected val underlying: DistanceMeasure[ BigInt, ThreeDim ], protected val idx: Int )
   extends OrthantLike[ BigInt ] with ImplBigInt

   private final class OrthantLong( protected val underlying: DistanceMeasure[ Long, ThreeDim ], protected val idx: Int )
   extends OrthantLike[ Long ] with ImplLong

   private object ChebyshevXY extends ChebyshevXYLike {
      override def toString = "DistanceMeasure3D.chebyshevXY"
      protected def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   private object VehsybehcXY extends ChebyshevXYLike {
      override def toString = "DistanceMeasure3D.vehsybehcXY"
      protected def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )
   }

   private sealed trait ChebyshevXYLike extends ImplLong {
      protected def apply( dx: Long, dy: Long ) : Long

      def distance( a: PointLike, b: PointLike ) : Long = {
         val dx = math.abs( a.x.toLong - b.x.toLong )
         val dy = math.abs( a.y.toLong - b.y.toLong )
         apply( dx, dy )
      }
      def minDistance( a: PointLike, q: HyperCube ) : Long = {
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
         apply( dx, dy )
      }

      def maxDistance( a: PointLike, q: HyperCube ) : Long = {
         val px   = a.x
         val py   = a.y
         val qcx  = q.cx
         val qcy  = q.cy
         val qe   = q.extent
         val qem1 = qe - 1
         if( px < qcx ) {
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
         }
      }
   }

   private sealed trait ImplBigInt extends DistanceMeasure[ BigInt, ThreeDim ] {
      final val manifest : Manifest[ BigInt ] = Manifest.classType[ BigInt ]( classOf[ BigInt ])

      final val maxValue : BigInt = BigInt( 0x7FFFFFFFFFFFFFFFL ) * BigInt( 0x7FFFFFFFFFFFFFFFL )
      final def isMeasureZero( m: BigInt ) : Boolean = m == ThreeDim.bigZero
      final def isMeasureGreater( a: BigInt, b: BigInt ) : Boolean = a > b
      final def compareMeasure( a: BigInt, b: BigInt ) : Int = if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : DistanceMeasure[ BigInt, ThreeDim ] = new ClipBigInt( this, quad )
      final def approximate( thresh: BigInt ) : DistanceMeasure[ BigInt, ThreeDim ] = new ApproximateBigInt( this, thresh )
      final def orthant( idx: Int ) : DistanceMeasure[ BigInt, ThreeDim ] = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new OrthantBigInt( this, idx )
      }
   }

   private sealed trait ImplLong extends DistanceMeasure[ Long, ThreeDim ] {
      final def manifest : Manifest[ Long ] = Manifest.Long

      final def maxValue : Long = Long.MaxValue
      final def isMeasureZero( m: Long ) : Boolean = m == 0L
      final def isMeasureGreater( a: Long, b: Long ) : Boolean = a > b
      final def compareMeasure( a: Long, b: Long ) : Int = if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : DistanceMeasure[ Long, ThreeDim ] = new ClipLong( this, quad )
      final def approximate( thresh: Long ) : DistanceMeasure[ Long, ThreeDim ] = sys.error( "TODO" ) // new ApproximateLong( this, thresh )
      final def orthant( idx: Int ) : DistanceMeasure[ Long, ThreeDim ] = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new OrthantLong( this, idx )
      }
   }
}