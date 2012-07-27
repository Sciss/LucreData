/*
 *  DistanceMeasure3D.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

object IntDistanceMeasure3D {
   import IntSpace.ThreeDim
   import ThreeDim._
   import Space.{bigZero => Zero}
   import DistanceMeasure.Ops

   private type Sqr = BigInt
   private type ML = DistanceMeasure[ Long, ThreeDim ] with Ops[ Long, ThreeDim ]
   private type MS = DistanceMeasure[ Sqr,  ThreeDim ] with Ops[ Sqr,  ThreeDim ]
   private val MaxDistance : Sqr = {
      val n = BigInt( Long.MaxValue )
      n * n
   }

   /**
    * A measure that uses the euclidean squared distance
    * which is faster than the euclidean distance as the square root
    * does not need to be taken.
    */
   val euclideanSq : MS = EuclideanSq

   /**
    * A chebychev distance measure, based on the maximum of the absolute
    * distances across the first two dimensions. The 3rd dimension is ignored!
    */
   val chebyshevXY : ML = ChebyshevXY

   /**
    * An 'inverted' chebychev distance measure, based on the *minimum* of the absolute
    * distances across the first two dimensions. The 3rd dimension is ignored!
    * This is, strictly speaking, only a semi metric.
    */
   val vehsybehcXY : ML = VehsybehcXY

   private object EuclideanSq extends SqrImpl {
      override def toString = "IntDistanceMeasure3D.euclideanSq"
      def distance( a: PointLike, b: PointLike ) = b.distanceSq( a )
      def minDistance( a: PointLike, b: HyperCube ) = b.minDistanceSq( a )
      def maxDistance( a: PointLike, b: HyperCube ) = b.maxDistanceSq( a )
   }

   private sealed trait ClipLike[ @specialized( Long ) M ] extends DistanceMeasure[ M, ThreeDim ] {
      protected def underlying: DistanceMeasure[ M, ThreeDim ]
      protected def clipping: HyperCube
      def distance( a: PointLike, b: PointLike ) : M = if( clipping.contains( b )) underlying.distance(    a, b ) else maxValue
      def minDistance( a: PointLike, b: HyperCube ) : M = if( clipping.contains( b )) underlying.minDistance( a, b ) else maxValue
      def maxDistance( a: PointLike, b: HyperCube ) : M = if( clipping.contains( b )) underlying.maxDistance( a, b ) else maxValue
   }

   private final class SqrClip( protected val underlying: SqrImpl, protected val clipping: HyperCube )
   extends ClipLike[ BigInt ] with SqrImpl

   private final class LongClip( protected val underlying: LongImpl, protected val clipping: HyperCube )
   extends ClipLike[ Long ] with LongImpl

   private sealed trait ApproximateLike[ @specialized( Long ) M ] extends Impl[ M ] {
      protected def underlying: DistanceMeasure[ M, ThreeDim ]
      protected def thresh: M

      def minDistance( a: PointLike, b: HyperCube ) : M = underlying.minDistance( a, b )
      def maxDistance( a: PointLike, b: HyperCube ) : M = underlying.maxDistance( a, b )
      def distance( a: PointLike, b: PointLike ) : M = {
         val res = underlying.distance( a, b ) // b.distanceSq( a )
         if( isMeasureGreater( res, thresh )) res else zeroValue
      }
   }

   private final class SqrApproximate( protected val underlying: SqrImpl, protected val thresh: Sqr )
   extends ApproximateLike[ Sqr ] with SqrImpl

   private final class LongApproximate( protected val underlying: LongImpl, protected val thresh: Long )
   extends ApproximateLike[ Long ] with LongImpl

   private sealed trait OrthantLike[ @specialized( Long ) M ]
   extends DistanceMeasure[ M, ThreeDim ] {
      protected def underlying: DistanceMeasure[ M, ThreeDim ]
      protected def idx: Int

      private val right    = (idx & 1) != 0
      private val bottom   = (idx & 2) != 0
      private val back     = (idx & 4) != 0

      override def toString = underlying.toString + ".quadrant(" + idx + ")"

      def distance( a: PointLike, b: PointLike ) : M = {
         if( (if( right  ) b.x >= a.x else b.x <= a.x) &&
             (if( bottom ) b.y >= a.y else b.y <= a.y) &&
             (if( back   ) b.z >= a.z else b.z <= a.z) ) {

            underlying.distance( a, b )
         } else maxValue
      }

      def minDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
             (if( bottom ) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) &&
             (if( back   ) (q.cz + qem1) >= p.z else (q.cz - qe) <= p.z) ) {

            underlying.minDistance( p, q )
         } else maxValue
      }

      def maxDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
             (if( bottom ) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) &&
             (if( back   ) (q.cz - qe) >= p.z else (q.cz + qem1) <= p.z) ) {

            underlying.maxDistance( p, q )
         } else maxValue
      }
   }

   private final class SqrOrthant( protected val underlying: DistanceMeasure[ BigInt, ThreeDim ], protected val idx: Int )
   extends OrthantLike[ BigInt ] with SqrImpl

   private final class LongOrthant( protected val underlying: DistanceMeasure[ Long, ThreeDim ], protected val idx: Int )
   extends OrthantLike[ Long ] with LongImpl

   private sealed trait ExceptOrthantLike[ @specialized( Long ) M ]
   extends DistanceMeasure[ M, ThreeDim ] {
      protected def underlying: DistanceMeasure[ M, ThreeDim ]
      protected def idx: Int

      private val right    = (idx & 1) != 0
      private val bottom   = (idx & 2) != 0
      private val back     = (idx & 4) != 0

      override def toString = underlying.toString + ".exceptQuadrant(" + idx + ")"

      def distance( a: PointLike, b: PointLike ) : M = {
         if( (if( right  ) b.x <= a.x else b.x >= a.x) ||
             (if( bottom ) b.y <= a.y else b.y >= a.y) ||
             (if( back   ) b.z <= a.z else b.z >= a.z) ) {

            underlying.distance( a, b )
         } else maxValue
      }

      def minDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) <= p.x else (q.cx + qem1) >= p.x) ||
             (if( bottom ) (q.cy - qe) <= p.y else (q.cy + qem1) >= p.y) ||
             (if( back   ) (q.cz - qe) <= p.z else (q.cz + qem1) >= p.z) ) {

            underlying.minDistance( p, q )
         } else maxValue
      }

      def maxDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) <= p.x else (q.cx - qe) >= p.x) ||
             (if( bottom ) (q.cy + qem1) <= p.y else (q.cy - qe) >= p.y) ||
             (if( back   ) (q.cz + qem1) <= p.z else (q.cz - qe) >= p.z) ) {

            underlying.maxDistance( p, q )
         } else maxValue
      }
   }

   private final class SqrExceptOrthant( protected val underlying: DistanceMeasure[ BigInt, ThreeDim ], protected val idx: Int )
   extends OrthantLike[ BigInt ] with SqrImpl

   private final class LongExceptOrthant( protected val underlying: DistanceMeasure[ Long, ThreeDim ], protected val idx: Int )
   extends OrthantLike[ Long ] with LongImpl

   private object ChebyshevXY extends ChebyshevXYLike {
      override def toString = "IntDistanceMeasure3D.chebyshevXY"
      protected def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   private object VehsybehcXY extends ChebyshevXYLike {
      override def toString = "IntDistanceMeasure3D.vehsybehcXY"
      protected def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )
   }

   private sealed trait ChebyshevXYLike extends LongImpl {
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

   private sealed trait Impl[ @specialized( Long ) M ] extends Ops[ M, ThreeDim ] {
      protected def zeroValue : M
   }

   private sealed trait SqrImpl extends Impl[ Sqr ] {
//      final val manifest : Manifest[ BigInt ] = Manifest.classType[ BigInt ]( classOf[ BigInt ])
      final def newArray( size: Int ) = new Array[ BigInt ]( size )

      final def maxValue : Sqr = MaxDistance
      final def zeroValue : Sqr = Zero
      final def isMeasureZero( m: BigInt ) : Boolean = m == Zero
      final def isMeasureGreater( a: BigInt, b: BigInt ) : Boolean = a > b
      final def compareMeasure( a: BigInt, b: BigInt ) : Int = if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : MS = new SqrClip( this, quad )
      final def approximate( thresh: BigInt ) : MS = new SqrApproximate( this, thresh )
      final def orthant( idx: Int ) : MS = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new SqrOrthant( this, idx )
      }
      final def exceptOrthant( idx: Int ) : MS = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new SqrExceptOrthant( this, idx )
      }
   }

   private sealed trait LongImpl extends Impl[ Long ] {
//      final def manifest : Manifest[ Long ] = Manifest.Long
      final def newArray( size: Int ) = new Array[ Long ]( size )

      final def maxValue : Long = Long.MaxValue
      final def zeroValue : Long = 0L
      final def isMeasureZero( m: Long ) : Boolean = m == 0L
      final def isMeasureGreater( a: Long, b: Long ) : Boolean = a > b
      final def compareMeasure( a: Long, b: Long ) : Int = if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : ML = new LongClip( this, quad )
      final def approximate( thresh: Long ) : ML = new LongApproximate( this, thresh )
      final def orthant( idx: Int ) : ML = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new LongOrthant( this, idx )
      }
      final def exceptOrthant( idx: Int ) : ML = {
         require( idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")" )
         new LongExceptOrthant( this, idx )
      }
   }
}