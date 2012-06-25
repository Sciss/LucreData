/*
 *  LongDistanceMeasure2D.scala
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

object LongDistanceMeasure2D {
   import LongSpace.TwoDim
   import TwoDim._
   import Space.{bigZero => Zero}
   import DistanceMeasure.Ops

   private type Sqr        = BigInt
   private type ML         = Ops[ Long, TwoDim ]
   private type MS         = Ops[ Sqr, TwoDim ]
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
    * distances across all dimensions.
    */
   val chebyshev : ML = Chebyshev

   /**
    * An 'inverted' chebychev distance measure, based on the *minimum* of the absolute
    * distances across all dimensions. This is, strictly speaking, only a semi metric.
    */
   val vehsybehc : ML = Vehsybehc

   private object Chebyshev extends ChebyshevLike {
      override def toString = "LongDistanceMeasure2D.chebyshev"
      protected def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   private object Vehsybehc extends ChebyshevLike {
      override def toString = "LongDistanceMeasure2D.vehsybehc"
      protected def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )
   }

   private object EuclideanSq extends SqrImpl {
      override def toString = "LongDistanceMeasure2D.euclideanSq"
      def distance( a: PointLike, b: PointLike )    = b.distanceSq( a )
      def minDistance( a: PointLike, b: HyperCube ) = b.minDistanceSq( a )
      def maxDistance( a: PointLike, b: HyperCube ) = b.maxDistanceSq( a )
   }

   /**
    * A 'next event' search when the quadtree is used to store spans (intervals).
    * It assumes that a span or interval is represented by a point whose x coordinate
    * corresponds to the span's start and whose y coordinate corresponds to the span's stop.
    * Furthermore, it allows for spans to be unbounded: A span which does not have a defined
    * start, should use `quad.left` as the x coordinate, and a span which does not have a defined
    * stop, should use `quad.right` as the y coordinate. A span denoting the special value 'void'
    * (no extent) can be encoded by giving it `quad.right` as x coordinate.
    *
    * The measure searches for the next 'event' beginning from the query point which is supposed
    * to have `x == y == query-time point`. It finds the closest span start _or_ span stop which
    * is greater than or equal to the query-time point, i.e. the nearest neighbor satisfying
    * `qx >= x || qy >= y` (given the special treatment of unbounded coordinates).
    *
    * @param quad the tree's root square which is used to deduce the special values for representing unbounded spans
    *
    * @return  the measure instance
    */
   def nextSpanEvent( quad: IntSquare ) : ML = new NextSpanEvent( quad )

   /**
    * A 'previous event' search when the quadtree is used to store spans (intervals).
    * It assumes that a span or interval is represented by a point whose x coordinate
    * corresponds to the span's start and whose y coordinate corresponds to the span's stop.
    * Furthermore, it allows for spans to be unbounded: A span which does not have a defined
    * start, should use `quad.left` as the x coordinate, and a span which does not have a defined
    * stop, should use `quad.right` as the y coordinate. A span denoting the special value 'void'
    * (no extent) can be encoded by giving it `quad.right` as x coordinate.
    *
    * The measure searches for the previous 'event' beginning from the query point which is supposed
    * to have `x == y == query-time point`. It finds the closest span start _or_ span stop which
    * is smaller than or equal to the query-time point, i.e. the nearest neighbor satisfying
    * `qx <= x || qy <= y` (given the special treatment of unbounded coordinates).
    *
    * @param quad the tree's root square which is used to deduce the special values for representing unbounded spans
    *
    * @return  the measure instance
    */
   def prevSpanEvent( quad: IntSquare ) : ML = new PrevSpanEvent( quad )

   private final class NextSpanEvent( quad: IntSquare ) extends ChebyshevLike {
      private val maxX = quad.right
      private val maxY = quad.bottom

      override def toString = "LongDistanceMeasure2D.NextSpanEvent(" + quad + ")"

      protected def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )

      override def distance( a: PointLike, b: PointLike ) = {
         val bx = b.x
         val by = b.y
         val ax = a.x
         val ay = a.y
         if( bx < ax || bx >= maxX ) {          // either start too small or unbounded
            if( by < ay || by >= maxY ) {       // ... and also stop too small or unbounded
               Long.MaxValue
            } else {
               by - ay
            }
         } else if( by < ay || by >= maxY ) {   // stop too small or unbounded
            bx - ax
         } else {
            val dx = bx - ax
            val dy = by - ay
            math.min( dx, dy )
         }
      }

      override def minDistance( p: PointLike, q: HyperCube ) : Long = {
         if( (q.right >= p.x) || (q.bottom >= p.y) ) {
            super.minDistance( p, q )
         } else Long.MaxValue
      }

      override def maxDistance( p: PointLike, q: HyperCube ) : Long = {
         if( (q.left >= p.x) || (q.top >= p.y) ) {
            super.maxDistance( p, q )
         } else Long.MaxValue
      }
   }

   private final class PrevSpanEvent( quad: IntSquare ) extends ChebyshevLike {
      private val minX = quad.left
      private val minY = quad.top   // note: we allow this to be used for unbounded span stops, as a special representation of Span.Void

      override def toString = "LongDistanceMeasure2D.PrevSpanEvent(" + quad + ")"

      protected def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )

      override def distance( a: PointLike, b: PointLike ) = {
         val bx = b.x
         val by = b.y
         val ax = a.x
         val ay = a.y
         if( bx > ax || bx <= minX ) {          // either start too large or unbounded
            if( by > ay || by <= minY ) {       // ... and also stop too large or unbounded
               Long.MaxValue
            } else {
               ay - by
            }
         } else if( by > ay || by <= minY ) {   // stop too large or unbounded
            ax - bx
         } else {
            val dx = ax - bx
            val dy = ay - by
            math.min( dx, dy )
         }
      }

      override def minDistance( p: PointLike, q: HyperCube ) : Long = {
         if( (q.left <= p.x) || (q.top <= p.y) ) {
            super.minDistance( p, q )
         } else Long.MaxValue
      }

      override def maxDistance( p: PointLike, q: HyperCube ) : Long = {
         if( (q.right <= p.x) || (q.bottom <= p.y) ) {
            super.maxDistance( p, q )
         } else Long.MaxValue
      }
   }

   private sealed trait ClipLike[ @specialized( Long ) M ] extends Impl[ M ] {
      protected def underlying: Impl[ M ]
      protected def quad: HyperCube
      override def toString = underlying.toString + ".clip(" + quad + ")"
      final def distance( a: PointLike, b: PointLike )    = if( quad.contains( b )) underlying.distance(    a, b ) else maxValue
      final def minDistance( a: PointLike, b: HyperCube ) = if( quad.contains( b )) underlying.minDistance( a, b ) else maxValue
      final def maxDistance( a: PointLike, b: HyperCube ) = if( quad.contains( b )) underlying.maxDistance( a, b ) else maxValue
   }

   private final class LongClip( protected val underlying: LongImpl, protected val quad: HyperCube )
   extends ClipLike[ Long ] with LongImpl

   private final class SqrClip(  protected val underlying: SqrImpl,  protected val quad: HyperCube )
   extends ClipLike[ Sqr ] with SqrImpl

   private sealed trait ApproximateLike[ @specialized( Long ) M ] extends Impl[ M ] {
      protected def underlying: Impl[ M ]
      protected def thresh: M

      override def toString = underlying.toString + ".approximate(" + thresh + ")"

      final def minDistance( a: PointLike, b: HyperCube ) = underlying.minDistance( a, b )
      final def maxDistance( a: PointLike, b: HyperCube ) = underlying.maxDistance( a, b )

      def distance( a: PointLike, b: PointLike ) : M = {
         val res = underlying.distance( a, b )
         if( isMeasureGreater( res, thresh )) res else zeroValue
      }
   }

   private final class LongApproximate( protected val underlying: LongImpl, protected val thresh: Long )
   extends ApproximateLike[ Long ] with LongImpl

   private final class SqrApproximate( protected val underlying: SqrImpl, protected val thresh: Sqr )
   extends ApproximateLike[ Sqr ] with SqrImpl

   private sealed trait QuadrantLike[ @specialized( Long ) M ] extends Impl[ M ] {
      private val right    = idx == 0 || idx == 3
      private val bottom   = idx >= 2

      protected def underlying: Impl[ M ]
      protected def idx: Int

      override def toString = underlying.toString + ".quadrant(" + idx + ")"

      final def distance( a: PointLike, b: PointLike ) : M = {
         if( (if( right  ) b.x >= a.x else b.x <= a.x) &&
             (if( bottom ) b.y >= a.y else b.y <= a.y) ) {

            underlying.distance( a, b )
         } else maxValue
      }

      final def minDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
             (if( bottom ) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) ) {

            underlying.minDistance( p, q )
         } else maxValue
      }

      final def maxDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
             (if( bottom ) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) ) {

            underlying.maxDistance( p, q )
         } else maxValue
      }
   }

   private final class LongQuadrant( protected val underlying: LongImpl, protected val idx: Int )
   extends QuadrantLike[ Long ] with LongImpl

   private final class SqrQuadrant( protected val underlying: SqrImpl, protected val idx: Int )
   extends QuadrantLike[ Sqr ] with SqrImpl

   // praktisch werden alle logischen statements, die zum ausschluss fuehren (maxValue)
   // von Quadrant uebernommen und umgekehrt (a & b --> !a | !b)
   private sealed trait ExceptQuadrantLike[ @specialized( Long ) M ] extends Impl[ M ] {
      private val right    = idx == 0 || idx == 3
      private val bottom   = idx >= 2

      protected def underlying: Impl[ M ]
      protected def idx: Int

      override def toString = underlying.toString + ".exceptQuadrant(" + idx + ")"

      final def distance( a: PointLike, b: PointLike ) : M = {
         if( (if( right  ) b.x <= a.x else b.x >= a.x) ||
             (if( bottom ) b.y <= a.y else b.y >= a.y) ) {

            underlying.distance( a, b )
         } else maxValue
      }

      final def minDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) <= p.x else (q.cx + qem1) >= p.x) ||
             (if( bottom ) (q.cy - qe) <= p.y else (q.cy + qem1) >= p.y) ) {

            underlying.minDistance( p, q )
         } else maxValue
      }

      final def maxDistance( p: PointLike, q: HyperCube ) : M = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) <= p.x else (q.cx - qe) >= p.x) ||
             (if( bottom ) (q.cy + qem1) <= p.y else (q.cy - qe) >= p.y) ) {

            underlying.maxDistance( p, q )
         } else maxValue
      }
   }

   private final class LongExceptQuadrant( protected val underlying: LongImpl, protected val idx: Int )
   extends ExceptQuadrantLike[ Long ] with LongImpl

   private final class SqrExceptQuadrant( protected val underlying: SqrImpl, protected val idx: Int )
   extends ExceptQuadrantLike[ Sqr ] with SqrImpl

   private sealed trait ChebyshevLike extends LongImpl {
      protected def apply( dx: Long, dy: Long ) : Long

      def distance( a: PointLike, b: PointLike ) = {
         val dx = math.abs( a.x - b.x )
         val dy = math.abs( a.y - b.y )
         apply( dx, dy )
      }
      def minDistance( a: PointLike, q: HyperCube ) : Long = {
         val px   = a.x
         val py   = a.y
         val l    = q.left
         val t    = q.top
         var dx   = 0L
         var dy   = 0L
         if( px < l ) {
            dx = l - px
            if( py < t ) {
               dy = t - py
            } else {
               val b = q.bottom
               if( py > b ) {
                  dy = py - b
               }
            }
         } else {
            val r = q.right
            if( px > r ) {
               dx   = px - r
               if( py < t ) {
                  dy = t - py
               } else {
                  val b = q.bottom
                  if( py > b ) {
                     dy = py - b
                  }
               }
            } else if( py < t ) {
               dy = t - py
               if( px < l ) {
                  dx = l - px
               } else {
                  if( px > r ) {
                     dx = px - r
                  }
               }
            } else {
               val b = q.bottom
               if( py > b ) {
                  dy = py - b
                  if( px < l ) {
                     dx = l - px
                  } else {
                     if( px > r ) {
                        dx = px - r
                     }
                  }
               }
            }
         }
         apply( dx, dy )
      }

      def maxDistance( a: PointLike, q: HyperCube ) : Long = {
         val px = a.x
         val py = a.y
         if( px < q.cx ) {
            val dx = q.right - px
            val dy = if( py < q.cy ) {    // bottom right is furthest
               q.bottom - py
            } else {                      // top right is furthest
               py - q.top
            }
            apply( dx, dy )
         } else {
            val dx = px - q.left
            val dy = if( py < q.cy ) {    // bottom left is furthest
               q.bottom - py
            } else {                      // top left is furthest
               py - q.top
            }
            apply( dx, dy )
         }
      }
   }

   private sealed trait Impl[ @specialized( Long ) M ] extends Ops[ M, TwoDim ] {
      def zeroValue: M
   }

   private sealed trait LongImpl extends Impl[ Long ] {
      final def manifest : Manifest[ Long ] = Manifest.Long

      final def maxValue : Long = Long.MaxValue
      final def zeroValue : Long = 0L
      final def isMeasureZero( m: Long ) : Boolean = m == 0L
      final def isMeasureGreater( a: Long, b: Long ) : Boolean = a > b
      final def compareMeasure( a: Long, b: Long ) : Int = if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : ML = new LongClip( this, quad )
      final def approximate( thresh: Long ) : ML = new LongApproximate( this, thresh )
      final def orthant( idx: Int ) : ML = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new LongQuadrant( this, idx )
      }
      final def exceptOrthant( idx: Int ) : ML = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new LongExceptQuadrant( this, idx )
      }
  }

   private sealed trait SqrImpl extends Impl[ Sqr ] {
      final def manifest : Manifest[ Sqr ] = Predef.manifest[ Sqr ]

      final def maxValue : Sqr = MaxDistance
      final def zeroValue : Sqr = Zero
      final def isMeasureZero( m: Sqr ) : Boolean = m == Zero
      final def isMeasureGreater( a: Sqr, b: Sqr ) : Boolean = a > b
      final def compareMeasure( a: Sqr, b: Sqr ) : Int = a.compare( b ) // if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : MS = new SqrClip( this, quad )
      final def approximate( thresh: Sqr ) : MS = new SqrApproximate( this, thresh )
      final def orthant( idx: Int ) : MS = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new SqrQuadrant( this, idx )
      }
      final def exceptOrthant( idx: Int ) : MS = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new SqrExceptQuadrant( this, idx )
      }
   }
}