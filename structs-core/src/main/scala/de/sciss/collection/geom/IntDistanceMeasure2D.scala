/*
 *  IntDistanceMeasure2D.scala
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

object IntDistanceMeasure2D {
   import IntSpace.TwoDim
   import TwoDim._
   import DistanceMeasure.Ops

   private type M = DistanceMeasure[ Long, TwoDim ] with Ops[ Long, TwoDim ]

   /**
    * A measure that uses the euclidean squared distance
    * which is faster than the euclidean distance as the square root
    * does not need to be taken.
    */
   val euclideanSq : M = EuclideanSq

   /**
    * A chebychev distance measure, based on the maximum of the absolute
    * distances across all dimensions.
    */
   val chebyshev : M = Chebyshev

   /**
    * An 'inverted' chebychev distance measure, based on the *minimum* of the absolute
    * distances across all dimensions. This is, strictly speaking, only a semi metric,
    * and probably totally **useless**.
    */
   val vehsybehc : M = Vehsybehc

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
   def nextSpanEvent( quad: IntSquare ) : M = new NextSpanEvent( quad )

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
   def prevSpanEvent( quad: IntSquare ) : M = new PrevSpanEvent( quad )

   private object Chebyshev extends ChebyshevLike {
      override def toString = "IntDistanceMeasure2D.chebyshev"
      protected def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   private final class NextSpanEvent( quad: IntSquare ) extends ChebyshevLike {
      private val maxX = quad.right
      private val maxY = quad.bottom

      override def toString = "IntDistanceMeasure2D.NextSpanEvent(" + quad + ")"

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
               by.toLong - ay.toLong
            }
         } else if( by < ay || by >= maxY ) {   // stop too small or unbounded
            bx.toLong - ax.toLong
         } else {
            val dx = bx.toLong - ax.toLong
            val dy = by.toLong - ay.toLong
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

      override def toString = "IntDistanceMeasure2D.PrevSpanEvent(" + quad + ")"

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
               ay.toLong - by.toLong
            }
         } else if( by > ay || by <= minY ) {   // stop too large or unbounded
            ax.toLong - bx.toLong
         } else {
            val dx = ax.toLong - bx.toLong
            val dy = ay.toLong - by.toLong
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

   private object Vehsybehc extends ChebyshevLike {
      override def toString = "IntDistanceMeasure2D.vehsybehc"
      protected def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )
   }

   private object EuclideanSq extends Impl {
      override def toString = "IntDistanceMeasure2D.euclideanSq"
      def distance( a: PointLike, b: PointLike )    = b.distanceSq( a )
      def minDistance( a: PointLike, b: HyperCube ) = b.minDistanceSq( a )
      def maxDistance( a: PointLike, b: HyperCube ) = b.maxDistanceSq( a )
   }

   private final class Clip( underlying: Impl, quad: HyperCube ) extends Impl {
      override def toString = underlying.toString + ".clip(" + quad + ")"
      def distance( a: PointLike, b: PointLike )    = if( quad.contains( b )) underlying.distance(    a, b ) else Long.MaxValue
      def minDistance( a: PointLike, b: HyperCube ) = if( quad.contains( b )) underlying.minDistance( a, b ) else Long.MaxValue
      def maxDistance( a: PointLike, b: HyperCube ) = if( quad.contains( b )) underlying.maxDistance( a, b ) else Long.MaxValue
   }

   private final class Approximate( underlying: Impl, thresh: Long ) extends Impl {
      override def toString = underlying.toString + ".approximate(" + thresh + ")"
      def minDistance( a: PointLike, b: HyperCube ) = underlying.minDistance( a, b )
      def maxDistance( a: PointLike, b: HyperCube ) = underlying.maxDistance( a, b )
      def distance( a: PointLike, b: PointLike ) = {
         val res = underlying.distance( a, b ) // b.distanceSq( a )
         if( res > thresh ) res else 0L
      }
   }

   private final class Quadrant( underlying: DistanceMeasure[ Long, TwoDim ], idx: Int )
   extends Impl {
      private val right    = idx == 0 || idx == 3
      private val bottom   = idx >= 2

      override def toString = underlying.toString + ".quadrant(" + idx + ")"

      def distance( a: PointLike, b: PointLike ) : Long = {
         if( (if( right  ) b.x >= a.x else b.x <= a.x) &&
             (if( bottom ) b.y >= a.y else b.y <= a.y) ) {

            underlying.distance( a, b )
         } else Long.MaxValue
      }

      def minDistance( p: PointLike, q: HyperCube ) : Long = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
             (if( bottom ) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) ) {

            underlying.minDistance( p, q )
         } else Long.MaxValue
      }

      def maxDistance( p: PointLike, q: HyperCube ) : Long = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
             (if( bottom ) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) ) {

            underlying.maxDistance( p, q )
         } else Long.MaxValue
      }
   }

   private final class ExceptQuadrant( underlying: DistanceMeasure[ Long, TwoDim ], idx: Int )
   extends Impl {
      private val right    = idx == 0 || idx == 3
      private val bottom   = idx >= 2

      override def toString = underlying.toString + ".exceptQuadrant(" + idx + ")"

      def distance( a: PointLike, b: PointLike ) : Long = {
         if( (if( right  ) b.x <= a.x else b.x >= a.x) ||
             (if( bottom ) b.y <= a.y else b.y >= a.y) ) {

            underlying.distance( a, b )
         } else Long.MaxValue
      }

      def minDistance( p: PointLike, q: HyperCube ) : Long = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) <= p.x else (q.cx + qem1) >= p.x) ||
             (if( bottom ) (q.cy - qe) <= p.y else (q.cy + qem1) >= p.y) ) {

            underlying.minDistance( p, q )
         } else Long.MaxValue
      }

      def maxDistance( p: PointLike, q: HyperCube ) : Long = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) <= p.x else (q.cx - qe) >= p.x) ||
             (if( bottom ) (q.cy + qem1) <= p.y else (q.cy - qe) >= p.y) ) {

            underlying.maxDistance( p, q )
         } else Long.MaxValue
      }
   }

   private sealed trait ChebyshevLike extends Impl {
      protected def apply( dx: Long, dy: Long ) : Long

      def distance( a: PointLike, b: PointLike ) = {
         val dx = math.abs( a.x.toLong - b.x.toLong )
         val dy = math.abs( a.y.toLong - b.y.toLong )
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
            dx = l.toLong - px.toLong
            if( py < t ) {
               dy = t.toLong - py.toLong
            } else {
               val b = q.bottom
               if( py > b ) {
                  dy = py.toLong - b.toLong
               }
            }
         } else {
            val r = q.right
            if( px > r ) {
               dx   = px.toLong - r.toLong
               if( py < t ) {
                  dy = t.toLong - py.toLong
               } else {
                  val b = q.bottom
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
               val b = q.bottom
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
         val px = a.x
         val py = a.y
         if( px < q.cx ) {
            val dx = q.right.toLong - px.toLong
            val dy = if( py < q.cy ) {    // bottom right is furthest
               q.bottom.toLong - py.toLong
            } else {                      // top right is furthest
               py.toLong - q.top.toLong
            }
            apply( dx, dy )
         } else {
            val dx = px.toLong - q.left.toLong
            val dy = if( py < q.cy ) {    // bottom left is furthest
               q.bottom.toLong - py.toLong
            } else {                      // top left is furthest
               py.toLong - q.top.toLong
            }
            apply( dx, dy )
         }
      }
   }

   private sealed trait Impl extends DistanceMeasure[ Long, TwoDim ] with Ops[ Long, TwoDim ] {
//      final def manifest : Manifest[ Long ] = Manifest.Long
      final def newArray( size : Int ) = new Array[ Long ]( size )

      final def maxValue : Long = Long.MaxValue
      final def isMeasureZero( m: Long ) : Boolean = m == 0L
      final def isMeasureGreater( a: Long, b: Long ) : Boolean = a > b
      final def compareMeasure( a: Long, b: Long ) : Int = if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : M = new Clip( this, quad )
      final def approximate( thresh: Long ) : M = new Approximate( this, thresh )
      final def orthant( idx: Int ) : M = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new Quadrant( this, idx )
      }
      final def exceptOrthant( idx: Int ) : M = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new ExceptQuadrant( this, idx )
      }
   }
}