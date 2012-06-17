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

   private type Sqr        = BigInt
   private type M          = DistanceMeasure[ Sqr, TwoDim ]
   private val MaxDistance : Sqr = {
      val n = BigInt( Long.MaxValue )
      n * n
   }

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
    * distances across all dimensions. This is, strictly speaking, only a semi metric.
    */
   val vehsybehc : M = Vehsybehc

   private object Chebyshev extends ChebyshevLike {
      override def toString = "LongDistanceMeasure2D.chebyshev"
      protected def apply( dx: Sqr, dy: Sqr ) : Sqr = dx max dy
   }

   private object Vehsybehc extends ChebyshevLike {
      override def toString = "LongDistanceMeasure2D.vehsybehc"
      protected def apply( dx: Sqr, dy: Sqr ) : Sqr = dx min dy
   }

   private object EuclideanSq extends Impl {
      override def toString = "LongDistanceMeasure2D.euclideanSq"
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

   private final class Approximate( underlying: Impl, thresh: Sqr ) extends Impl {
      override def toString = underlying.toString + ".approximate(" + thresh + ")"
      def minDistance( a: PointLike, b: HyperCube ) = underlying.minDistance( a, b )
      def maxDistance( a: PointLike, b: HyperCube ) = underlying.maxDistance( a, b )
      def distance( a: PointLike, b: PointLike ) = {
         val res = b.distanceSq( a )
         if( res > thresh ) res else 0L
      }
   }

   private final class Quadrant( underlying: M, idx: Int )
   extends Impl {
      private val right    = idx == 0 || idx == 3
      private val bottom   = idx >= 2

      def distance( a: PointLike, b: PointLike ) : Sqr = {
         if( (if( right  ) b.x >= a.x else b.x <= a.x) &&
             (if( bottom ) b.y >= a.y else b.y <= a.y) ) {

            underlying.distance( a, b )
         } else MaxDistance
      }

      def minDistance( p: PointLike, q: HyperCube ) : Sqr = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
             (if( bottom ) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) ) {

            underlying.minDistance( p, q )
         } else MaxDistance
      }

      def maxDistance( p: PointLike, q: HyperCube ) : Sqr = {
         val qe   = q.extent
         val qem1 = qe - 1

         if( (if( right  ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
             (if( bottom ) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) ) {

            underlying.maxDistance( p, q )
         } else MaxDistance
      }
   }

   private sealed trait ChebyshevLike extends Impl {
      protected def apply( dx: Sqr, dy: Sqr ) : Sqr

      final def distance( a: PointLike, b: PointLike ) = {
         val dx = (BigInt( a.x ) - BigInt( b.x )).abs
         val dy = (BigInt( a.y ) - BigInt( b.y )).abs
         apply( dx, dy )
      }
      final def minDistance( a: PointLike, q: HyperCube ) : Sqr = {
         val px   = a.x
         val py   = a.y
         val l    = q.left
         val t    = q.top
         var dx   = Zero
         var dy   = Zero
         if( px < l ) {
            dx = BigInt( l ) - BigInt( px )
            if( py < t ) {
               dy = BigInt( t ) - BigInt( py )
            } else {
               val b = q.bottom
               if( py > b ) {
                  dy = BigInt( py ) - BigInt( b )
               }
            }
         } else {
            val r = q.right
            if( px > r ) {
               dx   = BigInt( px ) - BigInt( r )
               if( py < t ) {
                  dy = BigInt( t ) - BigInt( py )
               } else {
                  val b = q.bottom
                  if( py > b ) {
                     dy = BigInt( py ) - BigInt( b )
                  }
               }
            } else if( py < t ) {
               dy = BigInt( t ) - BigInt( py )
               if( px < l ) {
                  dx = BigInt( l ) - BigInt( px )
               } else {
                  if( px > r ) {
                     dx = BigInt( px ) - BigInt( r )
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

      final def maxDistance( a: PointLike, q: HyperCube ) : Sqr = {
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

   private sealed trait Impl extends DistanceMeasure[ Sqr, TwoDim ] {
      final def manifest : Manifest[ Sqr ] = Predef.manifest[ Sqr ]

      final def maxValue : Sqr = MaxDistance
      final def isMeasureZero( m: Sqr ) : Boolean = m == Zero
      final def isMeasureGreater( a: Sqr, b: Sqr ) : Boolean = a > b
      final def compareMeasure( a: Sqr, b: Sqr ) : Int = a.compare( b ) // if( a > b ) 1 else if( a < b ) -1 else 0

      final def clip( quad: HyperCube ) : M = new Clip( this, quad )
      final def approximate( thresh: Sqr ) : M = new Approximate( this, thresh )
      final def orthant( idx: Int ) : M = {
         require( idx >= 0 && idx < 4, "Quadrant index out of range (" + idx + ")" )
         new Quadrant( this, idx )
      }
   }
}