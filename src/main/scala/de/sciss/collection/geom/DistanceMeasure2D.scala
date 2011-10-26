/*
 *  DistanceMeasure2D.scala
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

import annotation.switch

object DistanceMeasure2D {
   /**
    * A measure that uses the euclidean squared distance
    * which is faster than the euclidean distance as the square root
    * does not need to be taken.
    */
   val euclideanSq : DistanceMeasure[ Space.TwoDim ] = new DistanceMeasure2D {
      def distance( a: Point2DLike, b: Point2DLike ) = b.distanceSq( a )
      def minDistance( a: Point2DLike, b: SquareLike ) = b.minDistanceSq( a )
      def maxDistance( a: Point2DLike, b: SquareLike ) = b.maxDistanceSq( a )
   }

   /**
    * A chebychev distance measure, based on the maximum of the absolute
    * distances across all dimensions.
    */
   val chebyshev : DistanceMeasure[ Space.TwoDim ] = new ChebyshevLikeDistanceMeasure {
      protected final def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   /**
    * An 'inverted' chebychev distance measure, based on the *minimum* of the absolute
    * distances across all dimensions. This is, strictly speaking, only a semi metric.
    */
   val vehsybehc : DistanceMeasure[ Space.TwoDim ] = new ChebyshevLikeDistanceMeasure {
      protected final def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )
   }

   private class Clip( underlying: DistanceMeasure2D, quad: SquareLike ) extends DistanceMeasure2D {
      def distance( a: Point2DLike, b: Point2DLike )   = if( quad.contains( b )) underlying.distance(    a, b ) else Long.MaxValue
      def minDistance( a: Point2DLike, b: SquareLike )     = if( quad.contains( b )) underlying.minDistance( a, b ) else Long.MaxValue
      def maxDistance( a: Point2DLike, b: SquareLike )     = if( quad.contains( b )) underlying.maxDistance( a, b ) else Long.MaxValue
   }

   private class Approximate( underlying: DistanceMeasure2D, thresh: Long ) extends DistanceMeasure2D {
      def minDistance( a: Point2DLike, b: SquareLike ) = underlying.minDistance( a, b )
      def maxDistance( a: Point2DLike, b: SquareLike ) = underlying.maxDistance( a, b )
      def distance( a: Point2DLike, b: Point2DLike ) = {
         val res = b.distanceSq( a )
         if( res > thresh ) res else 0L
      }
   }

   private class SouthWest( underlying: DistanceMeasure2D ) extends DistanceMeasure2D {
      def distance( a: Point2DLike, b: Point2DLike ) =
         if( b.x <= a.x && b.y >= a.y ) underlying.distance( a, b ) else Long.MaxValue

      def minDistance( p: Point2DLike, q: SquareLike ) =
         if( p.x >= q.left && p.y <= q.bottom ) underlying.minDistance( p, q ) else Long.MaxValue

      def maxDistance( p: Point2DLike, q: SquareLike ) =
         if( q.right <= p.x && q.top >= p.y ) underlying.maxDistance( p, q ) else Long.MaxValue
   }

   private sealed trait ChebyshevLikeDistanceMeasure extends DistanceMeasure2D {
      protected def apply( dx: Long, dy: Long ) : Long

      def distance( a: Point2DLike, b: Point2DLike ) = {
         val dx = math.abs( a.x.toLong - b.x.toLong )
         val dy = math.abs( a.y.toLong - b.y.toLong )
         apply( dx, dy )
      }
      def minDistance( a: Point2DLike, q: SquareLike ) : Long = {
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

      def maxDistance( a: Point2DLike, q: SquareLike ) : Long = {
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

   private sealed trait DistanceMeasure2D extends DistanceMeasure[ Space.TwoDim ] {
      final def clip( quad: SquareLike ) : DistanceMeasure[ Space.TwoDim ] = new Clip( this, quad )
      final def approximate( thresh: Long ) : DistanceMeasure[ Space.TwoDim ] = new Approximate( this, thresh )
      final def orthant( idx: Int ) : DistanceMeasure[ Space.TwoDim ] = (idx: @switch) match {
         case 0 => sys.error( "TODO" )
         case 1 => sys.error( "TODO" )
         case 2 => new SouthWest( this )
         case 3 => sys.error( "TODO" )
         case _ => throw new IllegalArgumentException( "Invalid quadrant index " + idx )
      }
   }
}