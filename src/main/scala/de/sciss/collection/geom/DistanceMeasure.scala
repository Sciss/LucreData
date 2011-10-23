package de.sciss.collection.geom

/*
 *  DistanceMeasure.scala
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

import annotation.switch
import sys.error

object DistanceMeasure {
   /**
    * A measure that uses the euclidean squared distance
    * which is faster than the euclidean distance as the square root
    * does not need to be taken.
    */
   val euclideanSq = new DistanceMeasure {
      def distance( a: PointLike, b: PointLike ) = b.distanceSq( a )
      def minDistance( a: PointLike, b: Quad ) = b.minDistanceSq( a )
      def maxDistance( a: PointLike, b: Quad ) = b.maxDistanceSq( a )
   }

   /**
    * A chebychev distance measure, based on the maximum of the absolute
    * distances across all dimensions.
    */
   val chebyshev : DistanceMeasure = new ChebyshevLikeDistanceMeasure {
      protected final def apply( dx: Long, dy: Long ) : Long = math.max( dx, dy )
   }

   /**
    * An 'inverted' chebychev distance measure, based on the *minimum* of the absolute
    * distances across all dimensions. This is, strictly speaking, only a semi metric.
    */
   val vehsybehc : DistanceMeasure = new ChebyshevLikeDistanceMeasure {
      protected final def apply( dx: Long, dy: Long ) : Long = math.min( dx, dy )
   }

   private class Clip( underlying: DistanceMeasure, quad: Quad ) extends DistanceMeasure {
      def distance( a: PointLike, b: PointLike )   = if( quad.contains( b )) underlying.distance(    a, b ) else Long.MaxValue
      def minDistance( a: PointLike, b: Quad )     = if( quad.contains( b )) underlying.minDistance( a, b ) else Long.MaxValue
      def maxDistance( a: PointLike, b: Quad )     = if( quad.contains( b )) underlying.maxDistance( a, b ) else Long.MaxValue
   }

   private class Approximate( underlying: DistanceMeasure, thresh: Long ) extends DistanceMeasure {
      def minDistance( a: PointLike, b: Quad ) = underlying.minDistance( a, b )
      def maxDistance( a: PointLike, b: Quad ) = underlying.maxDistance( a, b )
      def distance( a: PointLike, b: PointLike ) = {
         val res = b.distanceSq( a )
         if( res > thresh ) res else 0L
      }
   }

   private class SouthWest( underlying: DistanceMeasure ) extends DistanceMeasure {
      def distance( a: PointLike, b: PointLike ) =
         if( b.x <= a.x && b.y >= a.y ) underlying.distance( a, b ) else Long.MaxValue

      def minDistance( p: PointLike, q: Quad ) =
         if( p.x >= q.left && p.y <= q.bottom ) underlying.minDistance( p, q ) else Long.MaxValue

      def maxDistance( p: PointLike, q: Quad ) =
         if( q.right <= p.x && q.top >= p.y ) underlying.maxDistance( p, q ) else Long.MaxValue
   }

   private sealed trait ChebyshevLikeDistanceMeasure extends DistanceMeasure {
      protected def apply( dx: Long, dy: Long ) : Long

      def distance( a: PointLike, b: PointLike ) = {
         val dx = math.abs( a.x.toLong - b.x.toLong )
         val dy = math.abs( a.y.toLong - b.y.toLong )
         apply( dx, dy )
      }
      def minDistance( a: PointLike, q: Quad ) : Long = {
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

      def maxDistance( a: PointLike, q: Quad ) : Long = {
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
}
/**
 * A `DistanceMeasure` is used in nearest neighbour search,
 * in order to allow different ways points and quads are
 * favoured or filtered during the search.
 *
 * For simplicity and performance, the measures, although
 * they could be generalized as `Ordered`, are given as
 * `Long` values. Only comparisons are performed with
 * the results, therefore some optimizations may be made,
 * for example the `euclidean` measure omits taking
 * the square root of the distances, while still preserving
 * the ordering between the possible results.
 */
trait DistanceMeasure {
   /**
    * Calculates the distance between two points.
    */
   def distance( a: PointLike, b: PointLike ) : Long

   /**
    * Calculates the minimum distance between a point and
    * any possible point of a given quad. In the euclidean
    * case, this is the distance to the quad `b`'s corner that
    * is closest to the point `a`, if `a` lies outside of `b`,
    * or zero, if `a` lies within `b`.
    */
   def minDistance( a: PointLike, b: Quad ) : Long

   /**
    * Calculates the maximum distance between a point and
    * any possible point of a given quad. In the euclidean
    * case, this is the distance to the quad `b`'s corner that
    * is furthest to the point `a`, no matter whether `a`
    * is contained in `b` or not.
    */
   def maxDistance( a: PointLike, b: Quad ) : Long

   /**
    * Applies a filter to this measure by constraining distances
    * to objects `b` which lie within the given `Quad`. That
    * is, if for example `distance( a, b )` is called, first it
    * is checked if `b` is within `quad`. If so, the underlying
    * measure is calculated, otherwise, `Long.MaxValue` is returned.
    * This behaviour extends to the `minDistance` and `maxDistance`
    * methods.
    */
   final def clip( quad: Quad ) : DistanceMeasure = new DistanceMeasure.Clip( this, quad )

   /**
    * Composes this distance so that a threshold is applied to
    * point-point distances. If the point-point distance of the
    * underlying measure returns a value less than or equal the given threshold,
    * then instead the value `0L` is returned. This allows for
    * quicker searches so that a nearest neighbour becomes an
    * approximate nn within the given threshold (the first
    * arbitrary point encountered with a distance smaller than
    * the threshold will be returned).
    *
    * Note that the threshold is directly compared to the result
    * of `distance`, thus if the underlying measure uses a skewed
    * distance, this must be taken into account. For example, if
    * `euclideanSq` is used, and points within a radius of 4 should
    * be approximated, a threshold of `4 * 4 = 16` must be chosen!
    */
   final def approximate( thresh: Long ) : DistanceMeasure = new DistanceMeasure.Approximate( this, thresh )

   final def quadrant( idx: Int ) : DistanceMeasure = (idx: @switch) match {
      case 0 => error( "TODO" )
      case 1 => error( "TODO" )
      case 2 => new DistanceMeasure.SouthWest( this )
      case 3 => error( "TODO" )
      case _ => throw new IllegalArgumentException( "Invalid quadrant index " + idx )
   }
}
