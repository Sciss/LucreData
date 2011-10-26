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

package de.sciss.collection.geom

/**
 * A `DistanceMeasure` is used in nearest neighbour search,
 * in order to allow different ways points and children are
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
trait DistanceMeasure[ D <: Space[ D ]] {
   /**
    * Calculates the distance between two points.
    */
   def distance( a: D#Point, b: D#Point ) : Long

   /**
    * Calculates the minimum distance between a point and
    * any possible point of a given hyper-cube. In the euclidean
    * case, this is the distance to the hyper-cube `b`'s corner that
    * is closest to the point `a`, if `a` lies outside of `b`,
    * or zero, if `a` lies within `b`.
    */
   def minDistance( a: D#Point, b: D#HyperCube ) : Long

   /**
    * Calculates the maximum distance between a point and
    * any possible point of a given hyper-cube. In the euclidean
    * case, this is the distance to the hyper-cube `b`'s corner that
    * is furthest to the point `a`, no matter whether `a`
    * is contained in `b` or not.
    */
   def maxDistance( a: D#Point, b: D#HyperCube ) : Long

   /**
    * Applies a filter to this measure by constraining distances
    * to objects `b` which lie within the given `Square`. That
    * is, if for example `distance( a, b )` is called, first it
    * is checked if `b` is within `hyperCube`. If so, the underlying
    * measure is calculated, otherwise, `Long.MaxValue` is returned.
    * This behaviour extends to the `minDistance` and `maxDistance`
    * methods.
    */
   def clip( hyperCube: D#HyperCube ) : DistanceMeasure[ D ]

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
   def approximate( thresh: Long ) : DistanceMeasure[ D ]

   def orthant( idx: Int ) : DistanceMeasure[ D ]
}
