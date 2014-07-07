/*
 *  DistanceMeasure.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package geom

object DistanceMeasure {
  trait Ops[M, D <: Space[D]] extends DistanceMeasure[M, D] {
    /**
     * Applies a filter to this measure by constraining distances
     * to objects `b` which lie within the given `IntSquare`. That
     * is, if for example `distance( a, b )` is called, first it
     * is checked if `b` is within `hyperCube`. If so, the underlying
     * measure is calculated, otherwise, `Long.MaxValue` is returned.
     * This behaviour extends to the `minDistance` and `maxDistance`
     * methods.
     */
    def clip(hyperCube: D#HyperCube): Ops[M, D]

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
    def approximate(thresh: M): Ops[M, D]

    def orthant(idx: Int): Ops[M, D]

    def exceptOrthant(idx: Int): Ops[M, D]

    //      def filter( p: D#PointLike => Boolean ) : Ops[ M, D ]
  }
}

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
trait DistanceMeasure[M, D <: Space[D]] {
  //   def manifest: Manifest[ M ]
  def newArray(size: Int): Array[M]

  /**
   * A value which will never be exceeded by the measure
   */
  def maxValue: M

  //   /**
  //    * A value which will never be undercut by the measure
  //    */
  //   def minValue : M

  def isMeasureGreater(a: M, b: M): Boolean

  def compareMeasure(a: M, b: M): Int

  def compareArea(a: D#HyperCube, b: D#HyperCube): Int = ???

  def isMeasureZero(m: M): Boolean

  /**
   * Calculates the distance between two points.
   *
   * @param   a  the input query point
   * @param   b  a point in the octree
   */
  def distance(a: D#PointLike, b: D#PointLike): M

  /**
   * Calculates the minimum distance between a point and
   * any possible point of a given hyper-cube. In the euclidean
   * case, this is the distance to the hyper-cube `b`'s corner that
   * is closest to the point `a`, if `a` lies outside of `b`,
   * or zero, if `a` lies within `b`.
   */
  def minDistance(a: D#PointLike, b: D#HyperCube): M

  /**
   * Calculates the maximum distance between a point and
   * any possible point of a given hyper-cube. In the euclidean
   * case, this is the distance to the hyper-cube `b`'s corner that
   * is furthest to the point `a`, no matter whether `a`
   * is contained in `b` or not.
   */
  def maxDistance(a: D#PointLike, b: D#HyperCube): M

  // def isEquipotent(v: D#PointLike, rmax: M, parent: D#HyperCube, child: D#HyperCube): Boolean = ???

  def stabbingDirections(v: D#PointLike, parent: D#HyperCube, child: D#HyperCube): List[Int] = ???
}