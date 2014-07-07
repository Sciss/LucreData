/*
 *  HyperCube.scala
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

trait HyperCube[D <: Space[D]] {

  def orthant(idx: Int): D#HyperCube

  def contains(point: D#PointLike): Boolean

  /**
   * Checks whether a given hyper-cube is fully contained in this hyper-cube.
   * This is also the case if their bounds full match.
   */
  def contains(hyperCube: D#HyperCube): Boolean

  /**
   * Calculates the minimum distance to a point in the euclidean metric.
   * This calls `minDistanceSq` and then takes the square root.
   */
  def minDistance(point: D#PointLike): Double

  /**
   * Calculates the maximum distance to a point in the euclidean metric.
   * This calls `maxDistanceSq` and then takes the square root.
   */
  def maxDistance(point: D#PointLike): Double

  /**
   * Determines the orthant index of a point `point`.
   *
   * @return  the index of the orthant (beginning at 0), or -1 if `point` lies
   *          outside of this hyper-cube.
   */
  def indexOf(point: D#PointLike): Int

  /**
   * Determines the orthant index of another internal hyper-cube `inner`.
   *
   * @return  the index of the orthant (beginning at 0), or -1 if `inner` lies
   *          outside of this hyper-cube.
   */
  def indexOf(inner: D#HyperCube): Int

  /**
   * Calculates the greatest interesting hyper-cube inside this hyper-cube which
   * contains both points `a` and `b`, and they occupy distinct orthants.
   */
  def greatestInteresting(a: D#PointLike, b: D#PointLike): D#HyperCube

  /**
   * Calculates the greatest interesting hyper-cube inside this hyper-cube which
   * contains both hyper-cube `a` and point `b`, and they occupy distinct orthants.
   */
  def greatestInteresting(a: D#HyperCube, b: D#PointLike): D#HyperCube
}