/*
 *  QueryShape.scala
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

/** A shape for range queries. Type `A` indicates the results
  * of area calculations, and may be specialized.
  */
trait QueryShape[Area, D <: Space[D]] {
  def overlapArea(q: D#HyperCube): Area

  def isAreaGreater(a: D#HyperCube, b: Area): Boolean

  def isAreaNonEmpty(area: Area): Boolean

  /** Queries the overlap of this shape with a given
    * `IntPoint2D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
  def contains(p: D#PointLike): Boolean
}
