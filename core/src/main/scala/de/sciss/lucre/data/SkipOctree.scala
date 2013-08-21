/*
 *  SkipOctree.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss
package lucre
package data

import geom.{Space, DistanceMeasure, QueryShape}
import collection.immutable.{IndexedSeq => Vec}
import stm.{Mutable, Sys}
import serial.{DataInput, Serializer}

object SkipOctree {
  implicit def nonTxnPointView[D <: Space[D], A](implicit view: A => D#PointLike): (A, Any) => D#PointLike =
    (a, _) => view(a)

  def empty[S <: Sys[S], D <: Space[D], A](hyperCube: D#HyperCube)
                                          (implicit tx: S#Tx, view: (A, S#Tx) => D#PointLike, space: D,
                                           keySerializer: Serializer[S#Tx, S#Acc, A]): SkipOctree[S, D, A] =
    DeterministicSkipOctree.empty[S, D, A](hyperCube)

  def read[S <: Sys[S], D <: Space[D], A](in: DataInput, access: S#Acc)(
    implicit tx: S#Tx, view: (A, S#Tx) => D#PointLike, space: D,
    keySerializer: Serializer[S#Tx, S#Acc, A]): SkipOctree[S, D, A] =
    DeterministicSkipOctree.read[S, D, A](in, access)
}
/**
 * A `SkipOctree` is a multi-dimensional data structure that
 * maps coordinates to values. It extends the interface
 * of scala's mutable `Map` and adds further operations such
 * as range requires and nearest neighbour search.
 */
trait SkipOctree[S <: Sys[S], D <: Space[D], A] extends Mutable[S#ID, S#Tx] {
  /** The space (i.e., resolution and dimensionality) underlying the tree. */
  def space: D

  /** A function which maps an element (possibly through transactional access) to a geometric point coordinate. */
  def pointView: (A, S#Tx) => D#PointLike

  /** The base square of the tree. No point can lie outside this square (or hyper-cube). */
  def hyperCube: D#HyperCube

  /** Reports the number of decimation levels in the tree. */
  def numLevels(implicit tx: S#Tx): Int

  /** The number of orthants in each hyperCube. This is equal
    * to `1 << numDimensions` and gives the upper bound
    * of the index to `QNode.child()`.
    */
  def numOrthants: Int

  /** Queries the element at a given point.
    *
    * @param point  the point to look up.
    * @return `Some` element if found, `None` if the point was not present.
    */
  def get(point: D#PointLike)(implicit tx: S#Tx): Option[A]

  /** Queries whether an element is stored at a given point.
    *
    * @param point  the point to query
    * @return   `true` if an element is associated with the query point, `false` otherwise
    */
  def isDefinedAt(point: D#PointLike)(implicit tx: S#Tx): Boolean

  /** Removes the element stored under a given point view.
    *
    * @param   point the location of the element to remove
    * @return  the element removed, wrapped as `Some`, or `None` if no element was
    *          found for the given point.
    */
  def removeAt(point: D#PointLike)(implicit tx: S#Tx): Option[A]

  /** Queries the number of leaves in the tree. This may be a very costly action,
    * so it is recommended to only use it for debugging purposes.
    */
  def size(implicit tx: S#Tx): Int

  /** Adds an element to the tree (or replaces a given element with the same point location).
    *
    * @param   elem  the element to add
    * @return  `true` if the element is new in the tree. If a previous entry with the
    *          same point view is overwritten, this is `true` if the elements were
    *          '''not equal''', `false` if they were equal
    */
  def add(elem: A)(implicit tx: S#Tx): Boolean

  /** Looks up a point and applies a transformation to the entry associated with it.
    * This can be used to update an element in-place, or used for maintaining a spatial multi-map.
    *
    * @param point   the location at which to perform the transformation
    * @param fun  a function to transform the element found, or generate a new element. The argument is the
    *             element previously stored with the point, or `None` if no element is found. The result is
    *             expected to be `Some` new element to be stored, or `None` if no element is to be stored
    *             (in this case, if an element was previously stored, it is removed)
    * @return     the previously stored element (if any)
    */
  def transformAt(point: D#PointLike)(fun: Option[A] => Option[A])(implicit tx: S#Tx): Option[A]

  /** Removes an element from the tree
    *
    * @param   elem  the element to remove
    * @return  `true` if the element had been found in the tree and thus been removed.
    */
  def remove(elem: A)(implicit tx: S#Tx): Boolean

  /** Adds an element to the tree (or replaces a given element with the same point location).
    *
    * @param   elem  the element to add to the tree
    * @return  the old element stored for the same point view, if it existed
    */
  def update(elem: A)(implicit tx: S#Tx): Option[A]

  def rangeQuery[@specialized(Long) Area](qs: QueryShape[Area, D])(implicit tx: S#Tx): Iterator[S#Tx, A]

  /** Tests whether the tree contains an element. */
  def contains(elem: A)(implicit tx: S#Tx): Boolean

  /** Tests whether the tree is empty (`true`) or whether it contains any elements (`false`). */
  def isEmpty(implicit tx: S#Tx): Boolean

  /** Converts the tree into a linearized indexed sequence. This is not necessarily a
    * very efficient method, and should usually just be used for debugging.
    */
  def toIndexedSeq(implicit tx: S#Tx): Vec[A]

  /** Converts the tree into a linearized list. This is not necessarily a
    * very efficient method, and should usually just be used for debugging.
    */
  def toList(implicit tx: S#Tx): List[A]

  /** Converts the tree into a linearized sequence. This is not necessarily a
    * very efficient method, and should usually just be used for debugging.
    * To avoid surprises, this does not call `iterator.toSeq` because that would
    * produce a `Stream` and thus subject to further changes to the tree while
    * traversing. The returned seq instead is 'forced' and thus stable.
    */
  def toSeq(implicit tx: S#Tx): Seq[A]

  /** Converts the tree into a non-transactional set. This is not necessarily a
    * very efficient method, and should usually just be used for debugging.
    */
  def toSet(implicit tx: S#Tx): Set[A]

  def clear()(implicit tx: S#Tx): Unit

  /** Reports the nearest neighbor entry with respect to
    * a given point.
    *
    * Note: There is a potential numeric overflow if the
    * squared distance of the query point towards the
    * furthest corner of the tree's root hyper-cube exceeds 63 bits.
    * For a root `IntSquare(0x40000000, 0x40000000, 0x40000000)`, this
    * happens for example for any point going more towards north-west
    * than `IntPoint2DLike(-1572067139, -1572067139)`.
    *
    * @param   point the point of which the nearest neighbor is to be found
    * @param   metric   (description missing)
    *
    * @throws  NoSuchElementException  if the tree is empty
    */
  def nearestNeighbor[@specialized(Long) M](point: D#PointLike, metric: DistanceMeasure[M, D])
                                           (implicit tx: S#Tx): A

  /** Same as `nearestNeighbor` but returning an `Option`, thus not throwing an exception
    * if no neighbor is found.
    */
  def nearestNeighborOption[@specialized(Long) M](point: D#PointLike, metric: DistanceMeasure[M, D])
                                                 (implicit tx: S#Tx): Option[A]

  /** An `Iterator` which iterates over the points stored
    * in the octree, using an in-order traversal directed
    * by the orthant indices of the nodes of the tree.
    *
    * Great care has to be taken as the iterator might be corrupted if the tree
    * is successively changed before the iterator is exhausted.
    */
  def iterator(implicit tx: S#Tx): Iterator[S#Tx, A]

  /** Adds an element to the tree. If there is already an element stored at the point represented by the
    * new element, it will be replaced. */
  def +=(elem: A)(implicit tx: S#Tx): this.type

  /** Removes an element from the tree. If the element is not found, this operation does nothing. */
  def -=(elem: A)(implicit tx: S#Tx): this.type

  /** Returns a string debug representation of the octree. */
  def debugPrint()(implicit tx: S#Tx): String
}