/*
 *  SkipOctree.scala
 *  (LucreData)
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

package de.sciss.collection.mutable

import collection.mutable.{Set => MSet}
import de.sciss.collection.geom.{Space, DistanceMeasure, QueryShape}

/**
 * A `SkipOctree` is a multi-dimensional data structure that
 * maps coordinates to values. It extends the interface
 * of scala's mutable `Map` and adds further operations such
 * as range requires and nearest neighbour search.
 */
trait SkipOctree[ D <: Space[ D ], A ] extends MSet[ A ] {
   def space: D

   def headTree: QNode
   def lastTree: QNode
   def pointView : A => D#PointLike

   def hyperCube : D#HyperCube

   def numLevels : Int

   /**
    * The number of orthants in each hyperCube. This is equal
    * to `1 << numDimensions` and gives the upper bound
    * of the index to `QNode.child()`.
    */
   def numOrthants : Int

   def get( point: D#PointLike ) : Option[ A ]
//   def apply( point: Point2DLike ) : A = get.getOrElse( throw new )
   def isDefinedAt( point: D#PointLike ) : Boolean

   /**
    * Removes the element stored under a given point view.
    *
    * @param   point the location of the element to remove
    * @return  the element removed, wrapped as `Some`, or `None` if no element was
    *          found for the given point.
    */
   def removeAt( point: D#PointLike ) : Option[ A ]

   /**
    * Adds an element to the tree
    *
    * @param   elem  the element to add
    * @return  `true` if the element is new in the tree. If a previous entry with the
    *          same point view is overwritten, this is `true` if the elements were
    *          '''not equal''', `false` if they were equal
    */
   def add( elem: A ) : Boolean

   /**
    * Adds an element to the tree
    *
    * @return  the old element stored for the same point view, if it existed
    */
   def update( elem: A ) : Option[ A ]

   def rangeQuery[ @specialized( Long ) Area ]( qs: QueryShape[ Area, D ]) : Iterator[ A ]

   /**
    * Reports the nearest neighbor entry with respect to
    * a given point.
    *
    * Note: There is a potential numeric overflow if the
    * squared distance of the query point towards the
    * furthest corner of the tree's root hyper-cube exceeds 63 bits.
    * For a root `Square( 0x40000000, 0x40000000, 0x40000000 )`, this
    * happens for example for any point going more towards north-west
    * than `Point2DLike( -1572067139, -1572067139 )`.
    *
    * @param   point the point of which the nearest neighbor is to be found
    * @param   a threshold which is an acceptable abortion criterion. I.e.,
    *    if a point is found whose distance is smaller or equal to this
    *    value, the search is immediately terminated and that entry is returned
    *
    * @throws  NoSuchElementException  if the tree is empty
    */
   def nearestNeighbor[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ]) : A

   def nearestNeighborOption[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ]) : Option[ A ]

   /**
    * An `Iterator` which iterates over the points stored
    * in the octree, using an in-order traversal directed
    * by the orthant indices of the nodes of the tree
    */
   def iterator : Iterator[ A ]

   /* sealed */ trait Q
   trait QEmpty extends Q
   sealed trait QNonEmpty extends Q
   trait QLeaf extends QNonEmpty {
      def value: A
   }
   trait QNode extends QNonEmpty {
      def hyperCube: D#HyperCube
      def child( idx: Int ) : Q
      /* final */ def prevOption: Option[ QNode ] // = Option( prev )
      /* final */ def nextOption: Option[ QNode ] // = Option( next )

      // XXX todo: how can we make this implementation private?
      /* protected[SkipOctree] */ def prev: QNode
      /* protected[SkipOctree] */ def next: QNode
   }
}