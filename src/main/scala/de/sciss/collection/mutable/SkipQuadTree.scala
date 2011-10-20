/*
 *  SkipQuadTree.scala
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
 *
 *
 *  Changelog:
 */

package de.sciss.collection
package mutable

import collection.mutable.{Set => MSet}
import geom.{Quad, QueryShape, DistanceMeasure, PointLike}

/**
 * A `SkipQuadTree` is a two-dimensional data structure that
 * maps coordinates to values. It extends the interface
 * of scala's mutable `Map` and adds further operations such
 * as range requires and nearest neighbour search.
 */
trait SkipQuadTree[ A ] extends MSet[ A ] {
   def headTree: QNode
   def lastTree: QNode
   def pointView : A => PointLike // PointView[ A ]

   def quad : Quad = headTree.quad

   def numLevels : Int

   def get( point: PointLike ) : Option[ A ]
//   def apply( point: PointLike ) : A = get.getOrElse( throw new )
   def isDefinedAt( point: PointLike ) : Boolean

   def removeAt( point: PointLike ) : Option[ A ]

   /**
    * Adds an element to the tree
    *
    * @return  true if the element is new in the tree. If a previous entry with the
    *          same point view is overwritten, this is true if the elements were
    *          equal, false otherwise
    */
   def add( elem: A ) : Boolean

   /**
    * Adds an element to the tree
    *
    * @return  the old element stored for the same point view, if it existed
    */
   def update( elem: A ) : Option[ A ]

   def rangeQuery( qs: QueryShape ) : Iterator[ A ]

   /**
    * Reports the nearest neighbor entry with respect to
    * a given point.
    *
    * Note: There is a potential numeric overflow if the
    * squared distance of the query point towards the
    * furthest corner of the tree's root quad exceeds 63 bits.
    * For a root `Quad( 0x40000000, 0x40000000, 0x40000000 )`, this
    * happens for example for any point going more towards north-west
    * than `PointLike( -1572067139, -1572067139 )`.
    *
    * @param   point the point of which the nearest neighbor is to be found
    * @param   a threshold which is an acceptable abortion criterion. I.e.,
    *    if a point is found whose distance is smaller or equal to this
    *    value, the search is immediately terminated and that entry is returned
    *
    * @throws  NoSuchElementException  if the tree is empty
    */
   def nearestNeighbor( point: PointLike, metric: DistanceMeasure = DistanceMeasure.euclideanSq ) : A

//   =
//      nearestNeighborOption( point, metric ).getOrElse(
//         throw new NoSuchElementException( "nearestNeighbor of an empty tree" ))

   def nearestNeighborOption( point: PointLike, metric: DistanceMeasure = DistanceMeasure.euclideanSq ) : Option[ A ]

//   /**
//    * Searches for the smallest rectangle containing a query point defined by an isomorphic
//    * function that establishes the orientation of any point in the tree with respect to
//    * the transformed query point. The two dimensional orientation is collapsed into a
//    * single `Int` according to the following scheme:
//    *
//    * {{{
//    *   5   4    7
//    *     +---+
//    *   1 | 0 |  3
//    *     +---+
//    *  13  12   15
//    *  }}}
//    *
//    *
//    *  Therefore the horizontal orientation can be extracted
//    *  with `_ & 3`, and the vertical orientation with `_ >> 2`,
//    *  where orientation is 0 for 'parallel', 1 for 'before' and
//    *  '3' for 'after', so that if the orient is before or
//    *  after, the sign can be retrieved via `_ - 2`
//
//    * @param   orient   a function which must return the orientation of the given point
//    *                   wrt the query point, according to the described schema
//    * @return  the search result which is either a `PointLike` object if the query
//    *    revealed its identity (the point view of the element!),
//    *    or the minimum empty rectangle covering possible
//    *    area of the transformed query point.
//    */
//   def isomorphicQuery( orient: A => Int ) : RectangleLike

   /**
    * An `Iterator` which iterates over the points stored
    * in the quadtree, using an in-order traversal directed
    * by the quadrant indices of the nodes of the tree
    */
   def iterator : Iterator[ A ]

   trait Q
   trait QEmpty extends Q
   trait QLeaf extends Q {
      def value: A
   }
   trait QNode extends Q {
      def quad: Quad
      def child( idx: Int ) : Q
      def prevOption: Option[ QNode ]
      def nextOption: Option[ QNode ]
   }
}