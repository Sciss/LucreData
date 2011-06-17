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

import collection.mutable.{Map => MMap}

/**
 * A `SkipQuadTree` is a two-dimensional data structure that
 * maps coordinates to values. It extends the interface
 * of scala's mutable `Map` and adds further operations such
 * as range requires and nearest neighbour search.
 */
trait SkipQuadTree[ V ] extends MMap[ Point, V ] {
   def headTree: QNode
   def lastTree: QNode

   def quad : Quad = headTree.quad

   def numLevels : Int = {
      var n = headTree
      val t = lastTree
      var i = 1; while( !(n eq t) ) {
         n = n.nextOption.orNull
         i += 1
      }
      i
   }

   def rangeQuery( qs: QueryShape ) : Iterator[ (Point, V) ]

   /**
    * Reports the nearest neighbor entry with respect to
    * a given point.
    *
    * XXX TODO: Should just return the tuple, not an option.
    * Because if the tree is non-empty the search is guaranteed to
    * succeed. Otherwise (tree empty) we can throw an error
    *
    * @param   point the point of which the nearest neighbor is to be found
    * @param   a threshold which is an acceptable abortion criterion. I.e.,
    *    if a point is found whose distance is smaller or equal to this
    *    value, the search is immediately terminated and that entry is returned
    */
   def nearestNeighbor( point: Point, abort: Int = 0 ) : Option[ (Point, V) ]

   /**
    * An `Iterator` which iterates over the points stored
    * in the quadtree, using an in-order traversal directed
    * by the quadrant indices of the nodes of the tree
    */
   def iterator : Iterator[ (Point, V) ]

   trait Q
   trait QEmpty extends Q
   trait QLeaf extends Q {
      def point: Point
      def value: V
   }
   trait QNode extends Q {
      def quad: Quad
      def child( idx: Int ) : Q
      def prevOption: Option[ QNode ]
      def nextOption: Option[ QNode ]
   }
}