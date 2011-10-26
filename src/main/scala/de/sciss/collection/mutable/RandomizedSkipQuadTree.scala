/*
 *  RandomizedSkipQuadtree.scala
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

package de.sciss.collection
package mutable

import geom.{Space, Quad2D, Point2DLike}

/**
 * A thin factory for a 2D octree aka quadtree.
 */
object RandomizedSkipQuadtree {
   import RandomizedSkipOctree.Coin

   def empty[ A ]( quad: Quad2D, coin: Coin = Coin() )( implicit view: A => Point2DLike ) : SkipQuadtree[ A ] =
      RandomizedSkipOctree.empty[ Space.TwoDim, A ]( Space.TwoDim, quad, coin ) // new TreeImpl[ A ]( quad, view )

   def apply[ A <% Point2DLike ]( quad: Quad2D, coin: Coin = Coin() )( xs: A* ) : SkipQuadtree[ A ] = {
      val t = empty[ A ]( quad, coin )
      xs.foreach( t.+=( _ ))
      t
   }
}