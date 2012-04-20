/*
 *  DeterministicSkipQuadtree.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

import geom.Space

/**
 * A thin factory for a 2D Octree aka Quadtree.
 */
object DeterministicSkipQuadtree {
   import Space.TwoDim
   import TwoDim._

   def empty[ A ]( quad: HyperCube, skipGap: Int = 2 )( implicit view: A => PointLike ) : SkipOctree[ Space.TwoDim, A ] =
      DeterministicSkipOctree.empty[ TwoDim, A ]( TwoDim, quad, skipGap )

   def apply[ A <% PointLike ]( quad: HyperCube, skipGap: Int = 2 )( xs: A* ) : SkipOctree[ Space.TwoDim, A ] = {
      val t = empty[ A ]( quad, skipGap )
      xs.foreach( t.+=( _ ))
      t
   }
}