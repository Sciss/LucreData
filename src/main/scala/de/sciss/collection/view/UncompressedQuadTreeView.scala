/*
 *  QuadTreeView.scala
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
package view

import mutable.QuadTree

class UncompressedQuadTreeView( t: QuadTree.Q[ _ ]) extends QuadTreeView {
   protected def drawTree( h: QuadView.PaintHelper ) {
      draw( h, t )
   }

   def rootQuad = t.quad

   private def draw( h: QuadView.PaintHelper, quad: QuadTree.Q[ _ ]) {
      quad match {
         case t: QuadTree.QNode[ _ ]   => List( t.nw, t.ne, t.sw, t.se ).foreach( draw( h, _ ))
         case _: QuadTree.QEmpty[ _ ]  => h.drawFrame( quad.quad )
         case l: QuadTree.QLeaf[ _ ]   =>
            h.drawFrame( quad.quad )
            h.drawPoint( l.point )
      }
   }
}