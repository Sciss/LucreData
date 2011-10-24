/*
 *  QuadtreeView.scala
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

package de.sciss.collection.obsolete

import java.awt.Dimension
import de.sciss.collection.geom.Quad2D
import de.sciss.collection.view.QuadView

abstract class QuadtreeView extends QuadView {
   def rootQuad : Quad2D

   override def getPreferredSize : Dimension = {
      val w1   = rootQuad.extent * 2 + 1
      val in   = getInsets
      new Dimension( w1 + (in.left + in.right), w1 + (in.top + in.bottom) )
   }

   protected def drawTree( h: QuadView.PaintHelper ) : Unit

   protected def draw( h: QuadView.PaintHelper ) {
      val q = rootQuad
      h.translate( q.extent - q.cx, q.extent - q.cy )
      drawTree( h )
   }
}