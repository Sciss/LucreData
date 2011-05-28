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

package de.sciss.tree
package view

import java.awt.{Color, RenderingHints, Graphics2D, Graphics, Dimension}
import javax.swing.{BorderFactory, JComponent}

class QuadView( t: QuadTree.Q[ _ ]) extends JComponent {
   setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ))

   override def getPreferredSize : Dimension = {
      val w1   = t.extent * 2 + 1
      val in   = getInsets()
      new Dimension( w1 + (in.left + in.right), w1 + (in.top + in.bottom) )
   }

   override def paintComponent( g: Graphics ) {
      val g2 = g.asInstanceOf[ Graphics2D ]
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      val atOrig = g2.getTransform
      val in   = getInsets()
      g2.translate( t.extent - t.center.x + in.left, t.extent - t.center.y + in.top )
      draw( g2, t )
      g2.setTransform( atOrig )
   }

   private def draw( g2: Graphics2D, quad: QuadTree.Q[ _ ]) {
      def drawFrame {
         g2.setColor( Color.black )
         val c = quad.center
         val e = quad.extent
         val w = e * 2
         g2.drawRect( c.x - e, c.y - e, w, w )
      }
      quad match {
         case t: QuadTree.QNode[ _ ]   => List( t.nw, t.ne, t.sw, t.se ).foreach( draw( g2, _ ))
         case _: QuadTree.QEmpty[ _ ]  => drawFrame
         case l: QuadTree.QLeaf[ _ ]   =>
            drawFrame
            g2.setColor( Color.red )
            g2.fillOval( l.point.x - 2, l.point.y - 2, 5, 5 )
      }
   }
}