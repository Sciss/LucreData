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

import java.awt.{Color, RenderingHints, Graphics2D, Graphics, Dimension}
import javax.swing.{BorderFactory, JComponent}

object QuadView {
   case class PaintHelper( g2: Graphics2D ) {
      def drawFrame( quad: Quad ) {
         g2.setColor( Color.black )
         val e = quad.extent
         val w = e * 2
         g2.drawRect( quad.cx - e, quad.cy - e, w, w )
      }

      def translate( x: Int, y: Int ) { g2.translate( x, y )}

      def drawPoint( point: Point ) {
         g2.setColor( Color.red )
         g2.fillOval( point.x - 2, point.y - 2, 5, 5 )
      }

   }
}
abstract class QuadView extends JComponent {
   setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ))

   protected def draw( h: QuadView.PaintHelper ) : Unit

   override def paintComponent( g: Graphics ) {
      val g2 = g.asInstanceOf[ Graphics2D ]
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      val atOrig = g2.getTransform
      val in   = getInsets()
//      val q    = rootQuad
//      g2.translate( q.extent - q.cx + in.left, q.extent - q.cy + in.top )
      g2.translate( in.left, in.top )
      draw( QuadView.PaintHelper( g2 ))
      g2.setTransform( atOrig )
   }
}