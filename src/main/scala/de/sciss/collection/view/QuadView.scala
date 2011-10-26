/*
 *  QuadView.scala
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
package view

import java.awt.{Color, RenderingHints, Graphics2D, Graphics}
import javax.swing.{BorderFactory, JComponent}
import geom.{SquareLike, Point2DLike}

object QuadView {
   private val colrGreen = new Color( 0x00, 0xC0, 0x00 )

   case class PaintHelper( g2: Graphics2D ) {
      var scale: Double = 1.0

      def drawFrame( quad: SquareLike, color: Color = Color.black ) {
         g2.setColor( color )
         val e = quad.extent
         val w = ((e.toLong << 1) * scale + 0.5).toInt
         g2.drawRect( ((quad.cx - e) * scale + 0.5).toInt, ((quad.cy - e) * scale + 0.5).toInt, w, w )
      }

      def translate( x: Int, y: Int ) { g2.translate( x, y )}

      def drawPoint( point: Point2DLike, highlight: Boolean = false ) {
         g2.setColor( if( highlight ) colrGreen else Color.red )
         g2.fillOval( (point.x * scale + 0.5).toInt - 2, (point.y * scale + 0.5).toInt - 2, 5, 5 )
      }
   }
}
abstract class QuadView extends JComponent {
   setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ))
   setBackground( Color.white )

   var topPainter = Option.empty[ QuadView.PaintHelper => Unit ]

   protected def draw( h: QuadView.PaintHelper ) : Unit

   override def paintComponent( g: Graphics ) {
      val g2 = g.asInstanceOf[ Graphics2D ]
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      val atOrig = g2.getTransform
      val in = getInsets
      g2.setColor( getBackground )
      g2.fillRect( 0, 0, getWidth, getHeight )
      g2.setColor( getForeground )
      g2.translate( in.left, in.top )
      val at2 = g2.getTransform
      val h = QuadView.PaintHelper( g2 )
      draw( h )
      topPainter.foreach { fun =>
         g2.setTransform( at2 )
         g2.setColor( getForeground )
         fun( h )
      }
      g2.setTransform( atOrig )
   }
}