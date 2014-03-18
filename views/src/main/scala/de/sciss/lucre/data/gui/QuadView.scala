/*
 *  QuadView.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package data
package gui

import java.awt.{Color, RenderingHints, Graphics2D, Graphics}
import javax.swing.{BorderFactory, JComponent}
import geom.{IntSquareLike, IntPoint2DLike}

object QuadView {
   private val colrGreen = new Color( 0x00, 0xC0, 0x00 )

   case class PaintHelper( g2: Graphics2D ) {
      var scale: Double = 1.0

      def drawFrame( quad: IntSquareLike, color: Color = Color.black ) {
         g2.setColor( color )
         val e = quad.extent
         val w = ((e.toLong << 1) * scale + 0.5).toInt
         g2.drawRect( ((quad.cx - e) * scale + 0.5).toInt, ((quad.cy - e) * scale + 0.5).toInt, w, w )
      }

      def translate( x: Int, y: Int ) { g2.translate( x, y )}

      def drawPoint( point: IntPoint2DLike, highlight: Boolean = false ) {
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