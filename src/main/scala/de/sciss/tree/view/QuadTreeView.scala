package de.sciss.tree
package view

import java.awt.{Color, RenderingHints, Graphics2D, Graphics, Dimension}
import javax.swing.{BorderFactory, JComponent}

class QuadTreeView( t: QuadTree[ _ ]) extends JComponent {
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

   private def draw( g2: Graphics2D, quad: Quad[ _ ]) {
      def drawFrame {
         g2.setColor( Color.black )
         val c = quad.center
         val e = quad.extent
         val w = e * 2
         g2.drawRect( c.x - e, c.y - e, w, w )
      }
      quad match {
         case t: QuadTree[ _ ]   => List( t.nw, t.ne, t.sw, t.se ).foreach( draw( g2, _ ))
         case _: QuadEmpty[ _ ]  => drawFrame
         case l: QuadLeaf[ _ ]   =>
            drawFrame
            g2.setColor( Color.red )
            g2.fillOval( l.point.x - 2, l.point.y - 2, 5, 5 )
      }
   }
}