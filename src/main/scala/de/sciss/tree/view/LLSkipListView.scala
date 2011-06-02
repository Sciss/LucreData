package de.sciss.tree.view

import javax.swing.JComponent
import de.sciss.tree.LLSkipList
import sys.error
import java.awt.{Polygon, Font, Color, Dimension, Graphics, RenderingHints, Graphics2D}

// suckers

class LLSkipListView( l: LLSkipList ) extends JComponent {
   setPreferredSize( new Dimension( (l.size + 1) * 64 + 36, l.height * 64 + 36 ))
   setBackground( Color.white )
   setForeground( Color.black )
   setFont( new Font( "Serif", Font.ITALIC, 14 ))

   override def paintComponent( g: Graphics ) {
      val g2      = g.asInstanceOf[ Graphics2D ]
      g2.setColor( getBackground )
      g2.fillRect( 0, 0, getWidth, getHeight )
      g2.setColor( getForeground )
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      var x       = l.head
      var atOrig  = g2.getTransform
      g2.translate( 4, 4 )
      while( x ne l.bottom ) {
         val atRow   = g2.getTransform
         val x0      = x
         while( x ne l.tail ) {
            drawNode( g2, x )
            g2.translate( (gapSize( x ) + 1) * 64, 0 )
            x = x.right
         }
         g2.setTransform( atRow )
         g2.translate( 0, 64 )
         x = x0.down
      }
      g2.setTransform( atOrig )
   }

   private def drawNode( g2: Graphics2D, x: LLSkipList.Node ) {
      g2.drawRect( 0, 0, 48, 48 )
      g2.drawLine( 24, 0, 24, 48 )
      g2.drawLine( 0, 24, 48, 24 )
      val keyStr = if( x.key == LLSkipList.MAX_KEY ) "M" else x.key.toString
      g2.drawString( keyStr, 4, 20 )
      g2.fillOval( 34, 10, 4, 4 )
      val harrLen = gapSize( x ) * 64 + 27
      g2.drawLine( 36, 12, 36 + harrLen, 12 )
      if( x.right eq l.tail ) {
         g2.drawLine( 36 + harrLen + 1, 8, 36 + harrLen + 1, 16 )
         g2.drawLine( 36 + harrLen + 3, 9, 36 + harrLen + 3, 14 )
         g2.drawLine( 36 + harrLen + 5, 10, 36 + harrLen + 5, 12 )
      } else {
         val p = new Polygon()
         p.addPoint( 36 + harrLen + 1, 12 )
         p.addPoint( 36 + harrLen - 4, 10 )
         p.addPoint( 36 + harrLen - 4, 14 )
         g2.fillPolygon( p )
      }
      g2.fillOval( 10, 34, 4, 4 )
      val varrLen = 27
      g2.drawLine( 12, 36, 12, 36 + varrLen )
      if( x.down eq l.bottom ) {
         g2.drawLine( 8, 36 + varrLen + 1, 16, 36 + varrLen + 1 )
         g2.drawLine( 9, 36 + varrLen + 3, 14, 36 + varrLen + 3 )
         g2.drawLine( 10, 36 + varrLen + 5, 12, 36 + varrLen + 5 )
      } else {
         val p = new Polygon()
         p.addPoint( 12, 36 + varrLen + 1 )
         p.addPoint( 10, 36 + varrLen - 4 )
         p.addPoint( 14, 36 + varrLen - 4 )
         g2.fillPolygon( p )
      }
   }

   private def gapSize( x: LLSkipList.Node ) : Int = {
      if( x.down eq l.bottom ) 0 else {
         var y = x
         while( y.down ne l.bottom ) y = y.down
         var i = 0; while( y.key != x.key ) { y = y.right; i += 1 }
         i
      }
   }
}