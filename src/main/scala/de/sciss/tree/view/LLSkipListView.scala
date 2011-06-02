/*
 *  LLSkipListView.scala
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

package de.sciss.tree.view

import javax.swing.JComponent
import de.sciss.tree.LLSkipList
import sys.error
import java.awt.{Polygon, Font, Color, Dimension, Graphics, RenderingHints, Graphics2D}
import java.awt.geom.GeneralPath

// suckers

class LLSkipListView( l: LLSkipList ) extends JComponent {
   setPreferredSize( new Dimension( (l.size + 1) * 64 + 16, l.height * 64 + 16 ))
   setBackground( Color.white )
   setForeground( Color.black )
   setFont( new Font( "Serif", Font.ITALIC, 15 ))

   override def paintComponent( g: Graphics ) {
      val g2      = g.asInstanceOf[ Graphics2D ]
      g2.setColor( getBackground )
      g2.fillRect( 0, 0, getWidth, getHeight )
      g2.setColor( getForeground )
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      var x       = l.top
      var atOrig  = g2.getTransform
      g2.translate( 4, 4 )
      while( !x.isBottom ) {
         val atRow   = g2.getTransform
         val x0      = x
         while( !x.isTail ) {
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
      g2.drawRect( 0, 0, 46, 46 )
      g2.drawLine( 23, 0, 23, 46 )
      g2.drawLine( 0, 23, 46, 23 )
      val keyStr = if( x.key == LLSkipList.MAX_KEY ) "M" else x.key.toString
      g2.drawString( keyStr, 4, 17 )
      g2.fillOval( 34, 10, 3, 3 )
      val harrLen = gapSize( x ) * 64 + 27
      g2.drawLine( 36, 11, 36 + harrLen - 1, 11 )
      if( x.right.isTail ) {
         g2.drawLine( 36 + harrLen, 7, 36 + harrLen, 16 )
         g2.drawLine( 36 + harrLen + 2, 9, 36 + harrLen + 2, 14 )
         g2.drawLine( 36 + harrLen + 4, 11, 36 + harrLen + 4, 12 )
      } else {
         val p = new GeneralPath()
         p.moveTo( 36 + harrLen + 1, 11.5f )
         p.lineTo( 36 + harrLen - 5, 9 )
         p.lineTo( 36 + harrLen - 5, 14 )
         p.closePath()
         g2.fill( p )
      }
      g2.fillOval( 10, 34, 3, 3 )
      val varrLen = 27
      g2.drawLine( 11, 36, 11, 36 + varrLen - 1 )
      if( x.down.isBottom ) {
         g2.drawLine( 7, 36 + varrLen, 16, 36 + varrLen )
         g2.drawLine( 9, 36 + varrLen + 2, 14, 36 + varrLen + 2 )
         g2.drawLine( 11, 36 + varrLen + 4, 12, 36 + varrLen + 4 )
      } else {
         val p = new GeneralPath()
         p.moveTo( 11.5f, 36 + varrLen + 1 )
         p.lineTo( 9, 36 + varrLen - 5 )
         p.lineTo( 14, 36 + varrLen - 5 )
         p.closePath()
         g2.fill( p )
      }
   }

   private def gapSize( x: LLSkipList.Node ) : Int = {
      if( x.down.isBottom ) 0 else {
         var y = x
         while( !y.down.isBottom ) y = y.down
         var i = 0; while( y.key != x.key ) { y = y.right; i += 1 }
         i
      }
   }
}