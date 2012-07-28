/*
 *  SkipListView.scala
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

package de.sciss.lucre
package data
package gui

import javax.swing.JComponent
import java.awt.{RenderingHints, Graphics2D, Graphics, Font, Color}
import java.awt.geom.{AffineTransform, GeneralPath}

abstract class SkipListView[ A ] extends JComponent {
   setBackground( Color.white )
   setForeground( Color.black )
   setFont( new Font( "Serif", Font.ITALIC, 15 ))

   var highlight = Map.empty[ A, Color ]

   override def paintComponent( g: Graphics ) {
      val g2      = g.asInstanceOf[ Graphics2D ]
      g2.setColor( getBackground )
      g2.fillRect( 0, 0, getWidth, getHeight )
      g2.setColor( getForeground )
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      val atOrig  = g2.getTransform
      g2.translate( 4, 4 )
      paintList( g2 )
      g2.setTransform( atOrig )
   }

   protected def drawArrow( g2: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int ) {
      g2.fillOval( x1 - 1, y1 - 2, 3, 3 )
      g2.drawLine( x1, y1, x2, y2 )
      val p    = new GeneralPath()
      val rad  = math.atan2( y2 - y1, x2 - x1 )
      p.moveTo( 1, 0.5f )
      p.lineTo( -5, -2 )
      p.lineTo( -5, 3 )
      p.closePath()
//      val at = AffineTransform.getRotateInstance( rad )
//      at.translate( x2, y2 )
      val at = AffineTransform.getTranslateInstance( x2, y2 )
      at.rotate( rad )
      g2.fill( p.createTransformedShape( at ))
   }

   protected def paintList( g2: Graphics2D ) : Unit
}