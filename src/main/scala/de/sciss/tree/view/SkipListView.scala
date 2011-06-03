/*
 *  SkipListView.scala
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
import java.awt.{RenderingHints, Graphics2D, Graphics, Font, Color}

abstract class SkipListView extends JComponent {
   setBackground( Color.white )
   setForeground( Color.black )
   setFont( new Font( "Serif", Font.ITALIC, 15 ))

   override def paintComponent( g: Graphics ) {
      val g2      = g.asInstanceOf[ Graphics2D ]
      g2.setColor( getBackground )
      g2.fillRect( 0, 0, getWidth, getHeight )
      g2.setColor( getForeground )
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      var atOrig  = g2.getTransform
      g2.translate( 4, 4 )
      paintList( g2 )
      g2.setTransform( atOrig )
   }

   protected def paintList( g2: Graphics2D ) : Unit
}