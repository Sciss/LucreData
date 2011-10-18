/*
 *  HASkipListView.scala
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

import mutable.HASkipList
import java.awt.{Color, Point, Rectangle, Dimension, Graphics2D}

class HASkipListView[ A ]( l: HASkipList[ A ]) extends SkipListView[ A ] {
   private var boxMap = Map.empty[ HASkipList.Node[ A ], NodeBox ]

   setPreferredSize( rebuildMap() match {
      case Some( bb )   => new Dimension( bb.r.width + 9, bb.r.height + 9 )
      case None         => new Dimension( 54, 54 )
   })

   private def rebuildMap() : Option[ Box  ] = {
      boxMap = boxMap.empty
      if( l.top.isBottom ) None else {
         val bb = buildBoxMap( l.top )
         bb.moveTo( 0, 0 )
         Some( bb )
      }
   }

   private def buildBoxMap( n: HASkipList.Node[ A ]) : Box = {
      val b = NodeBox( n )
      boxMap += n -> b
      if( n.down( 0 ).isBottom ) b else {
         val chb  = IndexedSeq.tabulate( n.size )( i => buildBoxMap( n.down( i )))
         val h    = Horiz( bs = chb )
         Vert( bs = IndexedSeq( b, h ))
      }
   }

   protected def paintList( g2: Graphics2D ) {
      rebuildMap()
      drawNode( g2, l.top )
   }

   private def drawNode( g2: Graphics2D, n: HASkipList.Node[ A ], arr: Option[ Point ] = None ) {
      boxMap.get( n ).foreach { b =>
         g2.setColor( Color.black )
         g2.draw( b.r )
         val x = b.r.x
         val y = b.r.y
         val w = b.r.width
         val h = b.r.height
         if( h > 23 ) g2.drawLine( x, y + 23, x + b.r.width, y + 23 )
         arr.foreach { pt =>
            drawArrow( g2, pt.x, pt.y, x + (w >> 1), y - 2 )
         }
         for( i <- 1 to l.maxGap ) {
            g2.drawLine( x + (i * 23), y, x + (i * 23), y + h )
         }
         for( i <- 0 until n.size ) {
            val x1      = x + (i * 23)
            val key     = n.key( i )
            val keyStr  = if( key == Int.MaxValue ) "M" else key.toString
            g2.setColor( highlight.getOrElse( key, Color.black ))
            g2.drawString( keyStr, x1 + 4, y + 17 )
            drawNode( g2, n.down( i ), Some( new Point( x1 + 11, y + 36 )))
         }
      }

//      g2.drawRect( 0, 0, 46, 46 )
//      g2.drawLine( 23, 0, 23, 46 )
//      g2.drawLine( 0, 23, 46, 23 )
//      val keyStr = if( x.key == HASkipList.MAX_KEY ) "M" else x.key.toString
//      g2.drawString( keyStr, 4, 17 )
//      g2.fillOval( 34, 10, 3, 3 )
//      val harrLen = gapSize( x ) * 64 + 27
//      g2.drawLine( 36, 11, 36 + harrLen - 1, 11 )
//      if( x.right.isTail ) {
//         g2.drawLine( 36 + harrLen, 7, 36 + harrLen, 16 )
//         g2.drawLine( 36 + harrLen + 2, 9, 36 + harrLen + 2, 14 )
//         g2.drawLine( 36 + harrLen + 4, 11, 36 + harrLen + 4, 12 )
//      } else {
//         val p = new GeneralPath()
//         p.moveTo( 36 + harrLen + 1, 11.5f )
//         p.lineTo( 36 + harrLen - 5, 9 )
//         p.lineTo( 36 + harrLen - 5, 14 )
//         p.closePath()
//         g2.fill( p )
//      }
//      g2.fillOval( 10, 34, 3, 3 )
//      val varrLen = 27
//      g2.drawLine( 11, 36, 11, 36 + varrLen - 1 )
//      if( x.down.isBottom ) {
//         g2.drawLine( 7, 36 + varrLen, 16, 36 + varrLen )
//         g2.drawLine( 9, 36 + varrLen + 2, 14, 36 + varrLen + 2 )
//         g2.drawLine( 11, 36 + varrLen + 4, 12, 36 + varrLen + 4 )
//      } else {
//         val p = new GeneralPath()
//         p.moveTo( 11.5f, 36 + varrLen + 1 )
//         p.lineTo( 9, 36 + varrLen - 5 )
//         p.lineTo( 14, 36 + varrLen - 5 )
//         p.closePath()
//         g2.fill( p )
//      }
   }

   private trait Box {
      var r = new Rectangle()
      def moveTo( x: Int, y: Int ) {
         r.x = x
         r.y = y
         updateChildren
      }

//      def calcDimensions : Unit
      def updateChildren : Unit
   }

   private case class Horiz( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box {
      r.width  = bs.map( _.r.width ).sum + ((bs.size - 1) * spacing)
      r.height = bs.map( _.r.height ).max

      def updateChildren {
         var x = r.x
         bs.foreach { b =>
            b.moveTo( x, r.y + ((r.height - b.r.height) >> 1) )
            x = b.r.x + b.r.width + spacing
         }
      }
   }

   private case class Vert( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box {
      r.width  = bs.map( _.r.width ).max
      r.height = bs.map( _.r.height ).sum + ((bs.size - 1) * spacing)

      def updateChildren {
         var y = r.y
         bs.foreach { b =>
            b.moveTo( r.x + ((r.width - b.r.width) >> 1), y )
            y = b.r.y + b.r.height + spacing
         }
      }
   }

   private case class NodeBox( n: HASkipList.Node[ A ]) extends Box {
//      def calcDimensions {
         r.width  = 23 * (l.maxGap + 1) + 1
         r.height = if( n.down( 0 ).isBottom ) 23 else 46
//      }

      def updateChildren {}
   }
}