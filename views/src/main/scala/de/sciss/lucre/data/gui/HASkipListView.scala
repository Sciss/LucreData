/*
 *  HASkipListView.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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

import java.awt.{Color, Point, Rectangle, Graphics2D}
import stm.{Cursor, Sys}

class HASkipListView[ S <: Sys[ S ], A ]( access: S#Tx => HASkipList.Set[ S, A ])( implicit cursor: Cursor[ S ])
extends SkipListView[ A ] {
   import HASkipList.Set.Node

//   private val stm      = l.system

   private val maxGap : Int = cursor.step( l( _ ).maxGap )

   def l( implicit tx: S#Tx ) : HASkipList.Set[ S, A ] = access( tx )

   private def buildBoxMap( n: Node[ S, A ], isRight: Boolean )( implicit tx: S#Tx ) : (Box, NodeBox) = {
      val sz   = n.size
      val szm  = sz - 1
      val keys = IndexedSeq.tabulate( sz ) { i =>
         val key     = n.key( i )
         (key, if( isRight && i == szm ) "M" else key.toString)
      }
      val chbo = if( n.isLeaf ) None else {
         val nb = n.asBranch
         Some( IndexedSeq.tabulate( sz )( i => buildBoxMap( nb.down( i ), isRight && (i == szm) )))
      }
      val b    = NodeBox( n, keys, chbo.map( _.map( _._2 )))
      val bb   = chbo match {
         case Some( chbt ) =>
            val chb  = chbt.map( _._1 )
            val h    = Horiz( bs = chb )
            Vert( bs = IndexedSeq( b, h ))
         case None => b
      }

      (bb, b)
   }

  protected def paintList(g2: Graphics2D): Unit = {
      cursor.step { implicit tx =>
         l.top match {
            case Some( n ) =>
               val (bb, nb) = buildBoxMap( n, isRight = true )
               bb.moveTo( 0, 0 )
               drawNode( g2, nb )
            case _ =>
         }
      }
   }

  private def drawNode(g2: Graphics2D, b: NodeBox, arr: Option[Point] = None)(implicit tx: S#Tx): Unit = {
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
      for( i <- 0 until b.keys.size ) {
         val x1 = x + (i * 23)
         val (key, keyStr) = b.keys( i )
         g2.setColor( if( keyStr != "M" ) highlight.getOrElse( key, Color.black ) else Color.black )
         g2.drawString( keyStr, x1 + 4, y + 17 )
         b.downs.foreach { downs =>
            drawNode( g2, downs( i ), Some( new Point( x1 + 11, y + 36 )))
         }
      }
   }

  private trait Box {
    var r = new Rectangle()

    def moveTo(x: Int, y: Int): Unit = {
      r.x = x
      r.y = y
      updateChildren()
    }

    def updateChildren(): Unit
  }

  private case class Horiz( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box {
      r.width  = bs.map( _.r.width ).sum + ((bs.size - 1) * spacing)
      r.height = bs.map( _.r.height ).max

      def updateChildren(): Unit = {
         var x = r.x
         bs.foreach { b =>
            b.moveTo( x, r.y + ((r.height - b.r.height) >> 1) )
            x = b.r.x + b.r.width + spacing
         }
      }
   }

   private final case class Vert( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box {
      r.width  = bs.map( _.r.width ).max
      r.height = bs.map( _.r.height ).sum + ((bs.size - 1) * spacing)

      def updateChildren(): Unit = {
         var y = r.y
         bs.foreach { b =>
            b.moveTo( r.x + ((r.width - b.r.width) >> 1), y )
            y = b.r.y + b.r.height + spacing
         }
      }
   }

   private final case class NodeBox( n: Node[ S, A ], keys: IndexedSeq[ (A, String) ], downs: Option[ IndexedSeq[ NodeBox ]])
   extends Box {
      r.width  = 23 * (/*l.*/maxGap + 1) + 1
      r.height = if( n.isLeaf ) 23 else 46

      def updateChildren() = ()
   }
}