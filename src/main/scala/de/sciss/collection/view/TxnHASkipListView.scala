/*
 *  TxnHASkipListView.scala
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

import java.awt.{Color, Point, Rectangle, Dimension, Graphics2D}
import de.sciss.lucrestm.Sys

class TxnHASkipListView[ S <: Sys[ S ], A ]( private val l: txn.HASkipList[ S, A ])
extends SkipListView[ A ] {
   private val stm      = l.system
   private var boxMap   = Map.empty[ l.Child, NodeBox ]

   setPreferredSize( rebuildMap() match {
      case Some( bb )   => new Dimension( bb.r.width + 9, bb.r.height + 9 )
      case None         => new Dimension( 54, 54 )
   })

   private def rebuildMap() : Option[ Box  ] = {
      boxMap = boxMap.empty

      stm.atomic { implicit tx =>
         l.top match {
            case n: l.Node =>
               val bb = buildBoxMap( n )
               bb.moveTo( 0, 0 )
               Some( bb )
            case _ => None
         }
      }
   }

   private def buildBoxMap( n: l.Node )( implicit tx: S#Tx ) : Box = {
      val b = NodeBox( n )
      boxMap += n -> b
      n match {
         case _: l.Leaf => b
         case nb: l.Branch =>
            val chb  = IndexedSeq.tabulate( n.size )( i => buildBoxMap( nb.down( i )))
            val h    = Horiz( bs = chb )
            Vert( bs = IndexedSeq( b, h ))
      }
   }

   protected def paintList( g2: Graphics2D ) {
      rebuildMap()
      stm.atomic { implicit tx => l.top match {
         case n: l.Node => drawNode( g2, n )
         case _ =>
      }}
   }

   private def drawNode( g2: Graphics2D, n: l.Node, arr: Option[ Point ] = None )( implicit tx: S#Tx ) {
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
            n match {
               case nb: l.Branch => drawNode( g2, nb.down( i ), Some( new Point( x1 + 11, y + 36 )))
               case _ =>
            }
         }
      }
   }

   private trait Box {
      var r = new Rectangle()
      def moveTo( x: Int, y: Int ) {
         r.x = x
         r.y = y
         updateChildren()
      }

      def updateChildren() : Unit
   }

   private case class Horiz( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box {
      r.width  = bs.map( _.r.width ).sum + ((bs.size - 1) * spacing)
      r.height = bs.map( _.r.height ).max

      def updateChildren() {
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

      def updateChildren() {
         var y = r.y
         bs.foreach { b =>
            b.moveTo( r.x + ((r.width - b.r.width) >> 1), y )
            y = b.r.y + b.r.height + spacing
         }
      }
   }

   private final case class NodeBox( n: l.Node ) extends Box {
      r.width  = 23 * (l.maxGap + 1) + 1
      r.height = n match {
         case _: l.Leaf => 23
         case _ => 46
      }

      def updateChildren() {}
   }
}