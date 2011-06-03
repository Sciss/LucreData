/*
 *  LLSkipListTest.scala
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

package de.sciss.tree

import java.awt.{BorderLayout, EventQueue}
import javax.swing.{WindowConstants, JFrame}
import view.{HASkipListView, LLSkipListView}

object SkipListTest extends App with Runnable {
   EventQueue.invokeLater( this )

   def run {
      val f    = new JFrame( "Skip Lists" )
      f.setResizable( false )
      val cp   = f.getContentPane
      val ll   = LLSkipList.empty
      val ha   = HASkipList.withIntKey( (i: Int) => i )
      val l1   = List( 9, 13, 30 ) // List( 9, 13, 30, 39, 41, 48, 51, 53, 55, 60 )
      val l2   = Nil // List( 20, 21, 61 )

      Seq( ll, ha ).foreach( l => (l1 ++ l2).foreach( i => { println( i ); l.add( i )}))

      val llv = new LLSkipListView( ll )
      val hav = new HASkipListView( ha )
//      cp.add( llv, BorderLayout.CENTER )
      cp.add( llv, BorderLayout.NORTH )
      cp.add( hav, BorderLayout.SOUTH )
      f.pack()
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }
}