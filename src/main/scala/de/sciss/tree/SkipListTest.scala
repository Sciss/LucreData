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

import view.{HASkipListView, LLSkipListView}
import javax.swing.{JComponent, SwingConstants, JLabel, BoxLayout, Box, WindowConstants, JFrame}
import java.awt.{Color, BorderLayout, EventQueue}

object SkipListTest extends App with Runnable {
   val n = 50  // <= 25 to show the same list for the LL implementation!

   EventQueue.invokeLater( this )

   def run {
      val f    = new JFrame( "Skip Lists" )
      f.setResizable( false )
      val cp   = f.getContentPane
      val ll   = LLSkipList.empty
      val ha   = HASkipList.empty[ Int ]( 0x7FFFFFFF ) // .withIntKey( (i: Int) => i )
      val ha2  = HASkipList.empty[ Int ]( 0x7FFFFFFF ) // .withIntKey( (i: Int) => i, minGap = 2 )
      val ha3  = HASkipList.empty[ Int ]( 0x7FFFFFFF ) // .withIntKey( (i: Int) => i, minGap = 6 )
//      val l    = List( 9, 13, 30, 39, 41, 48, 51, 53, 55, 60 ) ++ List( 20, 21, 61 )
      val rnd  = new util.Random() // ( 0L )
      val l    = List.fill( n )( rnd.nextInt( 100 ))
//      val l      = List( 60, 48, 29, 47 )
      println( l )

      l.take( 25 ).foreach( ll.add( _ ))
      l.foreach { i =>
//         if( i == 19 ) {
//            println( "HERE" )
//         }
         ha.add( i )
      }
      l.foreach( ha2.add( _ ))
      l.foreach( ha3.add( _ ))

      cp.setLayout( new BoxLayout( cp, BoxLayout.Y_AXIS ))
      val llv = new LLSkipListView( ll )
      val hav = new HASkipListView( ha )
      val hav2 = new HASkipListView( ha2 )
      val hav3 = new HASkipListView( ha3 )

      def addBox( c: JComponent, label: String ) {
         if( cp.getComponentCount > 0 ) cp.add( Box.createVerticalStrut( 8 ))
         val lbBox = Box.createHorizontalBox()
//         lbBox.setBackground( Color.white )
         lbBox.add( Box.createHorizontalGlue() )
         lbBox.add( new JLabel( label ))
         lbBox.add( Box.createHorizontalGlue() )
         val cBox = Box.createHorizontalBox()
         cBox.add( Box.createHorizontalGlue() )
         cBox.add( c )
         cBox.add( Box.createHorizontalGlue() )
         cp.add( lbBox )
         cp.add( Box.createVerticalStrut( 2 ))
         cp.add( cBox )
      }

//      cp.add( llv, BorderLayout.CENTER )
      addBox( llv,  "LL 1-3 DSL" )
      addBox( hav,  "HA 1-3 DSL" )
      addBox( hav2, "HA 2-5 DSL" )
      addBox( hav3, "HA 6-13 DSL" )
      f.pack()
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }
}