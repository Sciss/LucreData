/*
 *  InteractiveSkipListView.scala
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

import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.{WindowConstants, JFrame, JTextField, JButton, JPanel}
import java.awt.{Color, EventQueue, FlowLayout, BorderLayout, Dimension}
import mutable.{LLSkipList, SkipList}

/**
 * Simple GUI app to probe the LLSkipList interactively.
 */
object InteractiveSkipListView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run() {
      val f    = new JFrame( "SkipList" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = new InteractiveSkipListView
      cp.add( iv, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }
}
class InteractiveSkipListView extends JPanel( new BorderLayout() ) with SkipList.KeyObserver[ Int ] {
//   private var obsSet = Set.empty[ Int ]
   private var obsUp = IndexedSeq.empty[ Int ]
   private var obsDn = IndexedSeq.empty[ Int ]
   val l    = LLSkipList.empty[ Int ]( keyObserver =  this )
   val slv  = new LLSkipListView( l )
   slv.setPreferredSize( new Dimension( 16 * 64 + 16, 3 * 64 + 16 ))

   add( slv, BorderLayout.CENTER )
   private val p = new JPanel( new FlowLayout() )
   def but( lb: String )( action: => Unit ) {
      val b = new JButton( lb )
      b.setFocusable( false )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            action
            slv.repaint()
         }
      })
      p.add( b )
   }
   private val ggNum = new JTextField( 2 )
   p.add( ggNum )
   private def tryNum( fun: Int => Unit ) { try { val i = ggNum.getText().toInt; fun( i )} catch { case nfe: NumberFormatException => }}
   private val ggStatus = new JTextField( 12 )
   ggStatus.setEditable( false )
   private def status( str: String ) { ggStatus.setText( str )}
   private val colrGreen = new Color( 0x00, 0xA0, 0x00 )
   def butAddRemove( name: String )( fun: Int => Boolean ) { but( name ) { tryNum { i =>
      obsUp = IndexedSeq.empty
      obsDn = IndexedSeq.empty
      status( fun( i ).toString )
      slv.highlight = (obsUp.map( _ -> colrGreen ) ++ obsDn.map( _ -> Color.red )).toMap + (i -> Color.blue)
//      if( obsDn.nonEmpty ) println( "Lvl down: " + obsDn.mkString( ", " ))
//      if( obsUp.nonEmpty ) println( "Lvl up:   " + obsUp.mkString( ", " ))
   }}}

   butAddRemove( "Add" )( l.add( _ ))
   butAddRemove( "Remove" )( l.remove( _ ))

   but( "Contains" ) { tryNum { i =>
      status( l.contains( i ).toString )
      slv.highlight = Map( i -> Color.blue )
   }}
   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )

   def keyUp( key: Int ) {
      println( "Lvl up:   " + key )
      obsUp :+= key
   }
   def keyDown( key: Int ) {
      println( "Lvl down: " + key )
      obsDn :+= key
   }
}