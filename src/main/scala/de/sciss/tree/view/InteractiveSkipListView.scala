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

package de.sciss.tree.view

import de.sciss.tree.LLSkipList
import java.awt.event.{ActionListener, ActionEvent}
import java.awt.{EventQueue, FlowLayout, BorderLayout, Dimension}
import javax.swing.{WindowConstants, JFrame, JTextField, JButton, JPanel}

/**
 * Simple GUI app to probe the LLSkipList interactively.
 */
object InteractiveSkipListView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run {
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
class InteractiveSkipListView extends JPanel( new BorderLayout() ) {
   val l    = LLSkipList.empty( Int.MaxValue )
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
   but( "Add" ) { tryNum { i =>
      status( l.add( i ).toString )
      slv.highlight = Some( i )
   }}
   but( "Contains" ) { tryNum { i =>
      status( l.contains( i ).toString )
      slv.highlight = Some( i )
   }}
   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )
}