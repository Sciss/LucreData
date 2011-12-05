/*
 *  InteractiveSkipListView.scala
 *  (LucreData)
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

package de.sciss.collection.mutable
package view

import java.awt.event.{ActionListener, ActionEvent}
import java.awt.{Color, EventQueue, FlowLayout, BorderLayout, Dimension}
import javax.swing.{Box, JLabel, SwingConstants, WindowConstants, JFrame, JTextField, JButton, JPanel}
import de.sciss.collection.view.PDFSupport

/**
 * Simple GUI app to probe the LLSkipList interactively.
 */
object InteractiveSkipListView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run() {
      val mode = args.toSeq match {
         case Seq( "-a", sz ) => HA( sz.toInt )
         case _ => LL
      }

      val f    = new JFrame( "SkipList" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = new InteractiveSkipListView( mode )
      cp.add( iv, BorderLayout.CENTER )

      PDFSupport.addMenu( f, Seq( iv ), usePrefSize = false )

      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }

   sealed trait Mode
   case object LL extends Mode
   final case class HA( gap: Int ) extends Mode
}
class InteractiveSkipListView( mode: InteractiveSkipListView.Mode )
extends JPanel( new BorderLayout() ) with SkipList.KeyObserver[ Int ] {
   view =>

   import InteractiveSkipListView._

   private val rnd   = new util.Random( 1L )

//   private var obsSet = Set.empty[ Int ]
   private var obsUp = IndexedSeq.empty[ Int ]
   private var obsDn = IndexedSeq.empty[ Int ]

   val (l, slv) = mode match {
      case LL =>
         val _l   = LLSkipList.empty[ Int ]( keyObserver = this )
         val _slv = new LLSkipListView( _l )
         (_l, _slv)

      case HA( gap ) =>
         val _l   = HASkipList.empty[ Int ]( minGap = gap, keyObserver = view )
         val _slv = new HASkipListView( _l )
         (_l, _slv)
   }

   slv.setPreferredSize( new Dimension( 16 * 64 + 16, 3 * 64 + 16 ))
//slv.setBorder( BorderFactory.createLineBorder( Color.red, 2 ))

   add( slv, BorderLayout.CENTER )
   private val p = new JPanel( new FlowLayout() )
   private def but( lb: String )( action: => Unit ) {
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
   private def space() {
      p.add( Box.createHorizontalStrut( 8 ))
   }
   private def label( text: String ) {
      val l = new JLabel( text, SwingConstants.RIGHT )
//      l.putClientProperty( "JComponent.sizeVariant", "mini" )
      p.add( l )
   }
   private val ggNum = new JTextField( 2 )
   p.add( ggNum )
   private def tryNum( fun: Int => Unit ) { try { val i = ggNum.getText.toInt; fun( i )} catch { case nfe: NumberFormatException => }}
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

//Seq.tabulate( 9 )( i => l.add( (i+1) * 10 ))
////l.add( 54 ); l.add( 55 ); l.add( 56 )
////l.add( 24 ); l.add( 25 ); l.add( 26 )
//l.remove(60);l.remove(20)
   butAddRemove( "Add" )( l.add( _ ))
   butAddRemove( "Remove" ) { key =>
      l.remove( key )
   }

   but( "Contains" ) { tryNum { i =>
      status( l.contains( i ).toString )
      slv.highlight = Map( i -> Color.blue )
   }}

   space()
   label( "Randomly:" )

   private def addRandom( num: Int ) {
      obsUp = IndexedSeq.empty
      obsDn = IndexedSeq.empty
      val ps = Seq.fill( num )( rnd.nextInt( 100 ))
      status( ps.lastOption.map( _.toString ).getOrElse( "" ))
      ps.foreach( l add _ )
      slv.highlight = (obsUp.map( _ -> colrGreen ) ++ obsDn.map( _ -> Color.red ) ++ ps.map( _ -> Color.blue )).toMap
   }

   but( "Add 1x" )  { addRandom(  1 )}
   but( "Add 10x" ) { addRandom( 10 )}

//but( "Height" ) {
//   println( l.height )
//}

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