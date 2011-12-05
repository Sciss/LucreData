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

package de.sciss.collection.txn
package view

import java.awt.event.{ActionListener, ActionEvent}
import java.awt.{Color, EventQueue, FlowLayout, BorderLayout, Dimension}
import javax.swing.{Box, JLabel, SwingConstants, WindowConstants, JFrame, JTextField, JButton, JPanel}
import java.io.File
import de.sciss.lucrestm.{BerkeleyDB, Sys, InMemory}
import de.sciss.collection.view.PDFSupport

/**
* Simple GUI app to probe the txn.HASkipList interactively.
*/
object InteractiveSkipListView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run() {
      val a = args.headOption.getOrElse( "" )
      val iv = if( a.startsWith( "--db" )) {
         val dir     = if( a == "--dbtmp" ) {
            File.createTempFile( "skiplist", "_database" )
         } else {
            new File( sys.props( "user.home" ), "skiplist_database" )
         }
         dir.delete()
         dir.mkdir()
         val f       = new File( dir, "data" )
         println( f.getAbsolutePath )
         implicit val system = BerkeleyDB.open( f )
         new InteractiveSkipListView[ BerkeleyDB ]( obs => system.atomic { implicit tx =>
            implicit val reader = HASkipList.reader[ BerkeleyDB, Int ]( obs )
            system.root[ HASkipList[ BerkeleyDB, Int ]] {
               HASkipList.empty[ BerkeleyDB, Int ]( minGap = 1, keyObserver = obs )
            }
         })

      } else {
         implicit val system = new InMemory
         new InteractiveSkipListView[ InMemory ]( obs => system.atomic { implicit tx =>
            HASkipList.empty[ InMemory, Int ]( minGap = 1, keyObserver = obs )
         })
      }

      val f    = new JFrame( "SkipList" )
      val cp   = f.getContentPane
//      val iv   = new InteractiveSkipListView( system )
      cp.add( iv, BorderLayout.CENTER )

      PDFSupport.addMenu( f, Seq[ InteractiveSkipListView[ _ ]]( iv ), usePrefSize = false )

      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }
}
class InteractiveSkipListView[ S <: Sys[ S ]]( _create: SkipList.KeyObserver[ S#Tx, Int ] => HASkipList[ S, Int ])
extends JPanel( new BorderLayout() ) with SkipList.KeyObserver[ S#Tx, Int ] {
   view =>

   private val rnd   = new util.Random( 1L )
   private var obsUp = IndexedSeq.empty[ Int ]
   private var obsDn = IndexedSeq.empty[ Int ]

//   val l = {
//      implicit val sys = system
//      sys.atomic { implicit tx => txn.HASkipList.empty[ S, Int ]( minGap = 1, keyObserver = view )}
//   }
   val l = _create( this )
   val slv = new HASkipListView( l )

   slv.setPreferredSize( new Dimension( 16 * 64 + 16, 3 * 64 + 16 ))

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
      p.add( l )
   }
   private val ggNum = new JTextField( 2 )
   p.add( ggNum )
   private def tryNum( fun: Int => Unit ) { try { val i = ggNum.getText.toInt; fun( i )} catch { case nfe: NumberFormatException => }}
   private val ggStatus = new JTextField( 12 )
   ggStatus.setEditable( false )
   private def status( str: String ) { ggStatus.setText( str )}
   private val colrGreen = new Color( 0x00, 0xA0, 0x00 )
   def butAddRemove( name: String )( fun: (S#Tx, Int) => Boolean ) { but( name ) { tryNum { i =>
      obsUp = IndexedSeq.empty
      obsDn = IndexedSeq.empty
      val res = l.system.atomic( tx => fun( tx, i ))
      status( res.toString )
      slv.highlight = (obsUp.map( _ -> colrGreen ) ++ obsDn.map( _ -> Color.red )).toMap + (i -> Color.blue)
   }}}

//   private def atomic[ A ]( fun: InTxn => A ) : A = {
//      TxnExecutor.defaultAtomic( fun )
//   }

   butAddRemove( "Add" )( (txn, key) =>
      l.add( key )( txn )
   )
   butAddRemove( "Remove" ) { (txn, key) =>
      l.remove( key )( txn )
   }

   but( "Contains" ) { tryNum { key =>
      val res = l.system.atomic { implicit tx => l.contains( key )}
      status( res.toString )
      slv.highlight = Map( key -> Color.blue )
   }}

   space()
   label( "Randomly:" )

   private def addRandom( num: Int ) {
      obsUp = IndexedSeq.empty
      obsDn = IndexedSeq.empty
      val ps = Seq.fill( num )( rnd.nextInt( 100 ))
println( ps )
      status( ps.lastOption.map( _.toString ).getOrElse( "" ))
      l.system.atomic { implicit tx =>
         ps.foreach( l add _ )
      }
      slv.highlight = (obsUp.map( _ -> colrGreen ) ++ obsDn.map( _ -> Color.red ) ++ ps.map( _ -> Color.blue )).toMap
   }

   but( "Add 1x" )  { addRandom(  1 )}
   but( "Add 10x" ) { addRandom( 10 )}

   space()
   but( "Print List" ) {
      println( l.system.atomic( implicit tx => l.toList ))
   }

   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )

   def keyUp( key: Int )( implicit tx: S#Tx ) {
      println( "Lvl up:   " + key )
      obsUp :+= key
   }
   def keyDown( key: Int )( implicit tx: S#Tx ) {
      println( "Lvl down: " + key )
      obsDn :+= key
   }
}