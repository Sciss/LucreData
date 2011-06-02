package de.sciss.tree

import view.LLSkipListView
import java.awt.{BorderLayout, EventQueue}
import javax.swing.{WindowConstants, JFrame}

object LLSkipListTest extends App with Runnable {
   EventQueue.invokeLater( this )

   def run {
      val f    = new JFrame( "LL Skip List" )
      val cp   = f.getContentPane
      val l    = LLSkipList.empty
      List( 9, 13, 30, 39, 41, 48, 51, 53, 55, 60 ).foreach( l.add( _ ))
      l.add( 20 )
      l.add( 21 )
//      l.add( 22 )
//      l.add( 23 )
//      l.add( 24 )
//      l.add( 25 )
//      l.add( 26 )
//      l.add( 27 )
//      l.add( 28 )
//      l.add( 29 )
//      l.add( 31 )
      val v    = new LLSkipListView( l )
      cp.add( v, BorderLayout.CENTER )
      f.pack()
      f.setResizable( false )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }
}