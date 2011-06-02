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
      List( 9, 13, 30, 39, 41, 48, 51, 53, 55, 60 ).reverse.foreach( l.add( _ ))
      l.add( 20 )
      val v    = new LLSkipListView( l )
      cp.add( v, BorderLayout.CENTER )
      f.pack()
      f.setResizable( false )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }
}