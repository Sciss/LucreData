package de.sciss.tree

import java.awt.EventQueue
import view.QuadTreeView
import javax.swing.{WindowConstants, JFrame}

object QuadTreeTest extends App with Runnable {
   EventQueue.invokeLater( this )

   def run {
      val f    = new JFrame( "QuadTree" )
      f.setResizable( false )
      val cp   = f.getContentPane
      val t    = QuadTree[ Unit ]( Point( 0, 0 ), 256 )
      t.insert( Point( -128, 128 ), () )
      t.insert( Point( 232, -248 ), () )
      t.insert( Point( 248, -232 ), () )
      val v    = new QuadTreeView( t )
      cp.add( v, "Center" )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }
}