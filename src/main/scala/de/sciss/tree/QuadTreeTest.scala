/*
 *  QuadTreeTest.scala
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

import javax.swing.{WindowConstants, JFrame}
import java.awt.{BorderLayout, EventQueue}
import view.{CompressedQuadTreeView, QuadTreeView}

object QuadTreeTest extends App with Runnable {
   EventQueue.invokeLater( this )

   def run {
      val f    = new JFrame( "QuadTree" )
      f.setResizable( false )
      val cp      = f.getContentPane
      val center  = Point( 256, 256 )
      val extent  = 256
      val map     =  Map(
         Point( 128, 384 ) -> (),
         Point( 488,   8 ) -> (),
         Point( 504,  24 ) -> ()
      )
      val t    = QuadTree.fromMap( center, extent, map )
      val v    = new QuadTreeView( t )

      val ct   = CompressedQuadTree.fromMap( Quad( center.x, center.y, extent ), map )
      val cv   = new CompressedQuadTreeView( ct )

      cp.add( v, BorderLayout.WEST )
      cp.add( cv, BorderLayout.EAST )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }
}