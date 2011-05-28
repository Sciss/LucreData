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

import java.awt.{BorderLayout, EventQueue}
import view.{CompressedQuadTreeView, QuadTreeView}
import javax.swing.{BoxLayout, JComponent, WindowConstants, JFrame}

object QuadTreeTest extends App {
   args.headOption match {
      case Some( "-fig1" ) => new Figure1
      case Some( "-fig2" ) => new Figure2
      case _ => println( """
Options:
-fig1
-fig2
""")
         sys.exit( 1 )
   }

   abstract class Figure extends Runnable {
      lazy val center  = Point( 256, 256 )
      lazy val extent  = 256
      lazy val points1 = Map(
         Point(  80, 410 ) -> (),
         Point( 488,   8 ) -> (),
         Point( 504,  24 ) -> ()
      )

      EventQueue.invokeLater( this )

      def views : Seq[ JComponent ]

      def run {
         val f    = new JFrame( "QuadTrees" )
         f.setResizable( false )
         val cp      = f.getContentPane
         val vs   = views
         cp.setLayout( new BoxLayout( cp, BoxLayout.X_AXIS ))
         vs.foreach( cp.add( _ ))
         f.pack()
         f.setLocationRelativeTo( null )
         f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
         f.setVisible( true )
      }
   }

   class Figure1 extends Figure {
      def views = {
         val map  = points1
         val t    = QuadTree.fromMap( center, extent, map )
         val v    = new QuadTreeView( t )

         val ct   = CompressedQuadTree.fromMap( Quad( center.x, center.y, extent ), map )
         val cv   = new CompressedQuadTreeView( ct )

         Seq( v, cv )
      }
   }

   class Figure2 extends Figure {
      def views = {
         val map = points1 ++ Map(
            Point( 400, 332 ) -> (),
            Point( 424, 368 ) -> (),
            Point( 200, 312 ) -> (),
            Point( 216, 296 ) -> (),
            Point( 240, 304 ) -> (),
            Point( 272, 496 ) -> (),
            Point( 300, 460 ) -> ()
         )

//         val map2 = Map(
//            Point( 240, 304 ) -> (),
////            Point( 504,  24 ) -> (),
//            Point( 200, 312 ) -> ()
//         )

         val ct   = CompressedQuadTree.fromMap( Quad( center.x, center.y, extent ), map )
         val cv   = new CompressedQuadTreeView( ct )
         Seq( cv )
      }
   }
}