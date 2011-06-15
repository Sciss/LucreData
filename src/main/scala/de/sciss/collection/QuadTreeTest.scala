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

package de.sciss.collection

import java.awt.EventQueue
import javax.swing.{BoxLayout, JComponent, WindowConstants, JFrame}
import view.{SkipQuadTreeView, CompressedQuadTreeView, UncompressedQuadTreeView}
import annotation.tailrec

object QuadTreeTest extends App {
   args.headOption match {
      case Some( "-fig1" ) => new Figure1
      case Some( "-fig2" ) => new Figure2
      case Some( "-test1" ) => new Test1
      case Some( "-test2" ) => new Test2
      case Some( "-test3" ) => new Test3
      case _ => println( """
Options:
-fig1
-fig2
-test1
-test2 (somehow a variant of fig. 5 -- only with high n and skewed dist)
-test3
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
      lazy val points2 = points1 ++ Map(
         Point( 400, 332 ) -> (),
         Point( 424, 368 ) -> (),
         Point( 200, 312 ) -> (),
         Point( 216, 296 ) -> (),
         Point( 240, 304 ) -> (),
         Point( 272, 496 ) -> (),
         Point( 300, 460 ) -> ()
      )
      lazy val quad0 = Quad( center.x, center.y, extent )

      if( doRun ) EventQueue.invokeLater( this )

      def views : Seq[ JComponent ]
      def doRun = true

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
         val v    = new UncompressedQuadTreeView( t )

         val ct   = CompressedQuadTree.fromMap( Quad( center.x, center.y, extent ), map )
         val cv   = new CompressedQuadTreeView( ct )

         Seq( v, cv )
      }
   }

   class Figure2 extends Figure {
      def views = {
         val map = points2

//         val map2 = Map(
//            Point( 240, 304 ) -> (),
////            Point( 504,  24 ) -> (),
//            Point( 200, 312 ) -> ()
//         )

//         val ct   = CompressedQuadTree.fromMap( Quad( center.x, center.y, extent ), map )
//         val cv   = new CompressedQuadTreeView( ct )
//         Seq( cv )

         val rt   = RandomizedSkipQuadTree( quad0 )( map.toSeq: _* )
//         @tailrec def add( no: Option[ rt.QNode ], vs: List[ JComponent ]) : List[ JComponent ] = {
//            no match {
//               case None => vs
//               case Some( n ) => add( n.prevOption, new SkipQuadTreeView( n ) :: vs )
//            }
//         }
//         add( Some( rt.lastTree ), Nil )
         new SkipQuadTreeView( rt ) :: Nil
      }
   }

   class Test1 extends Figure {
      val map  = points2
      val t    = DeterministicSkipQuadTree( quad0 )( map.toSeq: _* )
      println( "Points ordered by in-ordered traversal:" )
      val ord  = t.toList.map( _._1 )
      println( ord )
      assert( ord == List(Point(488,  8), Point(504, 24), Point(216,296), Point(200,312), Point(240,304),
                          Point( 80,410), Point(400,332), Point(424,368), Point(300,460), Point(272,496)) )

      override val doRun = false
      def views = Nil
   }

   class Test2 extends Figure {
      def views = {
         val map = points2.toIndexedSeq
         val map2 = map ++ IndexedSeq(
            Point(279,  4) -> (),
            Point( 75,361) -> (),
            Point(195,308) -> (),
            Point(170,129) -> (),
            Point(374,425) -> (),
            Point(326,158) -> (),
            Point(320,146) -> (),
            Point( 53,129) -> (),
            Point(481, 90) -> (),
            Point(504,503) -> (),
            Point(123,310) -> (),
            Point( 11,  0) -> (),
            Point(493,288) -> (),
            Point(305,400) -> (),
            Point(210,  7) -> (),
            Point(281, 59) -> (),
            Point( 65,188) -> (),
            Point(140,160) -> (),
            Point(450, 11) -> (),
            Point(397,500) -> ()
         )

         val rnd     = new util.Random( 0 )
//         val set3    = /* map2.map(_._1).toSet ++ */ IndexedSeq.fill( 10000 )( Point( rnd.nextInt( 512 ), rnd.nextInt( 512 ))).toSet
         val set3    =IndexedSeq.fill( 10000 )( Point( (math.log( rnd.nextDouble() * 8886109.0 + 1 ) * 32).toInt,
                                                       (math.log( rnd.nextDouble() * 8886109.0 + 1 ) * 32).toInt )).toSet
         val keys3   = set3.toSeq

         val keys3b  = Seq(
            Point(300,460),
            Point(279,4),
            Point(75,361),
            Point(197,313),
            Point(216,296),
            Point(272,496),
            Point(170,129),
            Point(374,425),
            Point(424,368),
            Point(429,211),
            Point(375,291),
            Point(326,158),
            Point(195,308),
            Point(320,146),
            Point(269,371),
            Point(382,502),
            Point(53,129),
            Point(240,304),
            Point(504,24),
            Point(481,90),
            Point(504,503),
            Point(460,504),
            Point(249,444),
            Point(123,310),
            Point(11,0),
            Point(493,288),
            Point(305,400),
            Point(200,312)
//            Point(210,7)
//            Point(80,410),
//            Point(281,59),
//            Point(507,352),
//            Point(65,188),
//            Point(140,160),
//            Point(450,11)
//            Point(397,500)
//            Point(400,332)
//            Point(488,8)
//            Point(418,6)
         )

         val seq3    = keys3.map( _ -> () )
//println( keys3 )

         val dt   = DeterministicSkipQuadTree( quad0 )( seq3: _* )
//         dt += Point(397,500) -> ()
//         dt += Point(418,6) -> ()
//         @tailrec def add( no: Option[ dt.QNode ], vs: List[ JComponent ]) : List[ JComponent ] = {
//            no match {
//               case None => vs
//               case Some( n ) => add( n.prevOption, new SkipQuadTreeView( n ) :: vs )
//            }
//         }
//         add( Some( dt.lastTree ), Nil )
         val v = new SkipQuadTreeView( dt )
         v.adjustPreferredSize
         v :: Nil
      }
   }

   class Test3 {
      val pts = Seq(
         Point(784870680,892752974),
         Point(651058223,684421757),
         Point(591027245,125634880),
         Point(839166427,357790538),
         Point(413593736,658242131),
         Point(1055294422,944015042),
         Point(1010658620,188953660),
         Point(336012444,138402262),
         Point(157412326,757175630),
         Point(1488941,587057346),
         Point(1035609872,604462622),
         Point(271531193,671245801),
         Point(441089062,16027826),
         Point(1048721317,1063780480),
         Point(523162277,931662865),
         Point(1053281845,787216037)
      )
      val q = Quad( 0x20000000, 0x20000000, 0x20000000 )

      val t = RandomizedSkipQuadTree.empty[ Int ]( q )
      pts.foreach( t.put(_, -1 ))
      pts.foreach { p =>
//         println( "Contains " + p + " ? " )
         t.contains( p )
      }
      t.contains( Point(-998828580,-1206282339 ))
   }
}