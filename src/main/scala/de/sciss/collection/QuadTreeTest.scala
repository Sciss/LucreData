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
 */

package de.sciss.collection

import geom.{Point2DLike, Point2D, Quad2D}
import mutable.{SkipQuadTree, CompressedQuadTree, DeterministicSkipQuadTree, QuadTree, RandomizedSkipQuadTree}
import scala.collection.breakOut
import java.awt.EventQueue
import javax.swing.{BoxLayout, JComponent, WindowConstants, JFrame}
import view.{PDFSupport, SkipQuadTreeView, CompressedQuadTreeView, UncompressedQuadTreeView}

object QuadTreeTest extends App {
   args.headOption match {
      case Some( "-fig1" ) => new Figure1
      case Some( "-fig2" ) => new Figure2
      case Some( "-test1" ) => new Test1
      case Some( "-test2" ) => new Test2
      case Some( "-test3" ) => new Test3
      case Some( "-test4" ) => new Test4
      case Some( "-test5" ) => new Test5
      case _ => println( """
Options:
-fig1
-fig2
-test1
-test2 (somehow a variant of fig. 5 -- only with high n and skewed dist)
-test3
-test4
-test5
""")
         sys.exit( 1 )
   }

   abstract class Figure extends Runnable {
      lazy val center  = Point2D( 256, 256 )
      lazy val extent  = 256
      lazy val points1 = Set(
         Point2D(  80, 410 ),
         Point2D( 488,   8 ),
         Point2D( 504,  24 )
      )
      lazy val points2 = points1 ++ Set(
         Point2D( 400, 332 ),
         Point2D( 424, 368 ),
         Point2D( 200, 312 ),
         Point2D( 216, 296 ),
         Point2D( 240, 304 ),
         Point2D( 272, 496 ),
         Point2D( 300, 460 )
      )
      lazy val quad0 = Quad2D( center.x, center.y, extent )

      if( doRun ) EventQueue.invokeLater( this )

      def views : Seq[ JComponent ]
      def doRun = true

      def run() {
         val f    = new JFrame( "QuadTrees" )
         f.setResizable( false )
         val cp   = f.getContentPane
         val vs   = views
         cp.setLayout( new BoxLayout( cp, BoxLayout.X_AXIS ))
         vs.foreach( cp.add( _ ))
         f.pack()
         f.setLocationRelativeTo( null )
         f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
         PDFSupport.addMenu( f, vs )

         f.setVisible( true )
      }
   }

   class Figure1 extends Figure {
      def views = {
         val map: Map[ Point2DLike, Unit ]  = points1.map( p => p -> () )( breakOut )
         val t    = QuadTree.fromMap( center, extent, map )
         val v    = new UncompressedQuadTreeView( t )

         val ct   = CompressedQuadTree.fromMap( Quad2D( center.x, center.y, extent ), map )
         val cv   = new CompressedQuadTreeView( ct )

         Seq( v, cv )
      }
   }

   class Figure2 extends Figure {
      def views = {
         val map = points2

//         val map2 = Map(
//            Point2D( 240, 304 ) -> (),
////            Point2D( 504,  24 ) -> (),
//            Point2D( 200, 312 ) -> ()
//         )

//         val ct   = CompressedQuadTree.fromMap( Quad2D( center.x, center.y, extent ), map )
//         val cv   = new CompressedQuadTreeView( ct )
//         Seq( cv )

         val rt   = RandomizedSkipQuadTree[ Point2DLike ]( quad0 )( map.toSeq: _* )
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
      val ord  = t.toList
      println( ord )
      assert( ord == List(Point2D(488,  8), Point2D(504, 24), Point2D(216,296), Point2D(200,312), Point2D(240,304),
                          Point2D( 80,410), Point2D(400,332), Point2D(424,368), Point2D(300,460), Point2D(272,496)) )

      override val doRun = false
      def views = Nil
   }

   class Test2 extends Figure {
      def views = {
         val map = points2.toIndexedSeq
         val map2 = map ++ IndexedSeq(
            Point2D(279,  4) -> (),
            Point2D( 75,361) -> (),
            Point2D(195,308) -> (),
            Point2D(170,129) -> (),
            Point2D(374,425) -> (),
            Point2D(326,158) -> (),
            Point2D(320,146) -> (),
            Point2D( 53,129) -> (),
            Point2D(481, 90) -> (),
            Point2D(504,503) -> (),
            Point2D(123,310) -> (),
            Point2D( 11,  0) -> (),
            Point2D(493,288) -> (),
            Point2D(305,400) -> (),
            Point2D(210,  7) -> (),
            Point2D(281, 59) -> (),
            Point2D( 65,188) -> (),
            Point2D(140,160) -> (),
            Point2D(450, 11) -> (),
            Point2D(397,500) -> ()
         )

         val rnd     = new util.Random( 0 )
//         val set3    = /* map2.map(_._1).toSet ++ */ IndexedSeq.fill( 10000 )( Point2D( rnd.nextInt( 512 ), rnd.nextInt( 512 ))).toSet
         val set3    =IndexedSeq.fill( 10000 )( Point2D( (math.log( rnd.nextDouble() * 8886109.0 + 1 ) * 32).toInt,
                                                       (math.log( rnd.nextDouble() * 8886109.0 + 1 ) * 32).toInt )).toSet
         val keys3   = set3.toSeq

         val keys3b  = Seq(
            Point2D(300,460),
            Point2D(279,4),
            Point2D(75,361),
            Point2D(197,313),
            Point2D(216,296),
            Point2D(272,496),
            Point2D(170,129),
            Point2D(374,425),
            Point2D(424,368),
            Point2D(429,211),
            Point2D(375,291),
            Point2D(326,158),
            Point2D(195,308),
            Point2D(320,146),
            Point2D(269,371),
            Point2D(382,502),
            Point2D(53,129),
            Point2D(240,304),
            Point2D(504,24),
            Point2D(481,90),
            Point2D(504,503),
            Point2D(460,504),
            Point2D(249,444),
            Point2D(123,310),
            Point2D(11,0),
            Point2D(493,288),
            Point2D(305,400),
            Point2D(200,312)
//            Point2D(210,7)
//            Point2D(80,410),
//            Point2D(281,59),
//            Point2D(507,352),
//            Point2D(65,188),
//            Point2D(140,160),
//            Point2D(450,11)
//            Point2D(397,500)
//            Point2D(400,332)
//            Point2D(488,8)
//            Point2D(418,6)
         )

         val seq3    = keys3 // .map( _ -> () )
//println( keys3 )

         val dt   = DeterministicSkipQuadTree[ Point2DLike ]( quad0 )( seq3: _* )
//         dt += Point2D(397,500) -> ()
//         dt += Point2D(418,6) -> ()
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
      RandomizedSkipQuadTree.random.setSeed( 0L )

      val rnd   = new util.Random( 2L )
      def randFill( t: SkipQuadTree[ Point2D ]) {
         // seed = 2
         val n     = 0x467 // 0x2F80
         for( i <- 0 until n ) {
            val k = Point2D( rnd.nextInt( 0x40000000 ),
                           rnd.nextInt( 0x40000000 ))
//            val v = rnd.nextInt()
//println( "Putting " + k )
            if( i == 0x466 ) {
               println( "ahora" )
            }
            t += k // .put( k, v )
//            m.put( k, v )
         }
      }

      val q = Quad2D( 0x20000000, 0x20000000, 0x20000000 )
      val t = RandomizedSkipQuadTree.empty[ Point2D ]( q )
      randFill( t )
      t.contains( Point2D(1019607265,594590615))
   }

   class Test4 extends Figure {
      def views = {
         val pts = IndexedSeq(
            Point2D( 987382085, 256957624), Point2D( 942660266,2065821249), Point2D(1880325853, 512536383), Point2D(1855402795,1752363137),
            Point2D(1406072123, 736773014), Point2D(1451067880, 838941424), Point2D(1680383362,1757945958), Point2D( 108485717,1083825684),
            Point2D(1786990885, 734904829), Point2D(1538897317,1621211755), Point2D( 588733455,1321989469), Point2D( 970085865,  18693968),
            Point2D(  84465791,1389304241), Point2D(1320613709,1895906730), Point2D(1302114059,  59120943), Point2D(1875380311, 914610245),
            Point2D(1879245188,1371152848), Point2D(1142307640,2091846700), Point2D( 609767863,2128941925), Point2D(1940326799, 194460002),
            Point2D( 927919418,1747381231), Point2D( 574427432,1129025726), Point2D(1533713858,1650539592), Point2D(1432209827,1870128613),
            Point2D(1715954113,2052824697), Point2D(2147472390,  33434763), Point2D( 679908145, 525880408), Point2D( 897768608,1179676296),
            Point2D(1891988189, 655491140), Point2D(1419511000, 924605541), Point2D(1987386505,1438701325), Point2D(1460526506, 921259113),
            Point2D(1406305473, 500560245), Point2D( 397440262, 739613779), Point2D(1916328766, 832659661), Point2D( 174171991, 695568443),
            Point2D( 916493598,2037206422), Point2D(1596857294,1796978011), Point2D(2090862689, 479694894), Point2D(1814814029, 103971244),
            Point2D( 269088364, 194048757), Point2D( 453358341, 674193810), Point2D( 861681029,1977178734), Point2D(1209860895,2099456790),
            Point2D( 886172024,1836182336), Point2D(2044426000, 611676769), Point2D(1324218866, 431179567), Point2D( 819436644,1871326570),
            Point2D(1164165423,1549070687), Point2D(1519101474,1631642905), Point2D( 454350862,1490823032), Point2D(  82579994, 403368025),
            Point2D(1897823402, 943313041), Point2D( 587700920, 877826491), Point2D(1044506376,2068306920), Point2D(1502239034,1942860484),
            Point2D( 395772794, 759912650), Point2D(1900846418,1118789633), Point2D(1309019334, 145753660), Point2D(1342281372, 769671014),
            Point2D(1206087733, 754443339), Point2D( 103467380,1230646961), Point2D( 316331026, 772981253), Point2D( 544291853,1788225984),
            Point2D( 959845698,1649519236), Point2D( 109289948,1299634466), Point2D( 691481344, 707575970), Point2D( 192305838, 455323462),
            Point2D(  17770465,2079415335), Point2D(1574252549, 505415439), Point2D(2027918755, 112551044), Point2D( 325265040,1448009928),
            Point2D(1482177918, 894223501), Point2D( 663735004,2076816028), Point2D(1779146992,2015410083), Point2D(1961748366, 975296547),
            Point2D( 873793335,1871080525), Point2D( 588898498, 564564135), Point2D(1116774898,1428445500), Point2D( 880792427,1212333608),
            Point2D(1043920811,1466595611), Point2D(1271292099,1735313233), Point2D( 671354160, 426884813), Point2D( 295931180,  54308045),
            Point2D(1910619910, 953698015), Point2D(1235197716, 995794636), Point2D( 787870300, 390685778), Point2D(1657161143,1524021651), // !
            Point2D(1240648010,1618966038), Point2D(2097270995,2003048550), Point2D(2000578622, 955762625), Point2D(1372436909, 148157495),
            Point2D(1142303456, 819620717), Point2D(1825929978, 674142167), Point2D( 975107296, 959859652), Point2D(2066778244,1114072477),
            Point2D( 314287967, 938441883), Point2D(1091268772,  25955015), Point2D( 216743418,1594842550), Point2D(1451598057,1164466938),
            Point2D(  17743516,1037911680), Point2D( 805911685, 582695588), Point2D(1783270816,1116109536), Point2D( 371375701, 125798720),
            Point2D(2051599346, 164489340), Point2D( 960369060,1298871458), Point2D(1496184618, 975302901), Point2D(1419850734, 817845455),
            Point2D(1014858707,1356865809), Point2D( 677224283, 344716307), Point2D( 328018020,2043508172), Point2D( 809205921, 660457817),
            Point2D(1279318928, 975472572), Point2D(2130666640,1027390186), Point2D(1008612087,1755443805), Point2D( 372074621, 657985046),
            Point2D(1016899429, 691560465), Point2D(1960730165,1929168010), Point2D( 701159960,1747983614), Point2D(1892912978, 436167210),
            Point2D( 870300361, 529449494), Point2D( 216935679, 316882643), Point2D(1172320367,1949875935), Point2D( 460771467,1552756052),
            Point2D(1413453556,1079776225), Point2D( 774901347, 642757999), Point2D(1864077899,1082572995), Point2D( 465227848, 388362456),
            Point2D(1429631921, 496439413), Point2D(  98780374,1316772698), Point2D(1409959940,  52714241), Point2D(1406042559,1504257198),
            Point2D(1380494950,1792923609), Point2D( 345623365,1843388081), Point2D( 451894448,2124809402), Point2D( 251698901,1712213228),
            Point2D( 866700129,2122630266), Point2D(1680043987,  77378236), Point2D(1489859849,1375927900), Point2D(1434012920,1667093362),
            Point2D(1598649701,1592263107), Point2D(1483125202, 546831422), Point2D(2089333406, 481071960), Point2D(2027252544, 231633364),
            Point2D(1133586293, 730341371), Point2D( 207617796,1115327484), Point2D( 714534714,1300918048), Point2D(1668761030, 504772493),
            Point2D( 575892704,1808081247), Point2D(  98380819,1829933964), Point2D( 430103698,2104729956), Point2D( 886286916,1586526305),
            Point2D( 288351118, 426949761), Point2D(1768178767, 646774880), Point2D(1696650291, 552152461), Point2D(1863424494,1356345107),
            Point2D(1581214857,1094963167), Point2D(  55184551,1493805330), Point2D(1616536491, 612385146), Point2D(2067666064, 102801481),
            Point2D(1263285967,1869234887), Point2D( 378394090,1013299442), Point2D( 656924918,1120890746), Point2D( 442284265,1345079740),
            Point2D(1760661591, 822333291), Point2D( 752712255, 872154574), Point2D( 833458987,  85165962), Point2D(1073821254, 881093029),
            Point2D(1009559906, 339964480), Point2D( 946708962,1791704065), Point2D(1379679079,2019421136), Point2D( 270322531,1472309860),
            Point2D(1717548548,1459255964), Point2D(1868016416,2100169919), Point2D(1344997882, 456621144), Point2D(1250315903,1066450545),
            Point2D(1338841587,1853061263), Point2D(1949290902,1532611232), Point2D(1684229073,1786279438), Point2D(1287289534,1877211272),
            Point2D( 291539599, 297567234), Point2D(1338208422,1634476139), Point2D( 480533540, 585556219), Point2D( 849706692, 917842961),
            Point2D( 846746417, 969251087), Point2D(2141748538, 858836516), Point2D( 226535009,1195010500), Point2D( 859179150,1780048334),
            Point2D( 344532921, 643499424), Point2D( 331125438,1899272280), Point2D(  16434264, 447595105), Point2D( 172313236,1920085192),
            Point2D(1462448466,1698899019), Point2D(  86191959,1014158760), Point2D(1153172648,2123032090), Point2D(   4634141, 108964731),
            Point2D(  94323081,1457643629), Point2D( 989854569,  70207124), Point2D(2002511209,1348525255), Point2D( 242575075,1563459905),
            Point2D(1365727118,1929101189), Point2D( 257112136,1084105610), Point2D(1549422363,1980684617), Point2D(1561623527,1907839997),
            Point2D(2120606665, 365259731), Point2D( 809742909,1796771119), Point2D(1115986895,2143932227), Point2D(1175640470,2104465928),
            Point2D(1650161057,1385954353) // , Point2D(1661129149,1275316973) // , Point2D(1690285716, 368499742) // , Point2D(2050625821,  25674849)
//         Point2D( 231391113,1540191537), Point2D(  76851174, 361104318), Point2D( 702537509, 208019261), Point2D(2000906506, 729878800),
//         Point2D(1703353676, 654327672), Point2D( 358552122,1659067704), Point2D( 428924369,1298587004), Point2D( 989676324,1685033435),
//         Point2D(1421478041,2034995035), Point2D( 767054239, 757771480), Point2D(1654109098, 412552175), Point2D(1482054540,1464283949)
         )
         val q = Quad2D( 0x40000000, 0x40000000, 0x40000000 )
         RandomizedSkipQuadTree.random.setSeed( 0L )
         val t = RandomizedSkipQuadTree[ Point2DLike ]( q )( pts: _* )
         // query Point2D(1609162490,1507881173), wrong result Point2D(1598649701,1592263107), correct result Point2D(1657161143,1524021651)
         // query Point2D( 310852551,1213007527), wrong result Point2D( 257112136,1084105610), correct result Point2D( 226535009,1195010500)
         val q1   = Point2D(1609162490,1507881173)
         val res1 = t.nearestNeighbor( q1 ) // .get
         println( q1 -> res1 )
//      val q2   = Point2D( 310852551,1213007527)
//      val res2 = t.nearestNeighbor( q2 ).get
//      println( q2 -> res2 )

         val v = new SkipQuadTreeView( t )
            v.highlight = Set( /* Point2D(1609162490,1507881173), */ Point2D(1598649701,1592263107), Point2D(1657161143,1524021651) )
         v.scale = 256.0 / 0x40000000
//         v.gridColor = new Color( 0x00, 0x00, 0x00, 0x30 )
         v.adjustPreferredSize
         v :: Nil
      }
   }

   class Test5 {
      val pts = IndexedSeq(
         Point2D( 991999072,1423528248),
         Point2D( 456749246, 590203382),
         Point2D( 216625335, 502539523),
         Point2D(1209182061,1431162155),
         Point2D(1654374947, 485484877),
         Point2D(2073694040,1628576520),
         Point2D(1895150834, 755814641),
         Point2D(1344049776, 553609048),
         Point2D( 629649304, 881218872),
         Point2D(  5955764,  200745736)
      )
      val query   = Point2D(1599145891,-1341955486)
      val quad    = Quad2D( 0x40000000, 0x40000000, 0x40000000 )
      val t       = RandomizedSkipQuadTree[ Point2D ]( quad )( pts: _* )
      val res     = t.nearestNeighbor( query )
      println( res + " - " + res.distanceSq( query ))
      val correct = pts.minBy( _.distanceSq( query ))
      println( correct + " - " + correct.distanceSq( query ))
   }
}