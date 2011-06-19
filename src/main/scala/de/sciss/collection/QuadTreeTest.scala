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
import scala.collection.mutable.{Map => MMap}

object QuadTreeTest extends App {
   args.headOption match {
      case Some( "-fig1" ) => new Figure1
      case Some( "-fig2" ) => new Figure2
      case Some( "-test1" ) => new Test1
      case Some( "-test2" ) => new Test2
      case Some( "-test3" ) => new Test3
      case Some( "-test4" ) => new Test4
      case _ => println( """
Options:
-fig1
-fig2
-test1
-test2 (somehow a variant of fig. 5 -- only with high n and skewed dist)
-test3
-test4
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
      RandomizedSkipQuadTree.random.setSeed( 0L )

      val rnd   = new util.Random( 2L )
      def randFill( t: SkipQuadTree[ Int ]) {
         // seed = 2
         val n     = 0x467 // 0x2F80
         for( i <- 0 until n ) {
            val k = Point( rnd.nextInt( 0x40000000 ),
                           rnd.nextInt( 0x40000000 ))
            val v = rnd.nextInt()
//println( "Putting " + k )
            if( i == 0x466 ) {
               println( "ahora" )
            }
            t.put( k, v )
//            m.put( k, v )
         }
      }

      val q = Quad( 0x20000000, 0x20000000, 0x20000000 )
      val t = RandomizedSkipQuadTree.empty[ Int ]( q )
      randFill( t )
      t.contains( Point(1019607265,594590615))
   }

   class Test4 {
      val pts = IndexedSeq(
         Point( 987382085, 256957624), Point( 942660266,2065821249), Point(1880325853, 512536383), Point(1855402795,1752363137),
         Point(1406072123, 736773014), Point(1451067880, 838941424), Point(1680383362,1757945958), Point( 108485717,1083825684),
         Point(1786990885, 734904829), Point(1538897317,1621211755), Point( 588733455,1321989469), Point( 970085865,  18693968),
         Point(  84465791,1389304241), Point(1320613709,1895906730), Point(1302114059,  59120943), Point(1875380311, 914610245),
         Point(1879245188,1371152848), Point(1142307640,2091846700), Point( 609767863,2128941925), Point(1940326799, 194460002),
         Point( 927919418,1747381231), Point( 574427432,1129025726), Point(1533713858,1650539592), Point(1432209827,1870128613),
         Point(1715954113,2052824697), Point(2147472390,  33434763), Point( 679908145, 525880408), Point( 897768608,1179676296),
         Point(1891988189, 655491140), Point(1419511000, 924605541), Point(1987386505,1438701325), Point(1460526506, 921259113),
         Point(1406305473, 500560245), Point( 397440262, 739613779), Point(1916328766, 832659661), Point( 174171991, 695568443),
         Point( 916493598,2037206422), Point(1596857294,1796978011), Point(2090862689, 479694894), Point(1814814029, 103971244),
         Point( 269088364, 194048757), Point( 453358341, 674193810), Point( 861681029,1977178734), Point(1209860895,2099456790),
         Point( 886172024,1836182336), Point(2044426000, 611676769), Point(1324218866, 431179567), Point( 819436644,1871326570),
         Point(1164165423,1549070687), Point(1519101474,1631642905), Point( 454350862,1490823032), Point(  82579994, 403368025),
         Point(1897823402, 943313041), Point( 587700920, 877826491), Point(1044506376,2068306920), Point(1502239034,1942860484),
         Point( 395772794, 759912650), Point(1900846418,1118789633), Point(1309019334, 145753660), Point(1342281372, 769671014),
         Point(1206087733, 754443339), Point( 103467380,1230646961), Point( 316331026, 772981253), Point( 544291853,1788225984),
         Point( 959845698,1649519236), Point( 109289948,1299634466), Point( 691481344, 707575970), Point( 192305838, 455323462),
         Point(  17770465,2079415335), Point(1574252549, 505415439), Point(2027918755, 112551044), Point( 325265040,1448009928),
         Point(1482177918, 894223501), Point( 663735004,2076816028), Point(1779146992,2015410083), Point(1961748366, 975296547),
         Point( 873793335,1871080525), Point( 588898498, 564564135), Point(1116774898,1428445500), Point( 880792427,1212333608),
         Point(1043920811,1466595611), Point(1271292099,1735313233), Point( 671354160, 426884813), Point( 295931180,  54308045),
         Point(1910619910, 953698015), Point(1235197716, 995794636), Point( 787870300, 390685778), Point(1657161143,1524021651),
         Point(1240648010,1618966038), Point(2097270995,2003048550), Point(2000578622, 955762625), Point(1372436909, 148157495),
         Point(1142303456, 819620717), Point(1825929978, 674142167), Point( 975107296, 959859652), Point(2066778244,1114072477),
         Point( 314287967, 938441883), Point(1091268772,  25955015), Point( 216743418,1594842550), Point(1451598057,1164466938),
         Point(  17743516,1037911680), Point( 805911685, 582695588), Point(1783270816,1116109536), Point( 371375701, 125798720),
         Point(2051599346, 164489340), Point( 960369060,1298871458), Point(1496184618, 975302901), Point(1419850734, 817845455),
         Point(1014858707,1356865809), Point( 677224283, 344716307), Point( 328018020,2043508172), Point( 809205921, 660457817),
         Point(1279318928, 975472572), Point(2130666640,1027390186), Point(1008612087,1755443805), Point( 372074621, 657985046),
         Point(1016899429, 691560465), Point(1960730165,1929168010), Point( 701159960,1747983614), Point(1892912978, 436167210),
         Point( 870300361, 529449494), Point( 216935679, 316882643), Point(1172320367,1949875935), Point( 460771467,1552756052),
         Point(1413453556,1079776225), Point( 774901347, 642757999), Point(1864077899,1082572995), Point( 465227848, 388362456),
         Point(1429631921, 496439413), Point(  98780374,1316772698), Point(1409959940,  52714241), Point(1406042559,1504257198),
         Point(1380494950,1792923609), Point( 345623365,1843388081), Point( 451894448,2124809402), Point( 251698901,1712213228),
         Point( 866700129,2122630266), Point(1680043987,  77378236), Point(1489859849,1375927900), Point(1434012920,1667093362),
         Point(1598649701,1592263107), Point(1483125202, 546831422), Point(2089333406, 481071960), Point(2027252544, 231633364),
         Point(1133586293, 730341371), Point( 207617796,1115327484), Point( 714534714,1300918048), Point(1668761030, 504772493),
         Point( 575892704,1808081247), Point(  98380819,1829933964), Point( 430103698,2104729956), Point( 886286916,1586526305),
         Point( 288351118, 426949761), Point(1768178767, 646774880), Point(1696650291, 552152461), Point(1863424494,1356345107),
         Point(1581214857,1094963167), Point(  55184551,1493805330), Point(1616536491, 612385146), Point(2067666064, 102801481),
         Point(1263285967,1869234887), Point( 378394090,1013299442), Point( 656924918,1120890746), Point( 442284265,1345079740),
         Point(1760661591, 822333291), Point( 752712255, 872154574), Point( 833458987,  85165962), Point(1073821254, 881093029),
         Point(1009559906, 339964480), Point( 946708962,1791704065), Point(1379679079,2019421136), Point( 270322531,1472309860),
         Point(1717548548,1459255964), Point(1868016416,2100169919), Point(1344997882, 456621144), Point(1250315903,1066450545),
         Point(1338841587,1853061263), Point(1949290902,1532611232), Point(1684229073,1786279438), Point(1287289534,1877211272),
         Point( 291539599, 297567234), Point(1338208422,1634476139), Point( 480533540, 585556219), Point( 849706692, 917842961),
         Point( 846746417, 969251087), Point(2141748538, 858836516), Point( 226535009,1195010500), Point( 859179150,1780048334),
         Point( 344532921, 643499424), Point( 331125438,1899272280), Point(  16434264, 447595105), Point( 172313236,1920085192),
         Point(1462448466,1698899019), Point(  86191959,1014158760), Point(1153172648,2123032090), Point(   4634141, 108964731),
         Point(  94323081,1457643629), Point( 989854569,  70207124), Point(2002511209,1348525255), Point( 242575075,1563459905),
         Point(1365727118,1929101189), Point( 257112136,1084105610), Point(1549422363,1980684617), Point(1561623527,1907839997),
         Point(2120606665, 365259731), Point( 809742909,1796771119), Point(1115986895,2143932227), Point(1175640470,2104465928),
         Point(1650161057,1385954353), Point(1661129149,1275316973), Point(1690285716, 368499742), Point(2050625821,  25674849),
         Point( 231391113,1540191537), Point(  76851174, 361104318), Point( 702537509, 208019261), Point(2000906506, 729878800),
         Point(1703353676, 654327672), Point( 358552122,1659067704), Point( 428924369,1298587004), Point( 989676324,1685033435),
         Point(1421478041,2034995035), Point( 767054239, 757771480), Point(1654109098, 412552175), Point(1482054540,1464283949)
      )
//      val pts = IndexedSeq(
//         Point( 575892704,1808081247), Point( 430103698,2104729956), Point(1596857294,1796978011),
//         Point( 295931180,  54308045), Point(1880325853, 512536383), Point(1598649701,1592263107),
//         Point(1768178767, 646774880), Point(1910619910, 953698015), Point( 574427432,1129025726),
//         Point(1496184618, 975302901), Point(1164165423,1549070687), Point(1073821254, 881093029),
//         Point(1432209827,1870128613), Point( 216935679, 316882643), Point(1451067880, 838941424),
//         Point(  86191959,1014158760), Point(1825929978, 674142167), Point(1142307640,2091846700),
//         Point( 752712255, 872154574), Point( 397440262, 739613779), Point(2097270995,2003048550),
//         Point(1680383362,1757945958), Point( 480533540, 585556219), Point( 251698901,1712213228),
//         Point( 358552122,1659067704), Point(2089333406, 481071960), Point(1482054540,1464283949),
//         Point( 331125438,1899272280), Point(1875380311, 914610245), Point( 663735004,2076816028),
//         Point(1680043987,  77378236), Point( 774901347, 642757999), Point( 849706692, 917842961),
//         Point(1133586293, 730341371), Point(1372436909, 148157495), Point(1309019334, 145753660),
//         Point(1344997882, 456621144), Point(1657161143,1524021651), Point( 714534714,1300918048),
//         Point(1175640470,2104465928), Point(  98780374,1316772698), Point( 442284265,1345079740),
//         Point( 345623365,1843388081), Point( 588898498, 564564135), Point(2027918755, 112551044),
//         Point( 174171991, 695568443), Point(1406042559,1504257198), Point(1406072123, 736773014),
//         Point(2066778244,1114072477), Point(1153172648,2123032090), Point( 679908145, 525880408),
//         Point( 291539599, 297567234), Point( 927919418,1747381231), Point(1533713858,1650539592),
//         Point(1717548548,1459255964), Point(1482177918, 894223501), Point(1419511000, 924605541),
//         Point(1380494950,1792923609), Point( 859179150,1780048334), Point( 314287967, 938441883),
//         Point(1008612087,1755443805), Point(1434012920,1667093362), Point(2002511209,1348525255),
//         Point(1044506376,2068306920), Point(1661129149,1275316973), Point( 451894448,2124809402),
//         Point(2051599346, 164489340), Point(1240648010,1618966038), Point(1779146992,2015410083),
//         Point(1519101474,1631642905), Point( 975107296, 959859652), Point( 242575075,1563459905),
//         Point( 460771467,1552756052), Point(1429631921, 496439413), Point(1287289534,1877211272),
//         Point(1421478041,2034995035), Point( 880792427,1212333608), Point(1574252549, 505415439),
//         Point(1462448466,1698899019), Point(2067666064, 102801481), Point(1786990885, 734904829),
//         Point(1043920811,1466595611), Point( 588733455,1321989469), Point(1703353676, 654327672),
//         Point( 873793335,1871080525), Point( 103467380,1230646961), Point(1987386505,1438701325),
//         Point(1271292099,1735313233), Point(2120606665, 365259731), Point(1616536491, 612385146),
//         Point(1172320367,1949875935), Point( 270322531,1472309860), Point( 172313236,1920085192),
//         Point(1250315903,1066450545), Point(1091268772,  25955015), Point(1960730165,1929168010),
//         Point(1413453556,1079776225), Point( 767054239, 757771480), Point(   4634141, 108964731),
//         Point(1891988189, 655491140), Point( 809205921, 660457817), Point( 453358341, 674193810),
//         Point( 677224283, 344716307), Point(1654109098, 412552175), Point(1338208422,1634476139),
//         Point(1897823402, 943313041), Point( 886172024,1836182336), Point(1409959940,  52714241),
//         Point(2141748538, 858836516), Point( 970085865,  18693968), Point(1900846418,1118789633),
//         Point(1016899429, 691560465), Point( 861681029,1977178734), Point( 989854569,  70207124),
//         Point( 257112136,1084105610), Point(1783270816,1116109536), Point(1650161057,1385954353),
//         Point(1451598057,1164466938), Point(1235197716, 995794636), Point( 866700129,2122630266),
//         Point(1961748366, 975296547), Point( 897768608,1179676296), Point(1406305473, 500560245),
//         Point(1561623527,1907839997), Point( 269088364, 194048757), Point(1263285967,1869234887),
//         Point(1684229073,1786279438), Point(1949290902,1532611232), Point(1279318928, 975472572),
//         Point(1879245188,1371152848), Point(  84465791,1389304241), Point(1864077899,1082572995),
//         Point(1715954113,2052824697), Point( 702537509, 208019261), Point(1209860895,2099456790),
//         Point(1892912978, 436167210), Point( 428924369,1298587004), Point(1342281372, 769671014),
//         Point( 805911685, 582695588), Point(1696650291, 552152461), Point( 192305838, 455323462),
//         Point( 371375701, 125798720), Point(  17770465,2079415335), Point( 316331026, 772981253),
//         Point(1855402795,1752363137), Point( 846746417, 969251087), Point(1863424494,1356345107),
//         Point( 454350862,1490823032), Point( 987382085, 256957624), Point( 656924918,1120890746),
//         Point(1365727118,1929101189), Point(2090862689, 479694894), Point(  82579994, 403368025),
//         Point( 544291853,1788225984), Point( 231391113,1540191537), Point(1009559906, 339964480),
//         Point( 701159960,1747983614), Point(  16434264, 447595105), Point(  94323081,1457643629),
//         Point(1483125202, 546831422), Point(2147472390,  33434763), Point( 288351118, 426949761),
//         Point(1668761030, 504772493), Point(1760661591, 822333291), Point( 109289948,1299634466),
//         Point(1868016416,2100169919), Point( 942660266,2065821249), Point( 833458987,  85165962),
//         Point( 886286916,1586526305), Point(1549422363,1980684617), Point( 344532921, 643499424),
//         Point(1206087733, 754443339), Point( 691481344, 707575970), Point(1538897317,1621211755),
//         Point(1142303456, 819620717), Point( 787870300, 390685778), Point( 587700920, 877826491),
//         Point( 671354160, 426884813), Point( 870300361, 529449494), Point( 916493598,2037206422),
//         Point(1324218866, 431179567), Point( 819436644,1871326570), Point(1419850734, 817845455),
//         Point(1581214857,1094963167), Point(2000578622, 955762625), Point( 378394090,1013299442),
//         Point( 328018020,2043508172), Point( 207617796,1115327484), Point(1690285716, 368499742),
//         Point(  55184551,1493805330), Point(1116774898,1428445500), Point( 325265040,1448009928),
//         Point( 989676324,1685033435), Point(1379679079,2019421136), Point( 959845698,1649519236),
//         Point(2044426000, 611676769), Point(1115986895,2143932227), Point(1940326799, 194460002),
//         Point( 809742909,1796771119), Point(1014858707,1356865809), Point(1916328766, 832659661),
//         Point( 216743418,1594842550), Point(1460526506, 921259113), Point(  17743516,1037911680),
//         Point(2130666640,1027390186), Point( 946708962,1791704065), Point( 960369060,1298871458),
//         Point( 372074621, 657985046), Point(1814814029, 103971244), Point(1320613709,1895906730),
//         Point(1338841587,1853061263), Point( 395772794, 759912650), Point( 609767863,2128941925),
//         Point(  76851174, 361104318), Point(2050625821,  25674849), Point(  98380819,1829933964),
//         Point( 226535009,1195010500), Point(2027252544, 231633364), Point(1502239034,1942860484),
//         Point(1302114059,  59120943), Point(1489859849,1375927900), Point( 108485717,1083825684),
//         Point( 465227848, 388362456), Point(2000906506, 729878800)
//      )
      val q = Quad( 0x40000000, 0x40000000, 0x40000000 )
      RandomizedSkipQuadTree.random.setSeed( 0L )
      val t = RandomizedSkipQuadTree[ Int ]( q )( pts.map( p => p -> 0 ): _* )
      // query Point(1609162490,1507881173), wrong result Point(1598649701,1592263107), correct result Point(1657161143,1524021651)
      // query Point( 310852551,1213007527), wrong result Point( 257112136,1084105610), correct result Point( 226535009,1195010500)
      val q1   = Point(1609162490,1507881173)
      val res1 = t.nearestNeighbor( q1 ).get
      println( q1 -> res1 )
      val q2   = Point( 310852551,1213007527)
      val res2 = t.nearestNeighbor( q2 ).get
      println( q2 -> res2 )
   }
}