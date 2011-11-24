/*
 *  InteractiveTxnSkipOctreePanel.scala
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
package view

import java.awt.{Insets, Color, FlowLayout, EventQueue, BorderLayout}
import javax.swing.{JComponent, JLabel, SwingConstants, Box, WindowConstants, JComboBox, AbstractButton, JTextField, JButton, JFrame, JPanel}
import geom.{QueryShape, SquareLike, DistanceMeasure2D, Space, DistanceMeasure, Point2D, Square}
import java.awt.event.{MouseListener, MouseMotionListener, ActionListener, MouseEvent, MouseAdapter, ActionEvent}
import Space.TwoDim
import de.sciss.lucrestm.{Serializer, InMemory, Sys}

object InteractiveTxnSkipOctreePanel extends App with Runnable {
   val seed = 0L

   EventQueue.invokeLater( this )
   def run() {
//      val xs = args.toSeq
      implicit val system  = new InMemory
      val model = system.atomic { implicit tx =>
//         if( xs.contains( "--3d" )) {
//            val tree = txn.DeterministicSkipOctree.empty[ InMemory, Space.ThreeDim, Point3DLike ](
//               Space.ThreeDim, Cube( sz, sz, sz, sz ), skipGap = 1 )
//            new Model3D[ InMemory ]( tree )
//         } else {
         import txn.geom.Space.{Point2DSerializer, SquareSerializer}

            val tree = txn.DeterministicSkipOctree.empty[ InMemory, TwoDim, Point2D ](
               TwoDim, Square( sz, sz, sz ), skipGap = 1 )
            new Model2D[ InMemory ]( tree )
//         }
      }

      val f    = new JFrame( "Skip Octree" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = model.newPanel()
      cp.add( iv, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      model.addPDFSupport( f )
      f.setVisible( true )
   }

   private val sz = 256

   private final class Model2D[ S <: Sys[ S ]]( val tree: txn.DeterministicSkipOctree[ S, TwoDim, Point2D ])
   extends Model[ S, TwoDim, Point2D ] {
//      val tree = DeterministicSkipOctree.empty[ S, Space.TwoDim, TwoDim#Point ]( Space.TwoDim, Square( sz, sz, sz ), skipGap = 1 )

      def queryShape( sq: Square ) = sq
      def point( coords: IndexedSeq[ Int ]) = coords match {
         case IndexedSeq( x, y ) => Point2D( x, y )
      }
      def coords( p: TwoDim#PointLike ) : IndexedSeq[ Int ] = IndexedSeq( p.x, p.y )
      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) = coords match {
         case IndexedSeq( x, y ) => Square( x, y, ext )
      }

      val view = {
         val res = new TxnSkipQuadtreeView[ S, Point2D ]( tree )
         res.topPainter = Some( topPaint _ )
         res
      }
      def repaint() { view.repaint() }
//      val baseDistance = DistanceMeasure2D.euclideanSq

      def highlight: Set[ Point2D ] = view.highlight
      def highlight_=( points: Set[ Point2D ]) { view highlight = points }

      val distanceMeasures = IndexedSeq(
         "Euclidean" -> DistanceMeasure2D.euclideanSq,
         "Maximum" -> DistanceMeasure2D.chebyshev,
         "Minimum" -> DistanceMeasure2D.vehsybehc
      )

      var rangeHyperCube = Option.empty[ Square ]

      private val colrTrns = new Color( 0x00, 0x00, 0xFF, 0x40 )
      private def topPaint( h: QuadView.PaintHelper ) {
         rangeHyperCube.foreach { q =>
            h.g2.setColor( Color.blue )
            val side = q.extent << 1
            h.g2.drawRect( q.left, q.top, side, side )
            h.g2.setColor( colrTrns )
            h.g2.fillRect( q.left, q.top, side, side )
         }
      }

      def addPDFSupport( f: JFrame ) {
         PDFSupport.addMenu[ TxnSkipQuadtreeView[ S, Point2D ]]( f, view :: Nil, _.adjustPreferredSize() )
      }
   }

//   private final class Model3D[ S <: Sys[ S ]]( tree: txn.SkipOctree[ S, Space.ThreeDim, Point3DLike ])
//   extends Model[ S, Space.ThreeDim ] {
//
//      def queryShape( c: CubeLike ) = c
//      def point( coords: IndexedSeq[ Int ]) = coords match {
//         case IndexedSeq( x, y, z ) => Point3D( x, y, z )
//      }
//      def coords( p: Point3DLike ) : IndexedSeq[ Int ] = IndexedSeq( p.x, p.y, p.z )
//      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) = coords match {
//         case IndexedSeq( x, y, z ) => Cube( x, y, z, ext )
//      }
//
//      val view = new SkipOctree3DView( tree )
//      def repaint() { view.treeUpdated() }
////      val baseDistance = DistanceMeasure3D.euclideanSq
//      def highlight: Set[ Point3DLike ] = view.highlight
//      def highlight_=( points: Set[ Point3DLike ]) { view.highlight = points }
//
//      val distanceMeasures = IndexedSeq(
//         "Euclidean" -> DistanceMeasure3D.euclideanSq,
//         "MaximumXY" -> DistanceMeasure3D.chebyshevXY,
//         "MinimumXY" -> DistanceMeasure3D.vehsybehcXY
//      )
//
//      var rangeHyperCube = Option.empty[ CubeLike ]
//
//      def addPDFSupport( f: JFrame ) {
//         PDFSupport.addMenu[ JComponent ]( f, view :: Nil, _ => () )
//      }
//   }

   trait Model[ S <: Sys[ S ], D <: Space[ D ], Point <: D#PointLike ] {
      def tree: txn.SkipOctree[ S, D, Point ]
      def view: JComponent
      final def insets: Insets = view.getInsets
      def point( coords: IndexedSeq[ Int ]) : Point
      def coords( p: D#PointLike ) : IndexedSeq[ Int ]
      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) : D#HyperCube
//      def baseDistance: DistanceMeasure[ _, D ]
      def distanceMeasures: IndexedSeq[ (String, DistanceMeasure[ _, D ])]
      def highlight: Set[ Point ]
      def highlight_=( points: Set[ Point ]) : Unit
      final def pointString( p: D#PointLike ) : String = coords( p ).mkString( "(", "," , ")" )
      final def newPanel() : InteractiveTxnSkipOctreePanel[ S, D, Point ] = new InteractiveTxnSkipOctreePanel( this )
      def queryShape( q: D#HyperCube ) : QueryShape[ _, D ]
      def repaint() : Unit
      def rangeHyperCube : Option[ D#HyperCube ]
      def rangeHyperCube_=( q: Option[ D#HyperCube ]) : Unit

      final def addMouseAdapter( ma: MouseListener with MouseMotionListener ) {
         view.addMouseListener( ma )
         view.addMouseMotionListener( ma )
      }

      def addPDFSupport( f: JFrame ) : Unit
   }
}
class InteractiveTxnSkipOctreePanel[ S <: Sys[ S ], D <: Space[ D ], Point <: D#PointLike ](
   val model: InteractiveTxnSkipOctreePanel.Model[ S, D, Point ])
extends JPanel( new BorderLayout() ) {
   import InteractiveTxnSkipOctreePanel._

   val t = model.tree
   private val rnd = new util.Random( seed )

   private val in = model.insets

   private var distFilter : DistanceMeasure[ _, D ] => DistanceMeasure[ _, D ] = identity
   private var baseDistance = model.distanceMeasures( 0 )._2
   private var distMeasure : DistanceMeasure[ _, D ] = baseDistance

   def recalcDistMeasure() { distMeasure = distFilter( baseDistance )}

   private val ggCoord  = IndexedSeq.fill( t.space.dim )( new JTextField( 3 ))

   private val ggExt = new JTextField( 3 )

   private def updateNum( coords: Seq[ Int ]) {
      coords.zip( ggCoord ).foreach {
         case (value, gg) => gg.setText( value.toString )
      }
   }

   private def tryPoint( fun: Point => Unit ) {
      try {
//         val p = Point2D( ggX.getText.toInt, ggY.getText.toInt )
         val p = model.point( ggCoord.map( _.getText.toInt ))
         fun( p )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private def tryHyperCube( fun: D#HyperCube => Unit ) {
      try {
         val ext = ggExt.getText.toInt
         require( ext > 0 )
//         val q = Square( ggX.getText.toInt, ggY.getText.toInt, ext )
         val q = model.hyperCube( ggCoord.map( _.getText.toInt ), ext )
         fun( q )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private val p = new JPanel( new FlowLayout() )
   private def but( lb: String )( action: => Unit ) : AbstractButton = {
      val b = new JButton( lb )
      b.putClientProperty( "JButton.buttonType", "bevel" )
      b.putClientProperty( "JComponent.sizeVariant", "mini" )
      b.setFocusable( false )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            action
            model.repaint()
         }
      })
      p.add( b )
      b
   }
   private def combo( items: String* )( action: Int => Unit ) : JComboBox = {
      val b = new JComboBox( items.toArray[ AnyRef ])
      b.putClientProperty( "JComboBox.isSquare", java.lang.Boolean.TRUE )
      b.putClientProperty( "JComponent.sizeVariant", "mini" )
      b.setFocusable( false )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            action( b.getSelectedIndex )
         }
      })
      p.add( b )
      b
   }
   private def space() {
      p.add( Box.createHorizontalStrut( 8 ))
   }
   private def label( text: String ) {
      val l = new JLabel( text, SwingConstants.RIGHT )
      l.putClientProperty( "JComponent.sizeVariant", "mini" )
      p.add( l )
   }

   but( "Help" ) {
      println( """
- Number fields are cx, cy, and extent (for range queries)
- Mouse:
     Double-click to insert point
     Alt-click to remove point
     Shift-drag for range query
     Ctrl-click for NN
""" )
   }

   but( "Dump" ) {
      println( atomic { implicit tx => t.toList })
   }

   ggCoord.foreach( p.add )
   p.add( ggExt )

   private val ggAdd = but( "Add" ) { tryPoint { p =>
      atomic { implicit tx => t += p }
      model.highlight = Set( p )
   }}
   private val ggRemove = but( "Remove" ) { tryPoint( p => atomic( implicit tx => t -= p ))}
   but( "Contains" ) { tryPoint { p =>
      status( atomic { implicit tx => t.contains( p )}.toString )
      model.highlight = Set( p )
   }}

   private def rangeString( pt: Set[ Point ]) : String = {
      val s = pt.map( model.pointString ).mkString( " " )
      if( s.isEmpty ) "(empty)" else s
   }

   private def atomic[ Z ]( block: S#Tx => Z ) : Z = t.system.atomic( block )

   def findNN() { tryPoint { p =>
      val set = atomic { implicit tx => t.nearestNeighborOption( p, metric = distMeasure ).map( Set( _ )).getOrElse( Set.empty )}
      model.highlight = set
      status( rangeString( set ))
   }}

   combo( model.distanceMeasures.map( _._1 ): _* ) { i =>
      baseDistance = model.distanceMeasures( i )._2
      recalcDistMeasure()
   }

   combo( ("All Orthants" +: Seq.tabulate( t.numOrthants )( i => (i + 1).toString )): _* ) { i =>
      if( i > 0 ) {
         distFilter = _.orthant( i - 1 )
      } else {
         distFilter = identity
      }
      recalcDistMeasure()
   }

   but( "NN" )( findNN() )

   but( "Range" ) { tryHyperCube { q =>
      val set = atomic { implicit tx => t.rangeQuery( model.queryShape( q )).toSet }
      status( rangeString( set.take( 3 )))
      println( rangeString( set ))
   }}

   space()
   label( "Randomly:" )

   private def addPoints( num: Int ) {
      val ps = Seq.fill( num )( model.point( IndexedSeq.fill( t.space.dim )( rnd.nextInt( 512 ))))
      atomic { implicit tx => ps.foreach( t += _ )}
      model.highlight = ps.toSet
   }

   but( "Add 1x" )  { addPoints(  1 )}
   but( "Add 10x" ) { addPoints( 10 )}

   space()
   label( "In order:" )

   private def removePoints( num: Int ) {
      atomic { implicit tx =>
         val ps = t.iterator.take( num )
         ps.foreach( t -= _ )
      }
   }

   but( "Remove 1x" )  {
//break
      removePoints(  1 )
   }
   but( "Remove 10x" ) { removePoints( 10 )}

   space()

//   but( "Consistency" ) {
//      verifyConsistency()
//   }

   private val ma = new MouseAdapter {
      var drag = Option.empty[ (MouseEvent, Option[ MouseEvent ])]

      override def mouseDragged( e: MouseEvent ) {
         drag match {
            case Some( (m1, None) ) =>
               val dist = e.getPoint.distance( m1.getPoint )
               if( dist > 4 ) {
                  drag( m1, e )
               }
            case Some( (m1, Some( _ ))) => drag( m1, e )
            case _ =>
         }
      }

      def drag( m1: MouseEvent, m2: MouseEvent ) {
         drag = Some( m1 -> Some( m2 ))
         val ext = math.max( math.abs( m1.getPoint.x - m2.getPoint.x ),
                             math.abs( m1.getPoint.y - m2.getPoint.y ))
         ggExt.setText( ext.toString )
         tryHyperCube { q =>
            model.rangeHyperCube = Some( q )
            val set = atomic { implicit tx => t.rangeQuery( model.queryShape( q )).toSet }
            model.highlight = set
            model.repaint()
            status( rangeString( set.take( 3 )))
         }
      }

      override def mouseReleased( e: MouseEvent ) {
         drag match {
            case Some( (_, Some( _ ))) =>
               model.rangeHyperCube = None
               model.repaint()
            case _ =>
         }
         drag = None
      }

      override def mousePressed( e: MouseEvent ) {
         val x = e.getX - in.left
         val y = e.getY - in.top
         updateNum( Seq( x, y ))
         if( e.isControlDown ) {
            findNN()
            model.repaint()
         } else if( e.isAltDown ) {  // remove point
            ggRemove.doClick( 100 )
         } else if( e.isShiftDown ) {  // range search
            drag = Some( e -> None )
         } else if( e.getClickCount == 2 ) { // add point
            ggAdd.doClick( 100 )
         }
      }
   }
   model.addMouseAdapter( ma )

   add( model.view, BorderLayout.CENTER )
   private val ggStatus = new JTextField( 16 )
   ggStatus.setEditable( false )
   private def status( str: String ) { ggStatus.setText( str )}
   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )

//addPoints( 20 )
//removePoints( 14 )

//   def verifyConsistency() {
//      val q = t.hyperCube
//      var h = t.lastTree
//      var curreUnlinkedHyperCubes   = Set.empty[ D#HyperCube ]
//      var currPoints                = Set.empty[ D#Point ]
//      var prevs = 0
//      do {
//         assert( h.hyperCube == q, "Root level hyper-cube is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
//         val nextUnlinkedQuad2Ds   = curreUnlinkedHyperCubes
//         val nextPoints             = currPoints
//         curreUnlinkedHyperCubes    = Set.empty
//         currPoints                 = Set.empty
//         def checkChildren( n: t.QNode, depth: Int ) {
//            def assertInfo = " in level n-" + prevs + " / depth " + depth
//
//            var i = 0; while( i < t.numOrthants ) {
//               n.child( i ) match {
//                  case c: t.QNode =>
//                     val nq = n.hyperCube.orthant( i )
//                     val cq = c.hyperCube
//                     assert( nq.contains( cq ), "Node has invalid hyper-cube (" + cq + "), expected: " + nq + assertInfo )
//                     assert( n.hyperCube.indexOf( cq ) == i, "Mismatch between index-of and used orthant (" + i + "), with parent " + n.hyperCube + " and " + cq )
//                     c.nextOption match {
//                        case Some( next ) =>
//                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
//                           assert( next.hyperCube == cq, "Next hyper-cube does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
//                        case None =>
//                           assert( !nextUnlinkedQuad2Ds.contains( cq ), "Double missing link for " + cq + assertInfo )
//                     }
//                     c.prevOption match {
//                        case Some( prev ) =>
//                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
//                           assert( prev.hyperCube == cq, "Next hyper-cube do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
//                        case None => curreUnlinkedHyperCubes += cq
//                     }
//                     checkChildren( c, depth + 1 )
//                  case l: t.QLeaf =>
//                     currPoints += l.value
//                  case _: t.QEmpty =>
//               }
//            i += 1 }
//         }
//         checkChildren( h, 0 )
//         val pointsOnlyInNext    = nextPoints.filterNot( currPoints.contains( _ ))
//         assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
//         h                       = h.prevOption.orNull
//         prevs += 1
//      } while( h != null )
//
//      println( "Consistency check successful." )
//   }
}