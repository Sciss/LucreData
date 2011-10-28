/*
 *  InteractiveSkipQuadtreeView.scala
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

import java.awt.event.{ActionListener, MouseEvent, MouseAdapter, ActionEvent}
import java.awt.{Insets, Color, FlowLayout, EventQueue, BorderLayout}
import mutable.{DeterministicSkipOctree, RandomizedSkipOctree, SkipOctree, DeterministicSkipQuadtree, RandomizedSkipQuadtree}
import javax.swing.{JComponent, JLabel, SwingConstants, Box, WindowConstants, JComboBox, AbstractButton, ButtonGroup, JToolBar, JTextField, JButton, JFrame, JPanel}
import geom.{DistanceMeasure3D, Point3D, CubeLike, QueryShape, Cube, Point3DLike, SquareLike, DistanceMeasure2D, Space, DistanceMeasure, Point2D, Point2DLike, Square}

object InteractiveSkipOctreePanel extends App with Runnable {
   val seed = 0L

   EventQueue.invokeLater( this )
   def run() {
      val xs = args.toSeq
      val mode  = if( xs.contains( "--det" )) Deterministic else Randomized
      val model = if( xs.contains( "--3d" )) new Model3D( mode ) else new Model2D( mode )

      val f    = new JFrame( "Skip Octree" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = model.newPanel()
      cp.add( iv, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//      PDFSupport.addMenu[ SkipQuadtreeView[ Point2DLike ]]( f, iv.slv :: Nil, _.adjustPreferredSize )
      f.setVisible( true )
   }

   private val sz = 256

   private final class Model2D( mode: Mode ) extends Model[ Space.TwoDim ] {
      val tree = mode match {
         case Randomized =>
            RandomizedSkipQuadtree.empty[    Point2DLike ]( Square( sz, sz, sz ))
         case Deterministic =>
            DeterministicSkipQuadtree.empty[ Point2DLike ]( Square( sz, sz, sz ))
      }

      def queryShape( sq: SquareLike ) = sq
      def point( coords: IndexedSeq[ Int ]) = coords match {
         case IndexedSeq( x, y ) => Point2D( x, y )
      }
      def coords( p: Point2DLike ) : IndexedSeq[ Int ] = IndexedSeq( p.x, p.y )
      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) = coords match {
         case IndexedSeq( x, y ) => Square( x, y, ext )
      }

      val view = new SkipQuadtreeView[ Point2DLike ]( tree )
      def repaint() { view.repaint() }
      val baseDistance = DistanceMeasure2D.euclideanSq

      def highlight: Set[ Point2DLike ] = view.highlight
      def highlight_=( points: Set[ Point2DLike ]) { view highlight = points }
   }

   private final class Model3D( mode: Mode ) extends Model[ Space.ThreeDim ] {
      val tree = mode match {
         case Randomized =>
            RandomizedSkipOctree.empty[ Space.ThreeDim, Point3DLike ]( Space.ThreeDim, Cube( sz, sz, sz, sz ))
         case Deterministic =>
            DeterministicSkipOctree.empty[ Space.ThreeDim, Point3DLike ]( Space.ThreeDim, Cube( sz, sz, sz, sz ))
      }

      def queryShape( c: CubeLike ) = c
      def point( coords: IndexedSeq[ Int ]) = coords match {
         case IndexedSeq( x, y, z ) => Point3D( x, y, z )
      }
      def coords( p: Point3DLike ) : IndexedSeq[ Int ] = IndexedSeq( p.x, p.y, p.z )
      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) = coords match {
         case IndexedSeq( x, y, z ) => Cube( x, y, z, ext )
      }

      val view = new SkipOctree3DView( tree )
      def repaint() { view.treeUpdated() }
      val baseDistance = DistanceMeasure3D.euclideanSq
      def highlight: Set[ Point3DLike ] = view.highlight
      def highlight_=( points: Set[ Point3DLike ]) { view.highlight = points }
   }

   sealed trait Mode
   case object Randomized extends Mode
   case object Deterministic extends Mode

   trait Model[ D <: Space[ D ]] {
      def tree: SkipOctree[ D, D#Point ]
      def view: JComponent
      final def insets: Insets = view.getInsets
      def point( coords: IndexedSeq[ Int ]) : D#Point
      def coords( p: D#Point ) : IndexedSeq[ Int ]
      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) : D#HyperCube
      def baseDistance: DistanceMeasure[ D ]
//      def distanceFilter: DistanceMeasure[ D ] => DistanceMeasure[ D ]
      def highlight: Set[ D#Point ]
      def highlight_=( points: Set[ D#Point ]) : Unit
      final def pointString( p: D#Point ) : String = coords( p ).mkString( "(", "," , ")" )
      final def newPanel() : InteractiveSkipOctreePanel[ D ] = new InteractiveSkipOctreePanel( this )
      def queryShape( q: D#HyperCube ) : QueryShape[ D ]
      def repaint() : Unit
   }
}
class InteractiveSkipOctreePanel[ D <: Space[ D ]]( val model: InteractiveSkipOctreePanel.Model[ D ])
extends JPanel( new BorderLayout() ) {
   import InteractiveSkipOctreePanel._

   val t = model.tree
   private val rnd = new util.Random( seed )

//   val t    = mode match {
//      case Randomized      => RandomizedSkipQuadtree.empty[    Point2DLike ]( Square( 256, 256, 256 ))
//      case Deterministic   => DeterministicSkipQuadtree.empty[ Point2DLike ]( Square( 256, 256, 256 ), skipGap = 2 )
//   }
//   val slv  = new SkipQuadtreeView( t )
   private val in = model.insets

//   private var baseDist : DistanceMeasure[ Space.TwoDim ] = DistanceMeasure2D.euclideanSq
   private val distFilter : DistanceMeasure[ D ] => DistanceMeasure[ D ] = identity
   private var distMeasure : DistanceMeasure[ D ] = model.baseDistance

   def recalcDistMeasure() { distMeasure = distFilter( model.baseDistance )}

//   private val tools   = new JToolBar()
//   private val toolGrp = new ButtonGroup()
//   add( tools, BorderLayout.NORTH )

   private val ggCoord  = IndexedSeq.fill( t.space.dim )( new JTextField( 3 ))

//   private val ggX   = new JTextField( 3 )
//   private val ggY   = new JTextField( 3 )
   private val ggExt = new JTextField( 3 )

   private def updateNum( coords: Seq[ Int ]) {
      coords.zip( ggCoord ).foreach {
         case (value, gg) => gg.setText( value.toString )
      }
   }

   private def tryPoint( fun: D#Point => Unit ) {
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
      println( t.toList )
   }

   ggCoord.foreach( p.add )
   p.add( ggExt )

   private val ggAdd = but( "Add" ) { tryPoint { p =>
      t += p
      model.highlight = Set( p )
   }}
   private val ggRemove = but( "Remove" ) { tryPoint( t -= _ )}
   but( "Contains" ) { tryPoint { p =>
      status( t.contains( p ).toString )
      model.highlight = Set( p )
   }}

   private def rangeString( pt: Set[ D#Point ]) : String = {
      val s = pt.map( model.pointString ).mkString( " " )
      if( s.isEmpty ) "(empty)" else s
   }

   def findNN() { tryPoint { p =>
      val set = t.nearestNeighborOption( p, metric = distMeasure ).map( Set( _ )).getOrElse( Set.empty )
      model.highlight = set
      status( rangeString( set ))
   }}

//   combo( "Euclidean", "Maximum", "Minimum" ) { i => baseDist = i match {
//         case 0 => DistanceMeasure2D.euclideanSq
//         case 1 => DistanceMeasure2D.chebyshev
//         case 2 => DistanceMeasure2D.vehsybehc
//      }
//      recalcDistMeasure()
//   }
//   combo( "All Orthants", "North East", "North West", "South West", "South East" ) { i =>
//      if( i > 0 ) {
//         distFilter = _.orthant( i - 1 )
//      } else {
//         distFilter = identity
//      }
//      recalcDistMeasure()
//   }

   but( "NN" )( findNN() )

   but( "Range" ) { tryHyperCube { q =>
      val set = t.rangeQuery( model.queryShape( q )).toSet
      status( rangeString( set.take( 3 )))
      println( rangeString( set ))
   }}

   space()
   label( "Randomly:" )

   private def addPoints( num: Int ) {
      val ps = Seq.fill( num )( model.point( IndexedSeq.fill( t.space.dim )( rnd.nextInt( 512 ))))
      t ++= ps
      model.highlight = ps.toSet
   }

   but( "Add 1x" )  { addPoints(  1 )}
   but( "Add 10x" ) { addPoints( 10 )}

   space()
   label( "In order:" )

   private def removePoints( num: Int ) {
      t --= t.iterator.take( num ).toList
   }

   but( "Remove 1x" )  { removePoints(  1 )}
   but( "Remove 10x" ) { removePoints( 10 )}

   space()

   but( "Consistency" ) {
      verifyConsistency()
   }

   private val ma = new MouseAdapter {
      var drag = Option.empty[ (MouseEvent, Option[ MouseEvent ])]

      val colrTrns = new Color( 0x00, 0x00, 0xFF, 0x40 )
      val topPointer = (h: QuadView.PaintHelper) => {
//         tryQuad2D { q =>
//            h.g2.setColor( Color.blue )
//            h.g2.drawRect( q.left, q.top, q.side, q.side )
//            h.g2.setColor( colrTrns )
//            h.g2.fillRect( q.left, q.top, q.side, q.side )
//         }
      }

      override def mouseDragged( e: MouseEvent ) {
         drag match {
            case Some( (m1, None) ) =>
               val dist = e.getPoint.distance( m1.getPoint )
               if( dist > 4 ) {
//                  slv.topPainter = Some( topPointer )
                  drag( m1, e )
               }
            case Some( (m1, Some( _ ))) => drag( m1, e )
            case _ =>
         }
      }

      def drag( m1: MouseEvent, m2: MouseEvent ) {
//         drag = Some( m1 -> Some( m2 ))
//         val ext = math.max( math.abs( m1.getPoint.x - m2.getPoint.x ),
//                             math.abs( m1.getPoint.y - m2.getPoint.y ))
//         ggExt.setText( ext.toString )
//         tryQuad2D { q =>
//            val set = t.rangeQuery( q ).toSet
//            slv.highlight = set
//            slv.repaint()
//            status( rangeString( set.take( 3 )))
//         }
      }

      override def mouseReleased( e: MouseEvent ) {
         drag match {
            case Some( (_, Some( _ ))) =>
//               model.topPainter = None
               model.repaint()
            case _ =>
         }
         drag = None
      }

      override def mousePressed( e: MouseEvent ) {
         val x = e.getX - in.left
         val y = e.getY - in.top
//         updateNum( x, y )
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
//   model.addMouseListener( ma )
//   model.addMouseMotionListener( ma )

   add( model.view, BorderLayout.CENTER )
   private val ggStatus = new JTextField( 16 )
   ggStatus.setEditable( false )
   private def status( str: String ) { ggStatus.setText( str )}
   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )

   def verifyConsistency() {
      val q = t.hyperCube
      var h = t.lastTree
      var curreUnlinkedHyperCubes   = Set.empty[ D#HyperCube ]
      var currPoints                = Set.empty[ D#Point ]
      var prevs = 0
      do {
         assert( h.hyperCube == q, "Root level hyper-cube is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedQuad2Ds   = curreUnlinkedHyperCubes
         val nextPoints             = currPoints
         curreUnlinkedHyperCubes    = Set.empty
         currPoints                 = Set.empty
         def checkChildren( n: t.QNode, depth: Int ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < t.numOrthants ) {
               n.child( i ) match {
                  case c: t.QNode =>
                     val nq = n.hyperCube.orthant( i )
                     val cq = c.hyperCube
                     assert( nq.contains( cq ), "Child has invalid hyper-cube (" + cq + "), expected: " + nq + assertInfo )
                     c.nextOption match {
                        case Some( next ) =>
                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
                           assert( next.hyperCube == cq, "Next hyper-cube does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
                        case None =>
                           assert( !nextUnlinkedQuad2Ds.contains( cq ), "Double missing link for " + cq + assertInfo )
                     }
                     c.prevOption match {
                        case Some( prev ) =>
                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
                           assert( prev.hyperCube == cq, "Next hyper-cube do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
                        case None => curreUnlinkedHyperCubes += cq
                     }
                     checkChildren( c, depth + 1 )
                  case l: t.QLeaf =>
                     currPoints += l.value
                  case _: t.QEmpty =>
               }
            i += 1 }
         }
         checkChildren( h, 0 )
         val pointsOnlyInNext    = nextPoints.filterNot( currPoints.contains( _ ))
         assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
         h                       = h.prevOption.orNull
         prevs += 1
      } while( h != null )

      println( "Consistency check successful." )
   }
}