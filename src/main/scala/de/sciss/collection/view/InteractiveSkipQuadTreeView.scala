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

import java.awt.{Color, FlowLayout, EventQueue, BorderLayout}
import java.awt.event.{ActionListener, MouseEvent, MouseAdapter, ActionEvent}
import mutable.{SkipQuadtree, DeterministicSkipQuadtree, RandomizedSkipQuadtree}
import javax.swing.{JLabel, SwingConstants, Box, WindowConstants, JComboBox, AbstractButton, ButtonGroup, JToolBar, JTextField, JButton, JFrame, JPanel}
import geom.{SquareLike, DistanceMeasure2D, Space, DistanceMeasure, Point2D, Point2DLike, Square}

object InteractiveSkipQuadtreeView extends App with Runnable {
   val seed = 0L

   EventQueue.invokeLater( this )
   def run() {
      val mode = args.toSeq match {
         case Seq( "--det" ) => Deterministic
         case _ => Randomized
      }

      val f    = new JFrame( "Skip Quad2Dtree" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = new InteractiveSkipQuadtreeView( mode )
      cp.add( iv, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      PDFSupport.addMenu[ SkipQuadtreeView[ Point2DLike ]]( f, iv.slv :: Nil, _.adjustPreferredSize )
      f.setVisible( true )
   }

   sealed trait Mode
   case object Randomized extends Mode
   case object Deterministic extends Mode
}
class InteractiveSkipQuadtreeView( mode: InteractiveSkipQuadtreeView.Mode )
extends JPanel( new BorderLayout() ) {
   import InteractiveSkipQuadtreeView._

   private val rnd = new util.Random( seed )

   val t    = mode match {
      case Randomized      => RandomizedSkipQuadtree.empty[    Point2DLike ]( Square( 256, 256, 256 ))
      case Deterministic   => DeterministicSkipQuadtree.empty[ Point2DLike ]( Square( 256, 256, 256 ), skipGap = 1 )
   }
   val slv  = new SkipQuadtreeView( t )
   private val in = slv.getInsets

   private var baseDist : DistanceMeasure[ Space.TwoDim ] = DistanceMeasure2D.euclideanSq
   private var distFilter : DistanceMeasure[ Space.TwoDim ] => DistanceMeasure[ Space.TwoDim ] = identity
   private var distMeasure : DistanceMeasure[ Space.TwoDim ] = baseDist

   def recalcDistMeasure { distMeasure = distFilter( baseDist )}

   val tools   = new JToolBar()
   val toolGrp = new ButtonGroup()
   add( tools, BorderLayout.NORTH )

   private val ggX   = new JTextField( 3 )
   private val ggY   = new JTextField( 3 )
   private val ggExt = new JTextField( 3 )

   private def updateNum( x: Int, y: Int ) {
      ggX.setText( x.toString )
      ggY.setText( y.toString )
   }

   private def tryPoint( fun: Point2D => Unit ) {
      try {
         val p = Point2D( ggX.getText.toInt, ggY.getText.toInt )
         fun( p )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private def tryQuad2D( fun: Square => Unit ) {
      try {
         val ext = ggExt.getText.toInt
         require( ext > 0 )
         val q = Square( ggX.getText.toInt, ggY.getText.toInt, ext )
         fun( q )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private val p = new JPanel( new FlowLayout() )
   private def but( lb: String )( action: => Unit ) : AbstractButton = {
      val b = new JButton( lb )
      b.putClientProperty( "JButton.buttonType", "bevel" )
      b.setFocusable( false )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            action
            slv.repaint()
         }
      })
      p.add( b )
      b
   }
   private def combo( items: String* )( action: Int => Unit ) : JComboBox = {
      val b = new JComboBox( items.toArray[ AnyRef ])
      b.putClientProperty( "JComboBox.isSquare", java.lang.Boolean.TRUE )
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

   p.add( ggX )
   p.add( ggY )
   p.add( ggExt )

   private val ggAdd = but( "Add" ) { tryPoint { p =>
      t += p
      slv.highlight = Set( p )
   }}
   private val ggRemove = but( "Remove" ) { tryPoint( t -= _ )}
   but( "Contains" ) { tryPoint { p =>
      status( t.contains( p ).toString )
      slv.highlight = Set( p )
   }}

   private def rangeString( pt: Set[ Point2DLike ]) : String = {
      val s = pt.map( p => "(" + p.x + "," + p.y + ")" ).mkString( " " )
      if( s.isEmpty ) "(empty)" else s
   }

   def findNN { tryPoint { p =>
      val set = t.nearestNeighborOption( p, metric = distMeasure ).map( Set( _ )).getOrElse( Set.empty )
      slv.highlight = set
      status( rangeString( set ))
   }}

   combo( "Euclidean", "Maximum", "Minimum" ) { i => baseDist = i match {
         case 0 => DistanceMeasure2D.euclideanSq
         case 1 => DistanceMeasure2D.chebyshev
         case 2 => DistanceMeasure2D.vehsybehc
      }
      recalcDistMeasure
   }
   combo( "All Quad2Drants", "North East", "North West", "South West", "South East" ) { i =>
      if( i > 0 ) {
         distFilter = _.orthant( i - 1 )
      } else {
         distFilter = identity
      }
      recalcDistMeasure
   }

   but( "NN" )( findNN )

   but( "Range" ) { tryQuad2D { q =>
      val set = t.rangeQuery( q ).toSet
      status( rangeString( set.take( 3 )))
      println( rangeString( set ))
   }}

   space()
   label( "Randomly:" )

   private def addPoints( num: Int ) {
      val ps = Seq.fill( num )( Point2D( rnd.nextInt( 512 ), rnd.nextInt( 512 )))
      t ++= ps
      slv.highlight = ps.toSet
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
      verifyConsistency( t )
   }

   private val ma = new MouseAdapter {
      var drag = Option.empty[ (MouseEvent, Option[ MouseEvent ])]

      val colrTrns = new Color( 0x00, 0x00, 0xFF, 0x40 )
      val topPointer = (h: QuadView.PaintHelper) => {
         tryQuad2D { q =>
            h.g2.setColor( Color.blue )
            h.g2.drawRect( q.left, q.top, q.side, q.side )
            h.g2.setColor( colrTrns )
            h.g2.fillRect( q.left, q.top, q.side, q.side )
         }
      }

      override def mouseDragged( e: MouseEvent ) {
         drag match {
            case Some( (m1, None) ) =>
               val dist = e.getPoint.distance( m1.getPoint )
//println( "Kieka " + dist )
               if( dist > 4 ) {
                  slv.topPainter = Some( topPointer )
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
         tryQuad2D { q =>
            val set = t.rangeQuery( q ).toSet
            slv.highlight = set
            slv.repaint()
            status( rangeString( set.take( 3 )))
         }
      }

      override def mouseReleased( e: MouseEvent ) {
         drag match {
            case Some( (_, Some( _ ))) =>
               slv.topPainter = None
               slv.repaint()
            case _ =>
         }
         drag = None
      }

      override def mousePressed( e: MouseEvent ) {
         val x = e.getX - in.left
         val y = e.getY - in.top
         updateNum( x, y )
         if( e.isControlDown ) {
            findNN
            slv.repaint()
         } else if( e.isAltDown ) {  // remove point
            ggRemove.doClick( 100 )
         } else if( e.isShiftDown ) {  // range search
            drag = Some( e -> None )
         } else if( e.getClickCount == 2 ) { // add point
            ggAdd.doClick( 100 )
         }
      }
   }
   slv.addMouseListener( ma )
   slv.addMouseMotionListener( ma )

   add( slv, BorderLayout.CENTER )
//   private def tryNum( fun: Int => Unit ) { try { val i = ggNum.getText().toInt; fun( i )} catch { case nfe: NumberFormatException => }}
   private val ggStatus = new JTextField( 16 )
   ggStatus.setEditable( false )
   private def status( str: String ) { ggStatus.setText( str )}
//   private val colrGreen = new Color( 0x00, 0xA0, 0x00 )

//   butAddRemove( "Add" )( t.add( _ ))
//   butAddRemove( "Remove" )( l.remove( _ ))

//   but( "Contains" ) { tryNum { i =>
//      status( l.contains( i ).toString )
//      slv.highlight = Map( i -> Color.blue )
//   }}
   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )

//// TEST CODE
//println( "---ADDING" )
//addPoints( 31 )
//println( "---REMOVING" )
//removePoints( 4 )

   def verifyConsistency( t: SkipQuadtree[ Point2DLike ]) {
      val q = t.hyperCube
      var h = t.lastTree
      var currUnlinkedQuad2Ds   = Set.empty[ SquareLike ]
      var currPoints          = Set.empty[ Point2DLike ]
      var prevs = 0
      do {
         assert( h.hyperCube == q, "Root level quad is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedQuad2Ds   = currUnlinkedQuad2Ds
         val nextPoints          = currPoints
         currUnlinkedQuad2Ds       = Set.empty
         currPoints              = Set.empty
         def checkChildren( n: t.QNode, depth: Int ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < 4 ) {
               n.child( i ) match {
                  case c: t.QNode =>
                     val nq = n.hyperCube.orthant( i )
                     val cq = c.hyperCube
                     assert( nq.contains( cq ), "Child has invalid quad (" + cq + "), expected: " + nq + assertInfo )
                     c.nextOption match {
                        case Some( next ) =>
                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
                           assert( next.hyperCube == cq, "Next quad does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
                        case None =>
                           assert( !nextUnlinkedQuad2Ds.contains( cq ), "Double missing link for " + cq + assertInfo )
                     }
                     c.prevOption match {
                        case Some( prev ) =>
                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
                           assert( prev.hyperCube == cq, "Next quad do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
                        case None => currUnlinkedQuad2Ds += cq
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