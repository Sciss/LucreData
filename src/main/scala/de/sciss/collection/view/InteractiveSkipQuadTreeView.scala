/*
 *  InteractiveSkipQuadTreeView.scala
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
package view

import java.awt.{Color, FlowLayout, EventQueue, BorderLayout}
import java.awt.event.{ActionListener, MouseEvent, MouseAdapter, ActionEvent}
import javax.swing.{WindowConstants, JComboBox, AbstractButton, ButtonGroup, JToolBar, JTextField, JButton, JFrame, JPanel}

object InteractiveSkipQuadTreeView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run() {
      val f    = new JFrame( "Skip Quadtree" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = new InteractiveSkipQuadTreeView
      cp.add( iv, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      QuadTreeTest.addPDFExport[ SkipQuadTreeView[ PointLike ]]( f, iv.slv :: Nil, _.adjustPreferredSize )
      f.setVisible( true )
   }
}
class InteractiveSkipQuadTreeView extends JPanel( new BorderLayout() ) {
   val t    = RandomizedSkipQuadTree.empty[ PointLike ]( Quad( 256, 256, 256 ))
   val slv  = new SkipQuadTreeView( t )
   private val in = slv.getInsets

   private var baseDist : DistanceMeasure = DistanceMeasure.euclideanSq
   private var distFilter : DistanceMeasure => DistanceMeasure = identity
   private var distMeasure : DistanceMeasure = baseDist

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

   private def tryPoint( fun: Point => Unit ) {
      try {
         val p = Point( ggX.getText.toInt, ggY.getText.toInt )
         fun( p )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private def tryQuad( fun: Quad => Unit ) {
      try {
         val ext = ggExt.getText.toInt
         require( ext > 0 )
         val q = Quad( ggX.getText.toInt, ggY.getText.toInt, ext )
         fun( q )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private val p = new JPanel( new FlowLayout() )
   private def but( lb: String )( action: => Unit ) : AbstractButton = {
      val b = new JButton( lb )
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
      b.setFocusable( false )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            action( b.getSelectedIndex )
         }
      })
      p.add( b )
      b
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

   private def rangeString( pt: Set[ PointLike ]) : String = {
      val s = pt.map( p => "(" + p.x + "," + p.y + ")" ).mkString( " " )
      if( s.isEmpty ) "(empty)" else s
   }

   def findNN { tryPoint { p =>
      val set = t.nearestNeighborOption( p, metric = distMeasure ).map( Set( _ )).getOrElse( Set.empty )
      slv.highlight = set
      status( rangeString( set ))
   }}

   combo( "Euclidean", "Maximum", "Minimum" ) { i => baseDist = i match {
         case 0 => DistanceMeasure.euclideanSq
         case 1 => DistanceMeasure.chebyshev
         case 2 => DistanceMeasure.vehsybehc
      }
      recalcDistMeasure
   }
   combo( "All Quadrants", "North East", "North West", "South West", "South East" ) { i =>
      if( i > 0 ) {
         distFilter = _.quadrant( i - 1 )
      } else {
         distFilter = identity
      }
      recalcDistMeasure
   }

   but( "NN" )( findNN )

   but( "Range" ) { tryQuad { q =>
      val set = t.rangeQuery( q ).toSet
      status( rangeString( set.take( 3 )))
      println( rangeString( set ))
   }}

   but( "Add 10x Random" ) {
      val ps = Seq.fill( 10 )( Point( util.Random.nextInt( 512 ), util.Random.nextInt( 512 )))
      t ++= ps
      slv.highlight = ps.toSet
   }

   but( "Remove 10x" ) {
      t --= t.iterator.take( 10 ).toList
   }

   private val ma = new MouseAdapter {
      var drag = Option.empty[ (MouseEvent, Option[ MouseEvent ])]

      val colrTrns = new Color( 0x00, 0x00, 0xFF, 0x40 )
      val topPointer = (h: QuadView.PaintHelper) => {
         tryQuad { q =>
            h.g2.setColor( Color.blue )
            h.g2.drawRect( q.left, q.top, q.side, q.side )
            h.g2.setColor( colrTrns )
            h.g2.fillRect( q.left, q.top, q.side, q.side )
         }
      }

      override def mouseDragged( e: MouseEvent ) {
         drag match {
            case Some( (m1, None) ) =>
               val dist = e.getPoint().distance( m1.getPoint() )
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
         val ext = math.max( math.abs( m1.getPoint().x - m2.getPoint().x ),
                             math.abs( m1.getPoint().y - m2.getPoint().y ))
         ggExt.setText( ext.toString )
         tryQuad { q =>
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
   private val ggStatus = new JTextField( 18 )
   ggStatus.setEditable( false )
   private def status( str: String ) { ggStatus.setText( str )}
   private val colrGreen = new Color( 0x00, 0xA0, 0x00 )

//   butAddRemove( "Add" )( t.add( _ ))
//   butAddRemove( "Remove" )( l.remove( _ ))

//   but( "Contains" ) { tryNum { i =>
//      status( l.contains( i ).toString )
//      slv.highlight = Map( i -> Color.blue )
//   }}
   p.add( ggStatus )
   add( p, BorderLayout.SOUTH )
}