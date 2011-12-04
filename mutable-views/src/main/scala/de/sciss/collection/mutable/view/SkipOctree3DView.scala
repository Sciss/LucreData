/*
 *  SkipOctree3DView.scala
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
package mutable
package view

import edu.hendrix.ozark.burch.wireframe.{Polygon, Model, Point => WPoint, TransformUtility, Transform, Graphics3D, Vector => Vector3D}
import de.sciss.collection.mutable.SkipOctree
import de.sciss.collection.geom.Space
import annotation.switch
import javax.swing.event.{AncestorEvent, AncestorListener}
import javax.swing.{Timer, JComponent, BorderFactory}
import java.awt.event.{ActionEvent, ActionListener, KeyAdapter, KeyEvent, FocusEvent, FocusListener, MouseAdapter, MouseEvent}
import java.awt.{BasicStroke, RenderingHints, Graphics2D, Dimension, Graphics, Color}
import Space.ThreeDim

class SkipOctree3DView[ Point <: ThreeDim#PointLike ]( t: SkipOctree[ ThreeDim, Point ]) extends JComponent {
   import ThreeDim.PointLike

   setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ))
   setBackground( Color.white )
   setPreferredSize( new Dimension( 512, 256 ))
   setFocusable( true )

   private val extent   = t.hyperCube.extent
   private val RADIUS   = 10 // extent
   private val scale    = 1.25 // 6.0

   private var viewT = Transform.IDENTITY
//   private var projT = Transform.IDENTITY
//   private val projT = TransformUtility.perspectiveProjection( -1, 1, -1, 1, RADIUS - 1, RADIUS + 1 )
   private val projT = TransformUtility.perspectiveProjection( -2, 2, -2, 2, 0, -1 )

   private var u: Vector3D = null
   private var v: Vector3D = null
   private var n: Vector3D = null

   private var dx = 0
   private var dy = 0

//   private var model: Model = Model.EMPTY

   var highlight = Set.empty[ Point ]
   private val colrGreen = new Color( 0x00, 0xC0, 0x00 )
   private val strkThick = new BasicStroke( 4f )

   def resetPosition() {
      u = Vector3D.create( 0, 1, 0 )
      v = Vector3D.create( 1, 0, 0 )
      n = Vector3D.create( 0, 0, 1 )
      computeView()
   }

   private def computeView() {
      viewT = TransformUtility.viewTransform( WPoint.ORIGIN.addScaled( RADIUS, n ), WPoint.ORIGIN, u )
      repaint()
   }

   private var modelDirty = true

   resetPosition()

   private def step() {
      var newn = n
      if( dy != 0 ) newn = newn.add( u.scale( 0.002 * dy ))
      if( dx != 0 ) newn = newn.add( v.scale( 0.002 * dx ))
      if( newn == n ) return
      newn = newn.scale( 1.0 / newn.getLength )

      n = newn.scale( 1.0 / newn.getLength )
      u = u.subtract( u.projectOnto( n ))
      v = v.subtract( v.projectOnto( n ))
      computeView()
   }

   private var showin   = false
   private var movin    = true

   private val timer    = {
      val res = new Timer( 40, new ActionListener {
         def actionPerformed( e: ActionEvent ) { step() }
      })
      res.setRepeats( true )
      res
   }

   addMouseListener { new MouseAdapter {
      override def mousePressed( e: MouseEvent ) {
         requestFocus()
      }
   }}
   addFocusListener( new FocusListener {
      def focusLost( e: FocusEvent ) { repaint() }
      def focusGained( e: FocusEvent ) { repaint() }
   })
   addKeyListener( new KeyAdapter {
      override def keyPressed( e: KeyEvent ) {
         (e.getKeyCode: @switch) match {
            case KeyEvent.VK_UP     => dy += 1
            case KeyEvent.VK_DOWN   => dy -= 1
            case KeyEvent.VK_LEFT   => dx += 1
            case KeyEvent.VK_RIGHT  => dx -= 1
            case KeyEvent.VK_ESCAPE => dx = 0; dy = 0
            case KeyEvent.VK_SPACE if( showin ) =>
               movin = !movin
               if( movin ) timer.restart() else timer.stop()
            case _ =>
         }
      }
   })
   addAncestorListener( new AncestorListener {
      def ancestorAdded( e: AncestorEvent ) {
         showin = true
         if( movin ) timer.restart()
      }
      def ancestorRemoved( e: AncestorEvent ) {
         showin = false
         if( movin ) timer.stop()
      }
      def ancestorMoved( e: AncestorEvent ) {}
   })

   def treeUpdated() {
      modelDirty = true
      repaint()
   }

   private var levels = IndexedSeq.empty[ Level ]

   private val colrGray = new Color( 0, 0, 0, 0x2F )

   override def paintComponent( g: Graphics ) {
      super.paintComponent( g )
      val g2 = g.asInstanceOf[ Graphics2D ]
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      val atOrig = g2.getTransform
      g.translate( 4, 4 )
      try {
         val g3   = new Graphics3D( g )
         val w    = getWidth - 8
         val h    = getHeight - 8
         g.setColor( getBackground )
         g.fillRect( 0, 0, w, h )
         g3.setViewTransform( viewT )
         g3.setProjectionTransform( projT )
         g3.setViewFrameTransform(
//            Transform.scale( 1, 1, 2 )
//            .prepend(
               Transform.translate( extent, extent, 0 )
//            )

//               .prepend( Transform.scale( 1, 1, 2 ))
//               .prepend( Transform.translate( 0, 0, 50 ))
         )

         if( modelDirty ) {
            modelDirty = false
            levels   = IndexedSeq.empty
            var n    = t.headTree
            while( n != null ) {
               levels :+= new Level( n )
               n = n.next
            }
         }
//         m.draw( g3 )

         val atNow = g2.getTransform
         levels.foreach { level =>
            level.paint( g2, g3 )
            g.translate( (extent * 1.25).toInt + 64, 0 )
         }
         g2.setTransform( atNow )

         if( hasFocus ) {
            g.setColor( Color.blue )
            g.drawRect( 0, 0, w - 1, h - 1 )
         }

      } finally {
//         g.translate( -4, -4 )
         g2.setTransform( atOrig )
      }
   }

   private class Level( n: t.Q ) {
      var m     = IndexedSeq.empty[ Model ]
      var pts   = IndexedSeq.empty[ (WPoint, Color) ]

      addChild( n )

      def paint( g2: Graphics2D, g3: Graphics3D ) {
         g3.setColor( colrGray )
         m.foreach( _.draw( g3 ))

         val strkOrig = g2.getStroke
         g2.setStroke( strkThick )
         pts.foreach {
            case (point, colr) =>
               g3.setColor( colr )
               g3.drawLine( point, point )
         }

         g2.setStroke( strkOrig )
      }

      def poly( points: (Int, Int, Int)* ) {
         val pts = points.map { case (x, y, z) =>
            WPoint.create( (x - extent) * scale, (y - extent) * scale, (z - extent) * scale )
         }
         val p = new Polygon( pts.toArray )
         m :+= p // (p, colr)
//      m = if( m == Model.EMPTY ) p else new Composite( Array[ Model ]( m, p ))
      }

      def point( colr: Color, p: PointLike ) {
         val q = WPoint.create( (p.x - extent) * scale, (p.y - extent) * scale, (p.z - extent) * scale )
         pts :+= (q, colr)
      }

      def addChild( ch: t.Q ) {
         ch match {
            case n: t.QNode =>
               for( idx <- 0 until t.numOrthants ) {
                  drawFrame( n.hyperCube.orthant( idx ))
                  addChild( n.child( idx ))
               }

            case l: t.QLeaf =>
               val v = l.value
               val p = t.pointView( v )
               point( if( highlight.contains( v )) colrGreen else Color.red, p )

            case _ =>
         }
      }

      def drawFrame( c: ThreeDim#HyperCube ) {
         poly( // colr,
            (c.cx - c.extent, c.cy - c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy - c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy + c.extent, c.cz - c.extent),
            (c.cx - c.extent, c.cy + c.extent, c.cz - c.extent)
         )
         poly( // colr,
            (c.cx - c.extent, c.cy - c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy - c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy - c.extent, c.cz + c.extent),
            (c.cx - c.extent, c.cy - c.extent, c.cz + c.extent)
         )
         poly( // colr,
            (c.cx - c.extent, c.cy - c.extent, c.cz + c.extent),
            (c.cx + c.extent, c.cy - c.extent, c.cz + c.extent),
            (c.cx + c.extent, c.cy + c.extent, c.cz + c.extent),
            (c.cx - c.extent, c.cy + c.extent, c.cz + c.extent)
         )
         poly( // colr,
            (c.cx - c.extent, c.cy + c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy + c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy + c.extent, c.cz + c.extent),
            (c.cx - c.extent, c.cy + c.extent, c.cz + c.extent)
         )
         poly( // colr,
            (c.cx - c.extent, c.cy - c.extent, c.cz - c.extent),
            (c.cx - c.extent, c.cy - c.extent, c.cz + c.extent),
            (c.cx - c.extent, c.cy + c.extent, c.cz + c.extent),
            (c.cx - c.extent, c.cy + c.extent, c.cz - c.extent)
         )
         poly( // colr,
            (c.cx + c.extent, c.cy - c.extent, c.cz - c.extent),
            (c.cx + c.extent, c.cy - c.extent, c.cz + c.extent),
            (c.cx + c.extent, c.cy + c.extent, c.cz + c.extent),
            (c.cx + c.extent, c.cy + c.extent, c.cz - c.extent)
         )
      }
   }
}