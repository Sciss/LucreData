package de.sciss.collection.view

import javax.swing.{JComponent, BorderFactory}
import de.sciss.collection.mutable.SkipOctree
import de.sciss.collection.geom.{Point3DLike, Space}
import java.awt.{Dimension, Graphics, Color}
import java.awt.event.{FocusEvent, FocusListener, MouseAdapter, MouseEvent}
import edu.hendrix.ozark.burch.wireframe.{Composite, Polygon, Model, Point, TransformUtility, Transform, Graphics3D, Vector => Vector3D}

class SkipOctree3DView( t: SkipOctree[ Space.ThreeDim, Point3DLike ]) extends JComponent {
   setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ))
   setBackground( Color.white )
   setPreferredSize( new Dimension( 512, 256 ))
   setFocusable( true )

   private val RADIUS = 30

   private var viewT = Transform.IDENTITY
//   private var projT = Transform.IDENTITY
   private val projT = TransformUtility.perspectiveProjection( -1, 1, -1, 1, RADIUS - 1, RADIUS + 1 )

   private var u: Vector3D = null
   private var v: Vector3D = null
   private var n: Vector3D = null

//   private var model: Model = Model.EMPTY

   var highlight = Set.empty[ Point3DLike ]

   def resetPosition() {
      u = Vector3D.create( 0, 1, 0 )
      v = Vector3D.create( 1, 0, 0 )
      n = Vector3D.create( 0, 0, 1 )
      computeView()
   }

   private def computeView() {
      viewT = TransformUtility.viewTransform( Point.ORIGIN.addScaled( RADIUS, n ), Point.ORIGIN, u )
      repaint()
   }

   private var modelDirty = true

   resetPosition()

   addMouseListener { new MouseAdapter {
      override def mousePressed( e: MouseEvent ) {
         requestFocus()
      }
   }}
   addFocusListener( new FocusListener {
      def focusLost( e: FocusEvent ) { repaint() }
      def focusGained( e: FocusEvent ) { repaint() }
   })

   def treeUpdated() {
      modelDirty = true
      repaint()
   }

   private var m = Model.EMPTY

   private def resetModel() { m = Model.EMPTY }

   private def poly( points: (Int, Int, Int)* ) {
      val pts = points.map { case (x, y, z) => Point.create( x, y, z )}
      val p = new Polygon( pts.toArray )
      m = if( m == Model.EMPTY ) p else new Composite( Array[ Model ]( m, p ))
   }

   override def paintComponent( g: Graphics ) {
      super.paintComponent( g )
      g.translate( 4, 4 )
      try {
         val g3   = new Graphics3D( g )
         val w    = getWidth - 8
         val h    = getHeight - 8
         g.setColor( getBackground )
         g.fillRect( 0, 0, w, h )
         g.setColor( getForeground )
         g3.setViewTransform( viewT )
         g3.setProjectionTransform( projT )
         g3.setViewFrameTransform( Transform.translate( 10, 10, 1 ))
//               .prepend( Transform.scale( w / 2, -h / 2, 1 ))
//               .prepend( Transform.translate( 0, h, 0 )))

         if( modelDirty ) {
            modelDirty = false
            resetModel()
            addChild( t.headTree )
         }
         m.draw( g3 )

         if( hasFocus ) {
            g.setColor( Color.blue )
            g.drawRect( 0, 0, w - 1, h - 1 )
         }
      } finally {
         g.translate( -4, -4 )
      }
   }

   private def addChild( ch: t.Q ) {
//      ch match {
//         case n: t.QNode =>
//            for( idx <- 0 until 4 ) {
//               h.drawFrame( n.hyperCube.orthant( idx ), gridColor )
//               draw( h, n.child( idx ))
//            }
//         case _: t.QEmpty =>
//         case l: t.QLeaf =>
//            h.drawPoint( t.pointView( l.value ), highlight.contains( l.value ))
//      }
   }

//   private def recalcModel() {
//      val square = new Polygon( Array[ Point ](
//         Point.create(-1,  1, 0),
//         Point.create( 1,  1, 0),
//         Point.create( 1, -1, 0),
//         Point.create(-1, -1, 0)
//      ))
//      val cube = new Composite( Array[ Model ](
//         square.translate(0, 0,  1),
//         square.translate(0, 0, -1),
//         square.rotateX( math.Pi / 2.0).translate(0,  1, 0),
//         square.rotateX( math.Pi / 2.0).translate(0, -1, 0)
//      ))
//      val stack = new Composite( Array[ Model ](
//         cube.scale(0.4, 0.4, 0.4).translate(0, -0.3, 0),
//         cube.scale(0.2, 0.2, 0.2).translate(0, 0.3, 0),
//         cube.scale(0.1, 0.1, 0.1).translate(0, 0.6, 0),
//         cube.scale(0.2, 0.2, 0.2).translate(0.6, -0.5, 0),
//         cube.scale(0.1, 0.1, 0.1).translate(0.9, -0.6, 0)
//      ))
//      model = stack
//   }
}