package de.sciss.collection.view

import java.awt.{Color, FlowLayout, EventQueue, BorderLayout}
import java.awt.event.{MouseEvent, MouseAdapter, ActionEvent, ActionListener}
import de.sciss.collection.{Point, Quad, RandomizedSkipQuadTree}
import javax.swing.{AbstractButton, ButtonGroup, JToolBar, JTextField, JButton, JFrame, WindowConstants, JPanel}

object InteractiveSkipQuadTreeView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run {
      val f    = new JFrame( "Skip Quadtree" )
//      f.setResizable( false )
      val cp   = f.getContentPane
      val iv   = new InteractiveSkipQuadTreeView
      cp.add( iv, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }
}
class InteractiveSkipQuadTreeView extends JPanel( new BorderLayout() ) {
   val t    = RandomizedSkipQuadTree.empty[ Unit ]( Quad( 256, 256, 256 ))
   val slv  = new SkipQuadTreeView( t )
   private val in = slv.getInsets

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
         val p = Point( ggX.getText().toInt, ggY.getText().toInt )
         fun( p )
      } catch {
         case n: NumberFormatException =>
      }
   }

   private def tryQuad( fun: Quad => Unit ) {
      try {
         val ext = ggExt.getText().toInt
         require( ext > 0 )
         val q = Quad( ggX.getText().toInt, ggY.getText().toInt, ext )
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

   but( "Help" ) {
      println( """
- Number fields are cx, cy, and extent (for range queries)
- Mouse:
     Double-click to insert point
     Alt-click to remove point (-NOT YET WORKING-)
     Shift-drag for range query
     Ctrl-click for NN (-NOT FULLY WORKING-)
""" )
   }

   p.add( ggX )
   p.add( ggY )
   p.add( ggExt )

   private val ggAdd = but( "Add" ) { tryPoint { p =>
      t += p -> ()
      slv.highlight = Set( p )
   }}
   private val ggRemove = but( "Remove" ) { tryPoint( t -= _ )}
   but( "Contains" ) { tryPoint { p =>
      status( t.contains( p ).toString )
      slv.highlight = Set( p )
   }}

   private def rangeString( pt: Set[ Point ]) : String = {
      val s = pt.map( p => "(" + p.x + "," + p.y + ")" ).mkString( " " )
      if( s.isEmpty ) "(empty)" else s
   }

   def findNN { tryPoint { p =>
      val x = t.nonEmpty
      val set = if( x ) {
         val (p2, _) = t.nearestNeighbor( p )
         Set( p2 )
      } else Set.empty[ Point ]
      slv.highlight = set
      status( rangeString( set ))
   }}

   but( "NN" )( findNN )

   but( "Range" ) { tryQuad { q =>
      val set = t.rangeQuery( q ).map( _._1 ).toSet
      status( rangeString( set.take( 3 )))
      println( rangeString( set ))
   }}

   but( "Add 10x Random" ) {
      val ps = Seq.fill( 10 )( Point( util.Random.nextInt( 512 ), util.Random.nextInt( 512 )))
      t ++= ps.map( _ -> () )
      slv.highlight = ps.toSet
   }

   but( "Remove 10x" ) {
      t --= t.keysIterator.take( 10 ).toList
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
            val set = t.rangeQuery( q ).map( _._1 ).toSet
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