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

   private val ggX = new JTextField( 3 )
   private val ggY = new JTextField( 3 )

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

   p.add( ggX )
   p.add( ggY )

   private val ggAdd             = but( "Add" )      { tryPoint( p => t += p -> () )}
   private val ggRemove          = but( "Remove" )   { tryPoint( t -= _ )}
   /* private val ggContains = */  but( "Contains" ) { tryPoint( p => status( t.contains( p ).toString ))}

   slv.addMouseListener( new MouseAdapter {
      override def mousePressed( e: MouseEvent ) {
         val x = e.getX - in.left
         val y = e.getY - in.top
         updateNum( x, y )
         if( e.isAltDown ) {
            ggRemove.doClick( 100 )
         } else if( e.getClickCount == 2 ) {
            ggAdd.doClick( 100 )
         }
      }
   })

   add( slv, BorderLayout.CENTER )
//   private def tryNum( fun: Int => Unit ) { try { val i = ggNum.getText().toInt; fun( i )} catch { case nfe: NumberFormatException => }}
   private val ggStatus = new JTextField( 12 )
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