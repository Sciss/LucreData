package de.sciss.collection.view

import java.awt.{Color, FlowLayout, EventQueue, BorderLayout}
import javax.swing.{ButtonGroup, JToolBar, JTextField, JButton, JFrame, WindowConstants, JPanel}
import java.awt.event.{MouseEvent, MouseAdapter, ActionEvent, ActionListener}
import de.sciss.collection.{Point, Quad, RandomizedSkipQuadTree}

object InteractiveSkipQuadTreeView extends App with Runnable {
   EventQueue.invokeLater( this )
   def run {
      val f    = new JFrame( "Skip Quadtree" )
      f.setResizable( false )
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
   val slv  = new SkipQuadTreeView( t.headTree )
   private val in = slv.getInsets

   val tools   = new JToolBar()
   val toolGrp = new ButtonGroup()
   add( tools, BorderLayout.NORTH )

   private val ggX = new JTextField( 3 )
   private val ggY = new JTextField( 3 )

   def updateNum( x: Int, y: Int ) {
      ggX.setText( x.toString )
      ggY.setText( y.toString )
   }

   slv.addMouseListener( new MouseAdapter {
      override def mousePressed( e: MouseEvent ) {
         val x = e.getX - in.left
         val y = e.getY - in.top
         updateNum( x, y )
         if( e.getClickCount == 2 ) {
            t += Point( x, y ) -> ()
            slv.repaint()
         }
      }
   })

   add( slv, BorderLayout.CENTER )
   private val p = new JPanel( new FlowLayout() )
   def but( lb: String )( action: => Unit ) {
      val b = new JButton( lb )
      b.setFocusable( false )
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            action
            slv.repaint()
         }
      })
      p.add( b )
   }
   p.add( ggX )
   p.add( ggY )
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