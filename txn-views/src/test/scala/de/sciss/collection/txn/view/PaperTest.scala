package de.sciss.collection.txn.view

import java.awt.{BorderLayout, EventQueue}
import javax.swing.{JFrame, WindowConstants}
import de.sciss.lucre.stm.InMemory
import de.sciss.collection.txn.view.InteractiveSkipOctreePanel.Model2D
import de.sciss.collection.geom.{IntPoint2D, Square}
import de.sciss.collection.geom.Space.TwoDim
import de.sciss.collection.txn.{DeterministicSkipOctree, SpaceSerializers}

object PaperTest extends App with Runnable {
  EventQueue.invokeLater( this )

   val sz = 128

  def run() {
    implicit val system = InMemory()
    import SpaceSerializers.{Point2DSerializer, SquareSerializer}
    implicit val pointView = (p: Point2D, t: Any) => p
    implicit val reader = DeterministicSkipOctree.serializer[ InMemory, TwoDim, Point2D ]
    val access = system.root { implicit tx =>
       DeterministicSkipOctree.empty[ InMemory, TwoDim, Point2D ](
          Square( sz, sz, sz ), skipGap = 1 )
    }
    val model = new Model2D[ InMemory ]( system, access, { () => println( "(Consistency not checked)" )})

     val f    = new JFrame( "Skip Octree" )
    //      f.setResizable( false )
     val cp   = f.getContentPane
     val iv   = model.newPanel()
     cp.add( iv, BorderLayout.CENTER )
     model.addPDFSupport( f )
     f.pack()
     f.setLocationRelativeTo( null )
     f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//     f.setVisible( true )

     val rnd = new util.Random(33)
     def gen(n: Int) = {
        var seq = (1 to n).toIndexedSeq
        var res = IndexedSeq.empty[Int]
        while( seq.nonEmpty ) {
           val i = rnd.nextInt( seq.size )
           res :+= seq( i )
           seq = seq.patch( i, IndexedSeq.empty, 1 )
        }
        res
     }
     def gen2(n: Int, m: Int) = gen(n).map { i =>
        i * m + (rnd.nextInt( m * 2/3 ) - m/3)
     }

     val full = gen2(15,16).zip(gen2(15,16))
//     full.foreach(println)

     system.step { implicit tx =>
        full.foreach {
           case (x, y) =>
               model.tree.add( Point2D( x, y ))
        }
     }

     f.setVisible( true )
   }
}
