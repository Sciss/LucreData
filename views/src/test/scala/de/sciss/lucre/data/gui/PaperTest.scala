package de.sciss.lucre
package data
package gui

import java.awt.{BorderLayout, EventQueue}
import javax.swing.{JFrame, WindowConstants}
import stm.InMemory
import geom.{IntPoint2D, IntSquare}
import geom.IntSpace.TwoDim

object PaperTest extends App with Runnable {
  EventQueue.invokeLater( this )

   val sz = 128

  def run(): Unit = {
    implicit val system     = InMemory()
    implicit val pointView  = (p: TwoDim#Point, t: Any) => p
    import TwoDim.pointSerializer
    implicit val reader     = DeterministicSkipOctree.serializer[InMemory, TwoDim, TwoDim#Point]
    val access = system.root { implicit tx =>
      DeterministicSkipOctree.empty[InMemory, TwoDim, TwoDim#Point](
        IntSquare(sz, sz, sz), skipGap = 1)
    }
    val model = new InteractiveSkipOctreePanel.Model2D[InMemory](system, access, {
      () => println("(Consistency not checked)")
    })

    val f = new JFrame("Skip Octree")
    //      f.setResizable( false )
    val cp = f.getContentPane
    val iv = model.newPanel()
    cp.add(iv, BorderLayout.CENTER)
    // model.addPDFSupport(f)
    f.pack()
    f.setLocationRelativeTo(null)
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    //     f.setVisible( true )

    new de.sciss.pdflitz.SaveAction(model.view :: Nil).setupMenu(f)


    val rnd = new util.Random(33)
    def gen(n: Int) = {
      var seq = (1 to n).toIndexedSeq
      var res = IndexedSeq.empty[Int]
      while (seq.nonEmpty) {
        val i = rnd.nextInt(seq.size)
        res :+= seq(i)
        seq = seq.patch(i, IndexedSeq.empty, 1)
      }
      res
    }
    def gen2(n: Int, m: Int) = gen(n).map { i =>
      i * m + (rnd.nextInt(m * 2 / 3) - m / 3)
    }

    val full = gen2(15, 16).zip(gen2(15, 16))
    //     full.foreach(println)

    system.step { implicit tx =>
      full.foreach {
        case (x, y) =>
          model.tree.add(IntPoint2D(x, y))
      }
    }

    f.setVisible(true)
  }
}
