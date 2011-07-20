package de.sciss.collection.view

import java.awt.Dimension
import de.sciss.collection.geom.Quad

abstract class QuadTreeView extends QuadView {
   def rootQuad : Quad

   override def getPreferredSize : Dimension = {
      val w1   = rootQuad.extent * 2 + 1
      val in   = getInsets()
      new Dimension( w1 + (in.left + in.right), w1 + (in.top + in.bottom) )
   }

   protected def drawTree( h: QuadView.PaintHelper ) : Unit

   protected def draw( h: QuadView.PaintHelper ) {
      val q = rootQuad
      h.translate( q.extent - q.cx, q.extent - q.cy )
      drawTree( h )
   }
}