package de.sciss.tree
package view

class QuadTreeView( t: QuadTree.Q[ _ ]) extends QuadView {
   protected def draw( h: QuadView.PaintHelper ) {
      draw( h, t )
   }

   def rootQuad = t.quad

   private def draw( h: QuadView.PaintHelper, quad: QuadTree.Q[ _ ]) {
      quad match {
         case t: QuadTree.QNode[ _ ]   => List( t.nw, t.ne, t.sw, t.se ).foreach( draw( h, _ ))
         case _: QuadTree.QEmpty[ _ ]  => h.drawFrame( quad.quad )
         case l: QuadTree.QLeaf[ _ ]   =>
            h.drawFrame( quad.quad )
            h.drawPoint( l.point )
      }
   }
}