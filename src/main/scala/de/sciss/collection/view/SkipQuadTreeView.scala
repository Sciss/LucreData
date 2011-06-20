/*
 *  SkipQuadTreeView.scala
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

import java.awt.{Color, Dimension}

class SkipQuadTreeView[ V ]( t: SkipQuadTree[ V ]) extends QuadView {
   setPrefSz( 3 )

   var highlight  = Set.empty[ PointLike ]
   var gridColor  = new Color( 0x00, 0x00, 0x00, 0x30 ) // Color.black

   private var scaleVar = 1.0
   def scale : Double = scaleVar
   def scale_=( factor: Double ) {
      scaleVar = factor
   }

   private def setPrefSz( lvl: Int ) {
      val w1   = ((t.quad.extent.toLong << 1) * scale + 0.5).toInt + 1
      val in   = getInsets()
      setPreferredSize( new Dimension( ((w1 + 16) * lvl - 16) + (in.left + in.right), w1 + (in.top + in.bottom) ))
   }

   def adjustPreferredSize {
      setPrefSz( t.numLevels )
   }

   protected def draw( h: QuadView.PaintHelper ) {
      var n = t.headTree
      val q = t.quad
      val dx = ((q.extent.toLong << 1) * scale + 0.5).toInt + 16
      h.scale= scale
      while( n != null ) {
         draw( h, n )
         h.translate( dx, 0 )
         n = n.nextOption.orNull
      }
   }

   private def draw( h: QuadView.PaintHelper, quad: SkipQuadTree[ V ]#Q ) {
      quad match {
         case t: SkipQuadTree[ _ ]#QNode =>
            for( idx <- 0 until 4 ) {
               h.drawFrame( t.quad.quadrant( idx ), gridColor )
               draw( h, t.child( idx ))
            }
         case _: SkipQuadTree[ _ ]#QEmpty =>
         case l: SkipQuadTree[ _ ]#QLeaf =>
            h.drawPoint( l.point, highlight.contains( l.point ))
      }
   }
}