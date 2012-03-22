/*
*  SkipQuadtreeView.scala
*  (LucreData)
*
*  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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
package txn
package view

import java.awt.{Color, Dimension}
import de.sciss.collection.view.QuadView
import geom.{Point2DLike, Space}
import de.sciss.lucre.stm.{Cursor, Sys}

class SkipQuadtreeView[ S <: Sys[ S ], A ]( t: DeterministicSkipOctree[ S, Space.TwoDim, A ],
                                            cursor: Cursor[ S ], pointView: A => Point2DLike )
extends QuadView {
//   private type Child = txn.DeterministicSkipOctree.Node[ S, Space.TwoDim, A ]

   var highlight  = Set.empty[ A ]
   var gridColor  = new Color( 0x00, 0x00, 0x00, 0x30 )
   private var scaleVar = 1.0

   setPrefSz( 3 )

   def scale : Double = scaleVar
   def scale_=( factor: Double ) {
      scaleVar = factor
   }

   private def setPrefSz( lvl: Int ) {
      val w1   = ((t.hyperCube.extent.toLong << 1) * scale + 0.5).toInt + 1
      val in   = getInsets
      setPreferredSize( new Dimension( ((w1 + 16) * lvl - 16) + (in.left + in.right), w1 + (in.top + in.bottom) ))
   }

   def adjustPreferredSize() {
      setPrefSz( cursor.step { implicit tx => t.numLevels })
   }

   protected def draw( h: QuadView.PaintHelper ) {
      var n = t.headTree
      val q = t.hyperCube
      val dx = ((q.extent.toLong << 1) * scale + 0.5).toInt + 16
      h.scale= scale
      while( n != null ) {
         draw( h, n )
         h.translate( dx, 0 )
         n = cursor.step { implicit tx => n.nextOption.orNull }
      }
   }

   private def draw( h: QuadView.PaintHelper, quad: t.Child ) {
      quad match {
         case l: t.Leaf =>
            h.drawPoint( pointView( l.value ), highlight.contains( l.value ))
         case n: t.Branch =>
            for( idx <- 0 until 4 ) {
               h.drawFrame( n.hyperCube.orthant( idx ), gridColor )
               draw( h, cursor.step { implicit tx => n.child( idx )})
            }
         case _ =>
      }
   }
}