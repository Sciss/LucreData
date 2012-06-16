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
import geom.{IntSpace, IntPoint2DLike}
import de.sciss.lucre.stm.{Source, Cursor, Sys}

class SkipQuadtreeView[ S <: Sys[ S ], A ]( access: Source[ S#Tx, DeterministicSkipOctree[ S, IntSpace.TwoDim, A ]],
                                            cursor: Cursor[ S ], pointView: A => IntPoint2DLike )
extends QuadView {
//   private type Child = txn.DeterministicSkipOctree.Node[ S, Space.IntTwoDim, A ]

   def t( implicit tx: S#Tx ) : DeterministicSkipOctree[ S, IntSpace.TwoDim, A ] = access.get

   var highlight  = Set.empty[ A ]
   var gridColor  = new Color( 0x00, 0x00, 0x00, 0x30 )
   private var scaleVar = 1.0

   private val hyperCube = cursor.step { implicit tx => t.hyperCube }

   setPrefSz( 3 )

   def scale : Double = scaleVar
   def scale_=( factor: Double ) {
      scaleVar = factor
   }

   private def setPrefSz( lvl: Int ) {
      val w1   = ((hyperCube.extent.toLong << 1) * scale + 0.5).toInt + 1
      val in   = getInsets
      setPreferredSize( new Dimension( ((w1 + 16) * lvl - 16) + (in.left + in.right), w1 + (in.top + in.bottom) ))
   }

   def adjustPreferredSize() {
      setPrefSz( cursor.step { implicit tx => t.numLevels })
   }

   protected def draw( h: QuadView.PaintHelper ) {
      var (tr, n) = cursor.step { implicit tx => val res = t; (res, res.headTree) }
      val q = hyperCube
      val dx = ((q.extent.toLong << 1) * scale + 0.5).toInt + 16
      h.scale= scale
      while( n != null ) {
         draw( tr, h, n )
         h.translate( dx, 0 )
         n = cursor.step { implicit tx => n.nextOption.orNull }
      }
   }

   private def draw( tr: DeterministicSkipOctree[ S, IntSpace.TwoDim, A ],
                     h: QuadView.PaintHelper, quad: DeterministicSkipOctree[ S, IntSpace.TwoDim, A ]#Child ) {
      quad match {
         case l: tr.Leaf =>
            h.drawPoint( pointView( l.value ), highlight.contains( l.value ))
         case n: tr.Branch =>
            for( idx <- 0 until 4 ) {
               h.drawFrame( n.hyperCube.orthant( idx ), gridColor )
               draw( tr, h, cursor.step { implicit tx => n.child( idx )})
            }
         case _ =>
      }
   }
}