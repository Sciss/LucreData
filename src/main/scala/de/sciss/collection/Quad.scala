/*
 *  Quad.scala
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

//trait QuadLike {
//   def cx: Int
//   def cy: Int
//   def extent: Int
//   def center: Point = Point( cx, cy )
//   def topLeft: Point = {
//      Point( cx - extent, cy - extent )
//   }
//}

final case class Quad( cx: Int, cy: Int, extent: Int ) /* extends QuadLike */ {
   def quadrant( idx: Int ) : Quad = {
      val e = extent >> 1
      idx match {
         case 0 => Quad( cx + e, cy - e, e ) // ne
         case 1 => Quad( cx - e, cy - e, e ) // nw
         case 2 => Quad( cx - e, cy + e, e ) // sw
         case 3 => Quad( cx + e, cy + e, e ) // se
         case _ => throw new IllegalArgumentException( idx.toString )
      }
   }

   def x : Int = cx - extent
   def y : Int = cy - extent
   def side : Int = extent << 1

   def contains( point: Point ) : Boolean =
      (cx - extent <= point.x) && (cx + extent > point.x) && (cy - extent <= point.y) && (cy + extent > point.y)
}

final case class Point( x: Int, y: Int ) /* extends QuadLike */ {
//   def orthoDist( p: Point ) : Int = math.max( math.abs( x - p.x ), math.abs( y - p.y ))
   def +( p: Point ) = Point( x + p.x, y + p.y )
   def -( p: Point ) = Point( x - p.x, y - p.y )

//   def center = this
//   def extent = 1
}
