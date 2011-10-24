/*
 *  Rectangle.scala
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
 */

package de.sciss.collection.geom

trait Rectangle2DLike extends QueryShape[ Dim.Two ] {
   def top : Int
   def left : Int
//   def width : Int
//   def height : Int
   def right : Int // = left + (width - 1)
   def bottom : Int // = top + (height - 1)

   final def topLeft       = Point2D( left, top )
   final def topRight      = Point2D( right, top )
   final def bottomLeft    = Point2D( left, bottom )
   final def bottomRight   = Point2D( right, bottom )
}

/**
 * Warning: We are currently allowing the rectangle to have a width or height of
 * `0x80000000`. If you use the corner methods `topRight`, `bottomLeft` or
 * `bottomRight`, these will still give you correct points (given that
 * `topLeft` is `Point2D(0,0)`)!
 */
final case class Rectangle2D( left: Int, top: Int, right: Int, bottom: Int ) extends Rectangle2DLike {
   def area : Long = {
      val width   = right.toLong  + 1 - left.toLong
      val height  = bottom.toLong + 1 - top.toLong
      width * height
   }
   def contains( p: Point2DLike ) : Boolean = {
      val px = p.x
      val py = p.y
      px >= left && px <= right && py >= top && py <= bottom
   }
   def overlapArea( q: Quad2DLike ) : Long = sys.error( "NOT YET IMPLEMENTED" )
}