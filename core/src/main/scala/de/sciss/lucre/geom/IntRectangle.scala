/*
 *  IntRectangle.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.lucre
package geom

/**
 * A 2D rectangular query shape.
 */
trait IntRectangleLike extends QueryShape[ Long, IntSpace.TwoDim ] {
   import IntSpace.TwoDim._

   def left: Int
   def top: Int
   def width: Int
   def height: Int

   final def bottom : Int  = top + (height - 1)
   final def right : Int   = left + (width - 1)

   def contains( point: PointLike ) : Boolean = {
      val px = point.x
      val py = point.y
      (left <= px) && (right >= px) && (top <= py) && (bottom >= py)
   }

   final def overlapArea( q: HyperCube ) : Long = {
      val l = math.max( q.left, left ).toLong
      val r = math.min( q.right, right ).toLong
      val w = r - l + 1
      if( w <= 0L ) return 0L
      val t = math.max( q.top, top ).toLong
      val b = math.min( q.bottom, bottom ).toLong
      val h = b - t + 1
      if( h <= 0L ) return 0L
      w * h
   }

   final def isAreaGreater( a: HyperCube, b: Long ) : Boolean = a.area > b

   def isAreaNonEmpty( area: Long ) : Boolean = area > 0L
}
final case class IntRectangle( left: Int, top: Int, width: Int, height: Int ) extends IntRectangleLike
