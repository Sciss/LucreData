/*
 *  LongRectangle.scala
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

package de.sciss.collection.geom

/**
 * A 2D rectangular query shape.
 */
trait LongRectangleLike extends QueryShape[ BigInt, LongSpace.TwoDim ] {
   import LongSpace.TwoDim._
   import Space.bigZero

   def left: Long
   def top: Long
   def width: Long
   def height: Long

   final def bottom : Long  = top + (height - 1)
   final def right : Long   = left + (width - 1)

   def contains( point: PointLike ) : Boolean = {
      val px = point.x
      val py = point.y
      (left <= px) && (right >= px) && (top <= py) && (bottom >= py)
   }

   final def overlapArea( q: HyperCube ) : BigInt = {
      val l = math.max( q.left, left )
      val r = math.min( q.right, right )
      val w = r - l + 1
      if( w <= 0L ) return bigZero
      val t = math.max( q.top, top )
      val b = math.min( q.bottom, bottom )
      val h = b - t + 1
      if( h <= 0L ) return bigZero
      BigInt( w ) * BigInt( h )
   }

   final def isAreaGreater( a: HyperCube, b: BigInt ) : Boolean = a.area > b

   def isAreaNonEmpty( area: BigInt ) : Boolean = area > bigZero
}
final case class LongRectangle( left: Long, top: Long, width: Long, height: Long ) extends LongRectangleLike
