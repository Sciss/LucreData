/*
 *  Point3D.scala
 *  (LucreData)
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

trait Point3DLike {
   import Space.ThreeDim._

   def x: Int
   def y: Int
   def z: Int

//   final def left             = x
//   final def top              = y
//   final override def right   = x
//   final override def bottom  = y

   def distanceSq( that: PointLike ) : BigInt = {
      val dx = that.x.toLong - x.toLong
      val dy = that.y.toLong - y.toLong
      val dz = that.z.toLong - z.toLong
      BigInt( dx * dx + dy * dy ) + BigInt( dz * dz )
   }

   // ---- QueryShape ----
//   final def overlapArea( q: Cube ) : Long = if( q.contains( this )) 1L else 0L
//   final def area : Long = 1L

   /**
    * Queries the overlap of this shape with a given
    * `Point3D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
//   final def contains( p: Point3DLike ) : Boolean = p.x == this.x && p.y == this.y && p.z == this.z

   /**
    * Returns the orientation of the given point wrt this point, according
    * to the following scheme:
    *
    *   5   4    6
    *     +---+
    *   1 | 0 |  2
    *     +---+
    *   9   8   10
    *
    *  Therefore the horizontal orientation can be extracted
    *  with `_ & 3`, and the vertical orientation with `_ >> 2`,
    *  where orientation is 0 for 'parallel', 1 for 'before' and
    *  '3' for 'after', so that if the orient is before or
    *  after, the sign can be retrieved via `_ - 2`
    *
    *  For example, if this is `Point3D(4, 4)` and the query
    *  point is `Point3D(4, 5)`, the result is `12`. If the
    *  query is `Point3D(0, 0)`, the result is `5`, etc.
    */
//   final def orient( b: Point3DLike ) : Int = {
//      sys.error( "TODO" )
////      val ax = x
////      val ay = y
////      val bx = b.x
////      val by = b.y
////      val dx = if( bx < ax ) 1 else if( bx > ax ) 2 else 0
////      val dy = if( by < ay ) 4 else if( by > ay ) 8 else 0
////      dx | dy
//   }
}

final case class Point3D( x: Int, y: Int, z: Int ) extends Point3DLike {
   def +( p: Point3D ) = Point3D( x + p.x, y + p.y, z + p.z )
   def -( p: Point3D ) = Point3D( x - p.x, y - p.y, z + p.z )
}
