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

import sys.error

//object QueryShape {
//   sealed trait Overlap
//   case object In extends Overlap
//   case object Out extends Overlap
//   case object Stabbing extends Overlap
//}
trait QueryShape {
//   import QueryShape._

//   /**
//    * Queries the overlap of this shape
//    * with a given `Quad q`
//    *
//    * @return  `Out` if `q` lies outside of this shape,
//    *           `Stabbing` if `q` stabs this shape (there is
//    *           and intersection, but it is smaller than `q`;
//    *           this includes the case that this shape is
//    *           completely contained in `q`)
//    *           `In` if `q` is completely contained
//    *           inside this shape
//    */
//   def overlapType( q: Quad ) : Overlap

   def overlapArea( q: Quad ) : Long
   def area : Long

   /**
    * Queries the overlap of this shape with a given
    * `Point p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
   def contains( p: Point ) : Boolean
}

final case class Circle( cx: Int, cy: Int, radius: Int ) extends QueryShape {
//   import QueryShape._

   def contains( p: Point ) : Boolean = {
      val dx   = (cx - p.x).toLong
      val dy   = (cy - p.y).toLong
      val rd   = radius.toLong
      dx * dx + dy * dy < rd * rd
   }

   /*
    * (cf. http://paulbourke.net/geometry/sphereline/)
    */
   def overlapArea( q: Quad ) : Long = error( "TODO" )
   def area : Long = {
      val rd = radius.toLong
      (math.Pi * (rd * rd) + 0.5).toLong
   }
}

final case class Quad( cx: Int, cy: Int, extent: Int ) extends QueryShape {
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

   def top : Int     = cy - extent
   def left : Int    = cx - extent
   def bottom : Int  = cy + extent
   def right : Int   = cx + extent

   def side : Int    = extent << 1

   def contains( point: Point ) : Boolean = {
      val px = point.x
      val py = point.y
      (cx - extent <= px) && (cx + extent > px) && (cy - extent <= py) && (cy + extent > py)
   }

   def area : Long = {
      val sd = side.toLong
      sd * sd
   }

   def overlapArea( q: Quad ) : Long = {
      val l = math.max( q.left, left )
      val r = math.min( q.right, right )
      val w = r - l
      if( w <= 0 ) return 0L
      val t = math.max( q.top, top )
      val b = math.min( q.bottom, bottom )
      val h = b - t
      if( h <= 0 ) return 0L
      w.toLong * h.toLong
   }

   def closestDistance( point: Point ) : Double = math.sqrt( closestDistanceSq( point ))

   /**
    * Returns the orientation of the point wrt the quad, according
    * to the following scheme:
    *
    *   5   4    7
    *     +---+
    *   1 | 0 |  3
    *     +---+
    *  13  12   15
    *
    *  Therefore the horizontal orientation can be extracted
    *  with `_ & 3`, and the vertical orientation with `_ >> 2`,
    *  where orientation is 0 for 'parallel', 1 for 'before' and
    *  '3' for 'after', so that if the orient is before or
    *  after, the sign can be retrieved via `_ - 2`
    */
//   def orient( point: Point ) : Int = {
//
//   }

   /**
    * The squared (euclidean) distance of the closest of the quad's corners
    * to the point, if the point is outside the quad,
    * or `0L`, if the point is contained
    */
   def closestDistanceSq( point: Point ) /* ( orient: Int = orient( point )) */ : Long = {
      val px   = point.x
      val py   = point.y
      val l    = left
      val t    = top
      if( px < l ) {
         val dx   = (l - px).toLong
         val dxs  = dx * dx
         if( py < t ) {
            val dy = (t - py).toLong
            return dxs + dy * dy
         }
         val b = bottom
         if( py >= b ) {
            val dy = (py - b - 1).toLong
            return dxs + dy * dy
         }
         return dxs
      }
      val r = right
      if( px >= r ) {
         val dx   = (px - r).toLong
         val dxs  = dx * dx
         if( py < t ) {
            val dy = (t - py).toLong
            return dxs + dy * dy
         }
         val b = bottom
         if( py >= b ) {
            val dy = (py - b - 1).toLong
            return dxs + dy * dy
         }
         return dxs
      }
      if( py < t ) {
         val dy   = (t - py).toLong
         val dys  = dy * dy
         if( px < l ) {
            val dx = (l - px).toLong
            return dx * dx + dys
         }
         val r = right
         if( px >= r ) {
            val dx = (px - r - 1).toLong
            return dx * dx + dys
         }
         return dys
      }
      val b = bottom
      if( py >= b ) {
         val dy   = (py - b - 1).toLong
         val dys  = dy * dy
         if( px < l ) {
            val dx = (l - px).toLong
            return dx * dx + dys
         }
         val r = right
         if( px >= r ) {
            val dx = (px - r - 1).toLong
            return dx * dx + dys
         }
         return dys
      }
      // if we get here, the point is inside the quad
      0L
   }
}

final case class Point( x: Int, y: Int ) {
   def +( p: Point ) = Point( x + p.x, y + p.y )
   def -( p: Point ) = Point( x - p.x, y - p.y )

   def distanceSq( that: Point ) : Long = {
      val dx = (that.x - x).toLong
      val dy = (that.y - y).toLong
      dx * dx + dy * dy
   }
}