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
      val dx   = cx.toLong - p.x.toLong // (cx - p.x).toLong
      val dy   = cy.toLong - p.y.toLong // (cy - p.y).toLong
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
//   def bottom : Int  = cy + extent
//   def right : Int   = cx + extent

   /**
    * The bottom is defined as the center y coordinate plus
    * the extent minus one, it thus designed the 'last pixel'
    * still inside the quad. This was changed from the previous
    * definition of 'cy + extent' to be able to use the full
    * 31 bit signed int space for a quad without resorting
    * to long conversion.
    */
   def bottom : Int  = cy + (extent - 1)
   /**
    * The right is defined as the center x coordinate plus
    * the extent minus one, it thus designed the 'last pixel'
    * still inside the quad. This was changed from the previous
    * definition of 'cx + extent' to be able to use the full
    * 31 bit signed int space for a quad without resorting
    * to long conversion.
    */
   def right : Int   = cx + (extent - 1)

   /**
    * The side length is two times the extent.
    */
   def side : Int    = extent << 1

   def contains( point: Point ) : Boolean = {
      val px = point.x
      val py = point.y
      (left <= px) && (right >= px) && (top <= py) && (bottom >= py)
   }

   /**
    * Checks whether a given quad is fully contained in this quad.
    * This is also the case if their bounds full match.
    */
   def contains( quad: Quad ) : Boolean =
      quad.left >= left && quad.top >= top && quad.right <= right && quad.bottom <= bottom

   def area : Long = {
      val sd = side.toLong
      sd * sd
   }

   def overlapArea( q: Quad ) : Long = {
      val l = math.max( q.left, left ).toLong
      val r = math.min( q.right, right ).toLong
      val w = r - l + 1 // (r - l).toLong + 1
      if( w <= 0L ) return 0L
      val t = math.max( q.top, top ).toLong
      val b = math.min( q.bottom, bottom ).toLong
      val h = b - t + 1 // (b - t).toLong + 1
      if( h <= 0L ) return 0L
      w * h
   }

   def closestDistance( point: Point ) : Double = math.sqrt( closestDistanceSq( point ))

//   /**
//    * Returns the orientation of the point wrt the quad, according
//    * to the following scheme:
//    *
//    *   5   4    7
//    *     +---+
//    *   1 | 0 |  3
//    *     +---+
//    *  13  12   15
//    *
//    *  Therefore the horizontal orientation can be extracted
//    *  with `_ & 3`, and the vertical orientation with `_ >> 2`,
//    *  where orientation is 0 for 'parallel', 1 for 'before' and
//    *  '3' for 'after', so that if the orient is before or
//    *  after, the sign can be retrieved via `_ - 2`
//    */
//   def orient( point: Point ) : Int = {
//
//   }

   def furthestDistanceSq( point: Point ) : Long = {
      val px   = point.x
      val py   = point.y
      if( px < cx ) {
         val dx   = right.toLong - px.toLong // (right - px).toLong
         val dxs  = dx * dx
         if( py < cy ) {   // bottom right is furthest
            val dy   = bottom.toLong - py.toLong // (bottom - py).toLong
            dxs + dy * dy
         } else {          // top right is furthest
            val dy   = py.toLong - top.toLong // (py - top).toLong
            dxs + dy * dy
         }
      } else {
         val dx   = px.toLong - left.toLong // (px - left).toLong
         val dxs  = dx * dx
         if( py < cy ) {   // bottom left is furthest
            val dy   = bottom.toLong - py.toLong // (bottom - py).toLong
            dxs + dy * dy
         } else {          // top left is furthest
            val dy   = py.toLong - top.toLong // (py - top).toLong
            dxs + dy * dy
         }
      }
   }

   /**
    * Determines the quadrant index of a point `a`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `a` lies
    *          outside of this quad.
    */
   def indexOf( a: Point ) : Int = {
      val ax   = a.x
      val ay   = a.y
      if( ay < cy ) {      // north
         if( ax >= cx ) {  // east
            if( right >= ax && top <= ay ) 0 else -1   // ne
         } else {             // west
            if( left <= ax && top <= ay ) 1 else -2   // nw
         }
      } else {                // south
         if( ax < cx ) {   // west
            if( left <= ax && bottom >= ay ) 2 else -3   // sw
         } else {             // east
            if( right >= ax && bottom >= ay ) 3 else -4   // se
         }
      }
   }

   /**
    * Determines the quadrant index of another internal quad `aq`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `aq` lies
    *          outside of this quad.
    */
   def indexOf( aq: Quad ) : Int = {
      val atop = aq.top
      if( atop < cy ) {       // north
         if( top <= atop && aq.bottom <= cy ) {
            val aleft = aq.left
            if( aleft >= cx ) {  // east
               if( right >= aq.right ) 0 else -1  // ne
            } else {             // west
               if( left <= aleft && aq.right <= cx ) 1 else -1  // nw
            }
         } else -1
      } else {                // south
         if( bottom >= aq.bottom && atop >= cy ) {
            val aleft = aq.left
            if( aleft < cx ) {   // west
               if( left <= aleft && aq.right <= cx ) 2 else -1   // sw
            } else {             // east
               if( right >= aq.right ) 3 else -1    // se
            }
         } else -1
      }
   }


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
         val dx   = l.toLong - px.toLong // (l - px).toLong
         val dxs  = dx * dx
         if( py < t ) {
            val dy = t.toLong - py.toLong // (t - py).toLong
            return dxs + dy * dy
         }
         val b = bottom
         if( py > b ) {
            val dy = py.toLong - b.toLong // (py - b).toLong
            return dxs + dy * dy
         }
         return dxs
      }
      val r = right
      if( px > r ) {
         val dx   = px.toLong - r.toLong // (px - r).toLong
         val dxs  = dx * dx
         if( py < t ) {
            val dy = t.toLong - py.toLong // (t - py).toLong
            return dxs + dy * dy
         }
         val b = bottom
         if( py > b ) {
            val dy = py.toLong - b.toLong // (py - b).toLong
            return dxs + dy * dy
         }
         return dxs
      }
      if( py < t ) {
         val dy   = t.toLong - py.toLong // (t - py).toLong
         val dys  = dy * dy
         if( px < l ) {
            val dx = l.toLong - px.toLong // (l - px).toLong
            return dx * dx + dys
         }
         val r = right
         if( px > r ) {
            val dx = px.toLong - r.toLong // (px - r).toLong
            return dx * dx + dys
         }
         return dys
      }
      val b = bottom
      if( py > b ) {
         val dy   = py.toLong - b.toLong // (py - b).toLong
         val dys  = dy * dy
         if( px < l ) {
            val dx = l.toLong - px.toLong // (l - px).toLong
            return dx * dx + dys
         }
         val r = right
         if( px > r ) {
            val dx = px.toLong - r.toLong // (px - r).toLong
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
      val dx = that.x.toLong - x.toLong // (that.x - x).toLong
      val dy = that.y.toLong - y.toLong // (that.y - y).toLong
      dx * dx + dy * dy
   }
}
