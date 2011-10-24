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
 */

package de.sciss.collection.geom

import annotation.tailrec

//final case class Circle( cx: Int, cy: Int, radius: Int ) extends QueryShape2D {
////   import QueryShape2D._
//
//   def contains( p: Point2D ) : Boolean = {
//      val dx   = cx.toLong - p.x.toLong // (cx - p.x).toLong
//      val dy   = cy.toLong - p.y.toLong // (cy - p.y).toLong
//      val rd   = radius.toLong
//      dx * dx + dy * dy < rd * rd
//   }
//
//   /*
//    * (cf. http://paulbourke.net/geometry/sphereline/)
//    */
//   def overlapArea( q: Quad ) : Long = error( "TODO" )
//   def area : Long = {
//      val rd = radius.toLong
//      (math.Pi * (rd * rd) + 0.5).toLong
//   }
//}

/**
 * XXX TODO: not possible to have a side length of 1, which might
 * be(come) a problem.
 */
object Quad2D {
   // http://stackoverflow.com/questions/6156502/integer-in-an-interval-with-maximized-number-of-trailing-zero-bits
   @tailrec private def binSplit( a: Int, b: Int, mask: Int = 0xFFFF0000, shift: Int = 8 ): Int = {
      val gt = a > (b & mask)
      if( shift == 0 ) {
         if( gt ) mask >> 1 else mask
      } else {
        binSplit( a, b, if( gt ) mask >> shift else mask << shift, shift >> 1 )
      }
   }
}

trait Quad2DLike extends QuadLike[ Space.TwoDim ] with Rectangle2DLike {
   def cx: Int
   def cy: Int

//   def greatestInteresting( aleft: Int, atop: Int, asize: Int, b: Point2DLike ) : Quad2DLike
}

final case class Quad2D( cx: Int, cy: Int, extent: Int ) extends Quad2DLike {
   import Quad2D._

   def quadrant( idx: Int ) : Quad2DLike = {
      val e = extent >> 1
      idx match {
         case 0 => Quad2D( cx + e, cy - e, e ) // ne
         case 1 => Quad2D( cx - e, cy - e, e ) // nw
         case 2 => Quad2D( cx - e, cy + e, e ) // sw
         case 3 => Quad2D( cx + e, cy + e, e ) // se
         case _ => throw new IllegalArgumentException( idx.toString )
      }
   }

   def top : Int     = cy - extent
   def left : Int    = cx - extent

//   def width : Int   = extent << 1
//   def height : Int  = extent << 1

   /**
    * The bottom is defined as the center y coordinate plus
    * the extent minus one, it thus designed the 'last pixel'
    * still inside the quad. This was changed from the previous
    * definition of 'cy + extent' to be able to use the full
    * 31 bit signed int space for a quad without resorting
    * to long conversion.
    */
   override def bottom : Int  = cy + (extent - 1)
   /**
    * The right is defined as the center x coordinate plus
    * the extent minus one, it thus designed the 'last pixel'
    * still inside the quad. This was changed from the previous
    * definition of 'cx + extent' to be able to use the full
    * 31 bit signed int space for a quad without resorting
    * to long conversion.
    */
   override def right : Int   = cx + (extent - 1)

   /**
    * The side length is two times the extent.
    */
   def side : Int    = extent << 1

   def contains( point: Point2DLike ) : Boolean = {
      val px = point.x
      val py = point.y
      (left <= px) && (right >= px) && (top <= py) && (bottom >= py)
   }

   /**
    * Checks whether a given quad is fully contained in this quad.
    * This is also the case if their bounds full match.
    */
   def contains( quad: Quad2DLike ) : Boolean =
      quad.left >= left && quad.top >= top && quad.right <= right && quad.bottom <= bottom

   def area : Long = {
      val sd = side.toLong
      sd * sd
   }

   def overlapArea( q: Quad2DLike ) : Long = {
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

   /**
    * Calculates the minimum distance to a point in the euclidean metric.
    * This calls `minDistanceSq` and then takes the square root.
    */
   def minDistance( point: Point2DLike ) : Double = math.sqrt( minDistanceSq( point ))

   /**
    * Calculates the maximum distance to a point in the euclidean metric.
    * This calls `maxDistanceSq` and then takes the square root.
    */
   def maxDistance( point: Point2DLike ) : Double = math.sqrt( maxDistanceSq( point ))

   /**
    * The squared (euclidean) distance of the closest of the quad's corners
    * to the point, if the point is outside the quad,
    * or `0L`, if the point is contained
    */
   def minDistanceSq( point: Point2DLike ) : Long = {
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

   /**
    * Calculates the maximum squared distance to a point in the euclidean metric.
    * This is the distance (squared) to the corner which is the furthest from
    * the `point`, no matter if it lies within the quad or not.
    */
   def maxDistanceSq( point: Point2DLike ) : Long = {
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
   def indexOf( a: Point2DLike ) : Int = {
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
   def indexOf( aq: Quad2DLike ) : Int = {
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

   def greatestInteresting( a: Point2DLike, b: Point2DLike ) : Quad2DLike =
      gi( a.x, a.y, 1, b )

   def greatestInteresting( a: Quad2DLike, b: Point2DLike ) : Quad2DLike =
      gi( a.left, a.top, a.side, b )

   private def gi( aleft: Int, atop: Int, asize: Int, b: Point2DLike ) : Quad2DLike = {
      val tlx           = left   // pq.cx - pq.extent
      val tly           = top    // pq.cy - pq.extent
      val akx           = aleft - tlx
      val aky           = atop  - tly
      val bkx           = b.x - tlx
      val bky           = b.y - tly
      // XXX TODO : Tuple3 is not specialized
      val (x0, x1, x2)  = if( akx <= bkx ) (akx, akx + asize, bkx) else (bkx, bkx + 1, akx )
      val (y0, y1, y2)  = if( aky <= bky ) (aky, aky + asize, bky) else (bky, bky + 1, aky )
      val mx            = binSplit( x1, x2 )
      val my            = binSplit( y1, y2 )
      // that means the x extent is greater (x grid more coarse).
      if( mx <= my ) {
         Quad2D( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )
      } else {
         Quad2D( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )
      }
   }
}
