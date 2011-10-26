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

trait SquareLike extends HyperCube[ Space.TwoDim ] /* with RectangleLike */ with QueryShape[ Space.TwoDim ] {
   /**
    * X coordinate of the square's center
    */
   def cx: Int

   /**
    * Y coordinate of the square's center
    */
   def cy: Int

   /**
    * The extent is the half side length of the square
    */
   def extent: Int

   def top : Int
   def left : Int
   def right : Int
   def bottom : Int

//   def greatestInteresting( aleft: Int, atop: Int, asize: Int, b: Point2DLike ) : SquareLike
}

final case class Square( cx: Int, cy: Int, extent: Int ) extends SquareLike {
   def orthant( idx: Int ) : SquareLike = {
      val e = extent >> 1
      idx match {
         case 0 => Square( cx + e, cy - e, e ) // ne
         case 1 => Square( cx - e, cy - e, e ) // nw
         case 2 => Square( cx - e, cy + e, e ) // sw
         case 3 => Square( cx + e, cy + e, e ) // se
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
    * still inside the square. This was changed from the previous
    * definition of 'cy + extent' to be able to use the full
    * 31 bit signed int space for a square without resorting
    * to long conversion.
    */
   override def bottom : Int  = cy + (extent - 1)
   /**
    * The right is defined as the center x coordinate plus
    * the extent minus one, it thus designed the 'last pixel'
    * still inside the square. This was changed from the previous
    * definition of 'cx + extent' to be able to use the full
    * 31 bit signed int space for a square without resorting
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
    * Checks whether a given square is fully contained in this square.
    * This is also the case if their bounds full match.
    */
   def contains( quad: SquareLike ) : Boolean =
      quad.left >= left && quad.top >= top && quad.right <= right && quad.bottom <= bottom

   def area : Long = {
      val sd = side.toLong
      sd * sd
   }

   def overlapArea( q: SquareLike ) : Long = {
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
    * The squared (euclidean) distance of the closest of the square's corners
    * or sides to the point, if the point is outside the square,
    * or zero, if the point is contained
    */
   def minDistanceSq( point: Point2DLike ) : Long = {
      val ax   = point.x
      val ay   = point.y
      val em1  = extent - 1

      val dx   = if( ax < cx ) {
         val xmin = cx - extent
         if( ax < xmin ) xmin - ax else 0
      } else {
         val xmax = cx + em1
         if( ax > xmax ) ax - xmax else 0
      }

      val dy   = if( ay < cy ) {
         val ymin = cy - extent
         if( ay < ymin ) ymin - ay else 0
      } else {
         val ymax = cy + em1
         if( ay > ymax ) ay - ymax else 0
      }

      if( dx == 0 && dy == 0 ) 0L else {
         val dxl = dx.toLong
         val dyl = dy.toLong
         dxl * dxl + dyl * dyl
      }
   }

   /**
    * Calculates the maximum squared distance to a point in the euclidean metric.
    * This is the distance (squared) to the corner which is the furthest from
    * the `point`, no matter if it lies within the square or not.
    */
   def maxDistanceSq( point: Point2DLike ) : Long = {
      val ax   = point.x
      val ay   = point.y
      val em1  = extent - 1
      val axl  = ax.toLong
      val ayl  = ay.toLong

      val dx   = if( ax < cx ) {
         (cx + em1).toLong - axl
      } else {
         axl - (cx - extent).toLong
      }

      val dy   = if( ay < cy ) {
         (cy + em1).toLong - ayl
      } else {
         ayl - (cy - extent).toLong
      }

      dx * dx + dy * dy
   }

   /**
    * Determines the quadrant index of a point `a`.
    *
    * @return  the index of the quadrant (beginning at 0), or -1 if `a` lies
    *          outside of this square.
    */
   def indexOf( a: Point2DLike ) : Int = {
      val ax   = a.x
      val ay   = a.y
      if( ay < cy ) {      // north
         if( ax >= cx ) {  // east
            if( right >= ax && top <= ay ) 0 else -1   // ne
         } else {             // west
            if( left <= ax && top <= ay ) 1 else -1 // -2   // nw
         }
      } else {                // south
         if( ax < cx ) {   // west
            if( left <= ax && bottom >= ay ) 2 else -1 // -3   // sw
         } else {             // east
            if( right >= ax && bottom >= ay ) 3 else -1 // -4   // se
         }
      }
   }

   /**
    * Determines the quadrant index of another internal square `aq`.
    *
    * @return  the index of the quadrant (beginning at 0), or -1 if `aq` lies
    *          outside of this square.
    */
   def indexOf( aq: SquareLike ) : Int = {
      val atop = aq.top
      if( atop < cy ) {       // north
         if( top <= atop && aq.bottom < cy ) {
            val aleft = aq.left
            if( aleft >= cx ) {  // east
               if( right >= aq.right ) 0 else -1  // ne
            } else {             // west
               if( left <= aleft && aq.right < cx ) 1 else -1  // nw
            }
         } else -1
      } else {                // south
         if( bottom >= aq.bottom && atop >= cy ) {
            val aleft = aq.left
            if( aleft < cx ) {   // west
               if( left <= aleft && aq.right < cx ) 2 else -1   // sw
            } else {             // east
               if( right >= aq.right ) 3 else -1    // se
            }
         } else -1
      }
   }

   def greatestInteresting( a: Point2DLike, b: Point2DLike ) : SquareLike =
      gi( a.x, a.y, 1, b )

   def greatestInteresting( a: SquareLike, b: Point2DLike ) : SquareLike =
      gi( a.left, a.top, a.extent << 1, b )  // a.extent << 1 can exceed 31 bit -- but it seems to work :-/

   private def gi( aleft: Int, atop: Int, asize: Int, b: Point2DLike ) : SquareLike = {
      val tlx = cx - extent
      val tly = cy - extent
      val akx = aleft - tlx
      val aky = atop - tly
      val bkx = b.x - tlx
      val bky = b.y - tly

      var x0 = 0
      var x1 = 0
      var x2 = 0
      if( akx <= bkx ) {
         x0 = akx
         x1 = akx + asize
         x2 = bkx
      } else {
         x0 = bkx
         x1 = bkx + 1
         x2 = akx
      }
      val mx = HyperCube.binSplit( x1, x2 )

      var y0 = 0
      var y1 = 0
      var y2 = 0
      if( aky <= bky ) {
         y0 = aky
         y1 = aky + asize
         y2 = bky
      } else {
         y0 = bky
         y1 = bky + 1
         y2 = aky
      }
      val my = HyperCube.binSplit( y1, y2 )

      // that means the x extent is greater (x grid more coarse).
      if( mx <= my ) {
         Square( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )
      } else {
         Square( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )
      }
   }
}
