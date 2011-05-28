/*
 *  CompressedQuadTree.scala
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

package de.sciss.tree

import annotation.tailrec
import sys.error  // suckers

object CompressedQuadTree {
   def apply[ V ]( quad: Quad ) : QNode[ V ] = {
      val quads = new Array[ Q[ V ]]( 4 )
//      createEmptyQuads( quad, quads )
      new NodeImpl[ V ]( quad, quads )
   }

   sealed trait Q[ +V ] {
//      def quad: Quad
   }
   case object QEmpty extends Q[ Nothing ] // [ V ]() /* ( quad: Quad ) */ extends Q[ V ]
   final case class QLeaf[ V ]( /* quad: Quad, */ point: Point, value: V ) extends Q[ V ]
   sealed trait QNode[ V ] extends Q[ V ] {
      def insert( point: Point, value: V ) : Unit
      def quad: Quad
   }

//   private def createEmptyQuads[ V ]( quad: Quad, arr: Array[ Q[ V ]]) {
//      var i = 0; while( i < 4 ) {
//         if( arr( i ) == null ) arr( i ) = QEmpty( quad.quadrant( i ))
//      i += 1 }
//   }

   private class NodeImpl[ V ]( val quad: Quad, val quads: Array[ Q[ V ]])
   extends QNode[ V ] {
      // fix null squares
      {
         var i = 0; while( i < 4 ) {
            if( quads( i ) == null ) quads( i ) = QEmpty
         i += 1 }
      }

      def insert( point: Point, value: V ) {
         val qidx = quadIdx( quad, point )
         require( qidx >= 0, point.toString + " lies outside of root square " + quad )
         quads( qidx ) = quads( qidx ) match {
            case QEmpty => QLeaf( /* eq, */ point, value )
            case t: QNode[ _ ] =>
               val tq      = t.quad
               val te      = tq.extent
               val iq      = gisqr( qidx, tq.cx - te, tq.cy - te, te << 1, point )
               val iquads  = new Array[ Q[ V ]]( 4 )
               val tidx    = quadIdx( iq, tq )
               iquads( tidx ) = t // l.copy( quad = iq.quadrant( lidx ))
               val pidx    = quadIdx( iq, point )
               iquads( pidx ) = QLeaf( /* iq.quadrant( pidx ), */ point, value )
//               createEmptyQuads( iq, iquads )
               new NodeImpl[ V ]( iq, iquads )

            /*
               "If the quadrant of p(x) that x is inserted into already contains a point y or
               an interesting square r, then we insert to Q a new interesting square q ⊂ p
               that contains both x and y (or r) but separates x and y (or r) into different
               quadrants of q."
             */
            case l @ QLeaf( /* lq, */ point2, value2 ) =>
               val iq      = gisqr( qidx, point2.x, point2.y, 1, point )
               val iquads  = new Array[ Q[ V ]]( 4 )
               val lidx    = quadIdx( iq, point2 )
               iquads( lidx ) = l // .copy( quad = iq.quadrant( lidx ))
               val pidx    = quadIdx( iq, point )
               iquads( pidx ) = QLeaf( /* iq.quadrant( pidx ), */ point, value )
//               createEmptyQuads( iq, iquads )
               new NodeImpl[ V ]( iq, iquads )
         }
      }

   //   private def interesting( point: Point ) : Quad[ V ] = {
   //
   //   }

   //   private def binFloor( i: Int ) : Int = if( i >= 0 ) {
   //      Integer.highestOneBit( i )
   //   } else {
   //      -Integer.highestOneBit( -i )
   //   }
   //
   //   private def binCeil( i: Int ) : Int = if( i >= 0 ) {
   //      val j = Integer.highestOneBit( i )
   //      if( j == i ) i else j << 1
   //   } else {
   //      val i0 = -i
   //      val j = Integer.highestOneBit( i0 )
   //      if( j == i0 ) i else -(j << 1)
   //   }

      /*
       * "Given a quadrant of a square p containing two points x and y, find the largest interesting square inside this quadrant."
       * (greatest interesting square)
       *
       * @return  a tuple consisting of `_1` the centre point, `_2` the extent of the greatest interesting square,
       *          `_3` the quadrant of `a`, and `_4` the quadrant of `b` in this interesting square.
       */
//      private def gisqr( pq: Quad, a: Point, b: Point ) : Quad = {
//         val tlx        = pq.cx - pq.extent
//         val tly        = pq.cy - pq.extent
//         val akx        = a.x - tlx
//         val aky        = a.y - tly
//         val bkx        = b.x - tlx
//         val bky        = b.y - tly
//         val (x1, x2)   = if( akx <= bkx ) (akx, bkx) else (bkx, akx )
//         val (y1, y2)   = if( aky <= bky ) (aky, bky) else (bky, aky )
//         val mx         = binSplit( x1 + 1, x2 )
//         val my         = binSplit( y1 + 1, y2 )
//         // that means the x extent is greater (x grid more coarse).
//         if( mx <= my ) {
//            Quad( tlx + (x2 & mx), tly + (y1 & mx) - mx, -mx )
//         } else {
//            Quad( tlx + (x1 & my) - my, tly + (y2 & my), -my )
//         }
//      }

      private def gisqr( pqidx: Int, aleft: Int, atop: Int, asize: Int,  b: Point ) : Quad = {
         val pq            = quad.quadrant( pqidx )
         val tlx           = pq.cx - pq.extent
         val tly           = pq.cy - pq.extent
         val akx           = aleft - tlx
         val aky           = atop  - tly
         val bkx           = b.x - tlx
         val bky           = b.y - tly
         val (x0, x1, x2)  = if( akx <= bkx ) (akx, akx + asize, bkx) else (bkx, bkx + 1, akx )
         val (y0, y1, y2)  = if( aky <= bky ) (aky, aky + asize, bky) else (bky, bky + 1, aky )
         val mx            = binSplit( x1, x2 )
         val my            = binSplit( y1, y2 )
         // that means the x extent is greater (x grid more coarse).
         if( mx <= my ) {
            Quad( tlx + (x2 & mx), tly + (y0 & mx) - mx, -mx )
         } else {
            Quad( tlx + (x0 & my) - my, tly + (y2 & my), -my )
         }
      }

      /**
       * Determines the quadrant index of a point `a` in a square `p` defined
       * by its center `pc` and extent `pe`.
       *
       * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `a` lies
       *          outside of `p`.
       */
      private def quadIdx( pq: Quad, a: Point ) : Int = {
         val cx   = pq.cx
         val cy   = pq.cy
         val e    = pq.extent
         val ax   = a.x
         val ay   = a.y
         if( ay < cy ) {      // north
            if( ax >= cx ) {  // east
               if( cx + e >  ax && cy - e <= ay ) 0 else -1   // ne
            } else {             // west
               if( cx - e <= ax && cy - e <= ay ) 1 else -2   // nw
            }
         } else {                // south
            if( ax < cx ) {   // west
               if( cx - e <= ax && cy + e >  ay ) 2 else -3   // sw
            } else {             // east
               if( cx + e >  ax && cy + e >  ay ) 3 else -4   // se
            }
         }
      }

      private def quadIdx( pq: Quad, aq: Quad ) : Int = {
         val cx      = pq.cx
         val cy      = pq.cy
         val e       = pq.extent
         val ae      = aq.extent
         val aleft   = aq.cx - ae
         val atop    = aq.cy - ae
         val aright  = aq.cx + ae
         val abottom = aq.cy + ae
         if( atop < cy ) {       // north
            if( cy - e <= atop && abottom <= cy ) {
               if( aleft >= cx ) {  // east
                  if( cx + e >= aright ) 0 else -1  // ne
               } else {             // west
                  if( cx - e <= aleft && aright <= cx ) 1 else -1  // nw
               }
            } else -1
         } else {                // south
            if( cy + e >= abottom && atop >= cy ) {
               if( aleft < cx ) {   // west
                  if( cx - e <= aleft && aright <= cx ) 2 else -1   // sw
               } else {             // east
                  if( cx + e >= aright ) 3 else -1    // se
               }
            } else - 1
         }
      }

      /**
       * Determines the quadrant index of a point `a` in a square `p` defined
       * by its center `pc`
       *
       * @return  the index of the quadrant (beginning at 0)
       */
      private def quadIdx( pc: Point, a: Point ) : Int = {
         if( a.y < pc.y ) {      // north
            if( a.x >= pc.x ) 0  // ne
            else 1               // nw
         } else {                // south
            if( a.x < pc.x ) 2   // sw
            else 3               // se
         }
      }

   //   @tailrec private def binSplit( a: Int, b: Int, mask: Int = 0xFFFFFFFF ): Int = {
   //     val mask2 = mask << 1
   //     if( a > (b & mask2) ) mask // (b & mask, -mask)
   //     else binSplit( a, b, mask2 )
   //   }

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
}