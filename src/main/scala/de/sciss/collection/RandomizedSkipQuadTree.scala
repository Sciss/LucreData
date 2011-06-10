/*
 *  RandomizedSkipQuadTree.scala
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

import annotation.tailrec
import sys.error  // suckers

object RandomizedSkipQuadTree {
//   def apply[ V ]( quad: Quad ) : RandomizedSkipQuadTree[ V ] = TreeImpl[ V ]( quad )

   def apply[ V ]( quad: Quad, xs: (Point, V)* ) : RandomizedSkipQuadTree[ V ] = {
      val t = TreeImpl[ V ]( quad )
      xs.foreach( t.+=( _ ))
      t
   }

   private object TreeImpl {
      def apply[ V ]( quad: Quad ) = new TreeImpl[ V ]( quad )
   }
   private final class TreeImpl[ V ]( _quad: Quad  ) extends RandomizedSkipQuadTree[ V ] {
      val headTree         = Node( _quad, None )()
      private var tailVar  = headTree

      def lastTree: QNode = tailVar

      // ---- map support ----

      def +=( kv: (Point, V) ) : this.type = {
         val point   = kv._1
         val value   = kv._2
         val qpred   = tailVar.insertStep( point, value )
         if( qpred.nonEmpty && flipCoin ) {
            val hq      = headTree.quad
            val qidx    = quadIdx( hq, point )
            val l       = Leaf( point, value )
            do {
               val quads      = new Array[ Child ]( 4 )
               quads( qidx )  = l
               tailVar        = Node( hq, Some( tailVar ))( quads )
            } while( flipCoin )
         }
         this
      }

      def get( point: Point ) : Option[ V ]  = error( "Not yet implemented" )
      def -=( point: Point ) : this.type     = error( "Not yet implemented" )
      def iterator : Iterator[ (Point, V) ]  = error( "Not yet implemented" )

      sealed trait Child extends Q
      case object Empty extends Child with QEmpty
      final case class Leaf( point: Point, value: V ) extends Child with QLeaf
      final case class Node( quad: Quad, prevOption: Option[ Node ])( quads: Array[ Child ] = new Array[ Child ]( 4 ))
      extends Child with QNode {
         // fix null squares
         {
            var i = 0; while( i < 4 ) {
               if( quads( i ) == null ) quads( i ) = Empty
            i += 1 }
         }

//      def isDefined = true
//      def map( fun: Node[ V ] => MaybeQNode[ V ]) : MaybeQNode[ V ] = fun( this )

         def child( idx: Int ) : Child = quads( idx )

         def insertStep( point: Point, value: V ) : Option[ Node ] = {
            val qidx = quadIdx( quad, point )
//         require( qidx >= 0, point.toString + " lies outside of root square " + quad )
            quads( qidx ) match {
               case Empty =>
                  if( prevOption.isEmpty || (prevOption.flatMap( _.insertStep( point, value )).nonEmpty && flipCoin) ) {
                     quads( qidx ) = Leaf( point, value )
                     Some( this )
                  } else None

               case t @ Node( tq, tpred ) =>
                  if( tq.contains( point )) {
                     t.insertStep( point, value )
                  } else {
                     val qpred = prevOption.flatMap( _.insertStep( point, value ))
                     if( prevOption.isEmpty || (qpred.nonEmpty && flipCoin) ) {
                        val te      = tq.extent
                        val iq      = gisqr( qidx, tq.cx - te, tq.cy - te, te << 1, point )
                        val iquads  = new Array[ Child ]( 4 )
                        val tidx    = quadIdx( iq, tq )
                        iquads( tidx ) = t
                        val pidx    = quadIdx( iq, point )
                        iquads( pidx ) = Leaf( point, value )
                        val q       = Node( iq, qpred )( iquads )
                        quads( qidx ) = q
                        Some( q )
                     } else None
                  }

               case l @ Leaf( point2, value2 ) =>
                  val qpred   = prevOption.flatMap( _.insertStep( point, value ))
                  if( prevOption.isEmpty || (qpred.nonEmpty && flipCoin) ) {
                     val iq      = gisqr( qidx, point2.x, point2.y, 1, point )
                     val iquads  = new Array[ Child ]( 4 )
                     val lidx    = quadIdx( iq, point2 )
                     iquads( lidx ) = l
                     val pidx    = quadIdx( iq, point )
                     iquads( pidx ) = Leaf( point, value )
                     val q       = Node( iq, qpred )( iquads )
                     quads( qidx ) = q
                     Some( q )
                  } else None
            }
         }

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
//            val cx = tlx + (x2 & mx)
//            val cy = tly + (y0 & mx) - mx
//            Quad( cx, cy, -mx )
//            Quad( tlx + (x2 & mx), tly + (y0 & mx) - mx, -mx )
               Quad( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )
            } else {
//            Quad( tlx + (x0 & my) - my, tly + (y2 & my), -my )
               Quad( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )
            }
         }
      }
   }

   private def flipCoin : Boolean = util.Random.nextBoolean()

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
trait RandomizedSkipQuadTree[ V ] extends SkipQuadTree[ V ] {
   def headTree: QNode
   def lastTree: QNode
}