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
import scala.collection.mutable.{Stack => MStack}
import sys.error

object RandomizedSkipQuadTree {
   def empty[ V ]( quad: Quad ) : RandomizedSkipQuadTree[ V ] = TreeImpl[ V ]( quad )

   def apply[ V ]( quad: Quad )( xs: (Point, V)* ) : RandomizedSkipQuadTree[ V ] = {
      val t = TreeImpl[ V ]( quad )
      xs.foreach( t.+=( _ ))
      t
   }

   private def unsupportedOp : Nothing       = error( "Operation not supported" )

   private object TreeImpl {
      def apply[ V ]( quad: Quad ) = new TreeImpl[ V ]( quad )
   }
   private final class TreeImpl[ V ]( _quad: Quad  ) extends RandomizedSkipQuadTree[ V ] {
      val headTree         = Node( _quad, null, null )()
      private var tailVar  = headTree

      def lastTree: QNode = tailVar

      // ---- map support ----

      def +=( kv: (Point, V) ) : this.type = {
         val point   = kv._1
         val value   = kv._2
         require( _quad.contains( point ), point.toString + " lies out of root square " + _quad )

         val ns      = MStack.empty[ Node ]
         tailVar.findP0( point, ns )
         var coin    = true
         var n: Node = null
         var pr: Node= null
         while( ns.nonEmpty ) {
            n        = ns.pop
            if( coin ) pr = n.insert( point, value, pr )
            coin &= flipCoin
         }
         while( coin ) {
            n        = Node( _quad, null, n )()
            pr       = n.insert( point, value, pr )
            tailVar  = n
            coin    &= flipCoin
         }
         this
      }

      override def contains( point: Point ) : Boolean = tailVar.findLeaf( point ) != null
      override def apply( point: Point ) : V = {
         val leaf = tailVar.findLeaf( point )
         if( leaf == null ) throw new java.util.NoSuchElementException( "key not found: " + point )
         leaf.value
      }
      def get( point: Point ) : Option[ V ] = {
         val leaf = tailVar.findLeaf( point )
         if( leaf == null ) None else Some( leaf.value )
      }
      def -=( point: Point ) : this.type = {
         error( "Not yet implemented" )
      }

      def iterator = new Iterator[ (Point, V) ] {
         val stack   = MStack.empty[ (Node, Int) ]
         var n       = headTree
         var leaf: Leaf = _
         var idx     = 0
         var hasNext = true

         prepareNext

         def prepareNext {
            while( true ) {
               if( idx > 4 ) {
                  if( stack.isEmpty ) {
                     hasNext = false
                     return
                  }
                  val (pn, pidx) = stack.pop
                  n  = pn
                  idx= pidx
               }
               n.child( idx ) match {
                  case l: Leaf =>
                     leaf  = l
                     idx  += 1
                     return
                  case Empty =>
                     idx  += 1
                  case n2: Node =>
                     stack.push( n -> (idx + 1) )
                     n     = n2
                     idx   = 0
               }
            }
         }

         def next : (Point, V) = {
            require( hasNext, "Iterator exhausted" )
            val res = (leaf.point, leaf.value)
            prepareNext
            res
         }
      }

      def rangeQuery( qs: QueryShape ) : Iterator[ V ] = new RangeQuery( qs )

      private class RangeQuery( qs: QueryShape ) extends Iterator[ V ] {
//         headTree.findMaxNonCriticial( qs )

         def next : V = error( "TODO" )
         def hasNext : Boolean = error( "TODO" )
      }

      sealed trait Child extends Q {
//         def asNode : Node
      }
      case object Empty extends Child with QEmpty {
//         def asNode : Node = unsupportedOp
      }
      final case class Leaf( point: Point, value: V ) extends Child with QLeaf {
//         def asNode : Node = unsupportedOp
      }
      final case class Node( quad: Quad, var parent: Node, prev: Node )( quads: Array[ Child ] = new Array[ Child ]( 4 ))
      extends Child with QNode {
         var next: Node = null

         // fix null squares and link
         {
            var i = 0; while( i < 4 ) {
               if( quads( i ) == null ) quads( i ) = Empty
            i += 1 }

            if( prev != null ) prev.next = this
         }


//         def findCriticial( qs: QueryShape, a: Long, qsa: Long ) : Node = {
////            val a = qs.overlapArea( quad )
//            var i = 0; while( i < 4 ) {
//               child( i ) match {
//                  case n: Node =>
//                     val a2 = qs.overlapArea( n.quad )
//                     if( a2 == a ) { // this renders 'this' uncritical
//                        return n.findCriticial( qs, a2, qsa )
//                     }
//                  case _ =>
//               }
//            i += 1 }
//            this
//         }

         def child( idx: Int ) : Child = quads( idx )

         def prevOption = Option( prev: QNode ) // Option( null ) becomes None
         def nextOption = Option( next: QNode ) // Option( null ) becomes None

         def findP0( point: Point, ns: MStack[ Node ]) {
            val qidx = quadIdx( quad, point )
            quads( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point, ns )
               case _ =>
                  ns.push( this )
                  if( prev != null ) prev.findP0( point, ns )
            }
         }

         def findLeaf( point: Point ) : Leaf = {
            val qidx = quadIdx( quad, point )
            quads( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
               case l: Leaf if( l.point == point ) => l
               case _ => if( prev == null ) null else prev.findLeaf( point )
            }
         }

         def findSameSquare( iq: Quad ) : Node = if( quad == iq ) this else parent.findSameSquare( iq )

         def insert( point: Point, value: V, prevP: Node ) : Node = {
            val qidx = quadIdx( quad, point )
            val l    = Leaf( point, value )
            quads( qidx ) match {
               case Empty =>
                  quads( qidx ) = l
                  this

               case t: Node =>
                  val tq      = t.quad
                  assert( !tq.contains( point ))
                  val te      = tq.extent
                  val iq      = gisqr( qidx, tq.cx - te, tq.cy - te, te << 1, point )
                  val iquads  = new Array[ Child ]( 4 )
                  val tidx    = quadIdx( iq, tq )
                  iquads( tidx ) = t
                  val pidx    = quadIdx( iq, point )
                  iquads( pidx ) = l
                  val qpred   = if( prevP == null ) null else prevP.findSameSquare( iq )
                  val q       = Node( iq, this, qpred )( iquads )
                  t.parent    = q
                  quads( qidx ) = q
                  q

               case l2 @ Leaf( point2, _ ) =>
                  if( point == point2 ) {
                     quads( qidx ) = l
                     this
                  } else {
                     val iq      = gisqr( qidx, point2.x, point2.y, 1, point )
                     val iquads  = new Array[ Child ]( 4 )
                     val lidx    = quadIdx( iq, point2 )
                     iquads( lidx ) = l2
                     val pidx    = quadIdx( iq, point )
                     iquads( pidx ) = l
                     val qpred   = if( prevP == null ) null else prevP.findSameSquare( iq )
                     val q       = Node( iq, this, qpred )( iquads )
                     quads( qidx ) = q
                     q
                  }
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
trait RandomizedSkipQuadTree[ V ] extends SkipQuadTree[ V ]