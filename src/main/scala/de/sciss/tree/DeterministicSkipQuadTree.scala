/*
 *  DeterministicSkipQuadTree.scala
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

import sys.error
import annotation.{switch, tailrec}

object DeterministicSkipQuadTree {
   def apply[ V ]( quad: Quad ) : DeterministicSkipQuadTree[ V ] = new TreeImpl[ V ]( quad )

   def fromMap[ V ]( quad: Quad, m: Map[ Point, V ]) : DeterministicSkipQuadTree[ V ] = {
      val t = new TreeImpl[ V ]( quad )
      m.foreach( t.+=( _ ))
      t
   }

   type InOrder = TotalOrder[ Unit ]

   private class TreeImpl[ V ]( _quad: Quad ) extends DeterministicSkipQuadTree[ V ] {
      private var tl: Node = {
         TopLeftNode( _quad )
      }
      val list: SkipList[ InOrder ] = HASkipList.empty( TotalOrder.max[ NonEmpty ], 2 ) // 2-5 DSL
      def skipList = list

      // ---- map support ----

      def +=( kv: (Point, V) ) : this.type = {
//         println( "insert : " + kv )
         +=( kv._1, kv._2 )
      }

      def +=( point: Point, value: V ) : this.type = {
//         val (point, value)   = kv
         val p0               = tl.findP0( point )
         val ordLeaf          = p0.insert( point, value )
//         assert( ordLeaf.elem.isInstanceOf[ Leaf ])
//println( "adding to skiplist : " + ordLeaf.elem )
         list.add( ordLeaf )
         this
      }

      def get( point: Point ) : Option[ V ] = {
         val p0   = tl.findP0( point )
         val c    = p0.children
         var i = 0; while( i < c.size ) {
            c( i ) match {
               case l: Leaf if( l.point == point ) => return Some( l.value )
               case _ =>
            }
         i += 1 }
         None
      }

      def -=( point: Point ) : this.type = error( "Not yet implemented" )

      def iterator = new Iterator[ (Point, V) ] {
         val underlying = list.iterator
         def next : (Point, V) = {
            // XXX ouch... the problem with the TotalOrder is
            // that it cannot be made variant because of the
            // double linking, thus the skip list cannot hold
            // TotalOrder[ Leaf ] but only TotalOrder[ NonEmpty ]
            // requiring this ugly cast :-(
//            val l = underlying.next.elem.asInstanceOf[ Leaf ]
//            (l.point, l.value)
//            val res =
               underlying.next.elem.asMapEntry
//println( "iter : " + res )
//            res
         }
         def hasNext : Boolean = underlying.hasNext
      }

      sealed trait Child {
         def insertOrderAfter( parent: Node, child: NonEmpty ) : InOrder
         def insertOrderBefore( parent: Node, child: NonEmpty ) : InOrder
      }

      case object Empty extends Child {
         def insertOrderBefore( parent: Node, child: NonEmpty ) : InOrder  = parent.insertOrderBefore( parent /* XXX ouch */, child )
         def insertOrderAfter( parent: Node, child: NonEmpty ) : InOrder   = parent.insertOrderAfter( parent /* XXX ouch */, child )
      }

      sealed trait NonEmpty extends Child {
         def union( mq: Quad, point: Point ) : Quad
         def quadIdxIn( iq: Quad ) : Int
         def asMapEntry : (Point, V)
      }

      type InOrder = TotalOrder[ NonEmpty ]

      sealed trait Leaf extends NonEmpty {
         def point : Point
         def value : V
         def asMapEntry : (Point, V) = (point, value)
         def union( mq: Quad, point2: Point ) = {
            val p = point
            interestingSquare( mq, p.x, p.y, 1, point2 )
         }
         def quadIdxIn( iq: Quad ) : Int = pointInQuad( iq, point )
      }

      sealed trait Node extends NonEmpty {
         def children : Array[ Child ]
         def findP0( point: Point ) : LeftNode
         def quad: Quad
         def asMapEntry : (Point, V) = unsupportedOp

         def union( mq: Quad, point2: Point ) = {
            val q = quad
            interestingSquare( mq, q.x, q.y, q.side, point2 )
         }
         def quadIdxIn( iq: Quad ) : Int = quadInQuad( iq, quad )
      }

      sealed trait RightNode extends Node {
         def prev : Node

         def findP0( point: Point ) : LeftNode = {
            val qidx = pointInQuad( quad, point )
            children( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => prev.findP0( point )
            }
         }
      }

//      sealed trait LeftChild {
////         def startOrder: InOrder
////         def stopOrder: InOrder
//      }

      sealed trait LeftNode extends Node {
//         def north: InOrder
//         def south: InOrder
//         override def children : Array[ LeftChild ]

         def startOrder: InOrder
         def stopOrder: InOrder

         def newLeaf( point: Point, value: V ) : Leaf
         def newNode( iq: Quad, old: NonEmpty, nu: Leaf ) : LeftNode

//         def order( child: NonEmpty ) : InOrder = {
//            val cidx = child.quadIdxIn( quad )
//            val hemi = if( cidx < 2 ) north else south
//println( "order " + child + " wrt " + this + " -> idx = " + cidx )
//            if( (cidx % 2) == 0 ) hemi.insertBefore( child ) else hemi.insertAfter( child )
//         }

         def insertOrderBefore( parent: Node, child: NonEmpty ) : InOrder  = startOrder.insertAfter( child )
         def insertOrderAfter( parent: Node, child: NonEmpty ) : InOrder   = stopOrder.insertBefore( child )

         def orderLeaf( qidx: Int, child: Leaf ) : InOrder = {
            (qidx: @switch) match {
               case 0 => startOrder.insertAfter( child )
               case 1 => children( 0 ).insertOrderAfter( this, child )
               case 2 => children( 3 ).insertOrderBefore( this, child )
               case 3 => stopOrder.insertBefore( child )
            }
         }

//         def orderStop( child: NonEmpty ) : InOrder = {
//            (child.quadIdxIn( quad ): @switch) match {
//               case 0 => startOrder.insertAfter( child )
//               case 1 => children( 0 ) match {
//                  case n: NonEmpty => n.stopOrder.insertAfter( child )
//                  case _ => startOrder.insertAfter( child )
//               }
//               case 2 => children( 3 ) match {
//                  case n: NonEmpty => n.startOrder.insertBefore( child )
//                  case _ => stopOrder.insertBefore( child )
//               }
//               case 3 => stopOrder.insertBefore( child )
//            }
//         }

         def findP0( point: Point ) : LeftNode = {
            val qidx = pointInQuad( quad, point )
            children( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => this
            }
         }

         def insert( point: Point, value: V ) : InOrder = {
            val qidx = pointInQuad( quad, point )
            val leaf = newLeaf( point, value )
            val c    = children
            c( qidx ) match {
               case Empty =>
                  c( qidx ) = leaf
//                  order( leaf )
                  orderLeaf( qidx, leaf )
               case n: NonEmpty =>
                  val qn2  = n.union( quad.quadrant( qidx ), point )
                  val n2   = newNode( qn2, n, leaf  )
                  c( qidx ) = n2
//                  n2.order( leaf )
                  n2.orderLeaf( leaf.quadIdxIn( qn2 ), leaf )
            }
         }
      }

      sealed trait LeftNodeImpl extends LeftNode {
         val children = Array.fill[ Child ]( 4 )( Empty )

         def newLeaf( point: Point, value: V ) : Leaf = new LeafImpl( point, value ) // XXX parent?

         def newNode( iq: Quad, old: NonEmpty, nu: Leaf ) : LeftNode = {
            val n    = InnerLeftNode( this, iq )( old )
            val c    = n.children
            val oidx = old.quadIdxIn( iq )
            c( oidx )= old
            val nidx = nu.quadIdxIn( iq )
            c( nidx )= nu
            // XXX parents?
            n
         }
      }

      final case class TopLeftNode( quad: Quad ) extends LeftNodeImpl {
         val startOrder = TotalOrder[ NonEmpty ]( this )
         val stopOrder  = startOrder.insertAfter( this )
      }

      final case class InnerLeftNode( parent: LeftNode, quad: Quad )( _rplc: NonEmpty ) extends LeftNodeImpl {
//         val north   = parent.order( this )
//         val startOrder = _rplc.startOrder.insertBefore( this )
//         val stopOrder  = _rplc.stopOrder.insertAfter( this )
         val startOrder = _rplc.insertOrderBefore( parent, this )
         val stopOrder  = _rplc.insertOrderAfter( parent, this )
      }

      final case class LeafImpl( point: Point, value: V ) extends Leaf {
         def insertOrderBefore( parent: Node, child: NonEmpty ) : InOrder = {
            val cs = parent.children
            var i = 0; while( true ) {
               if( cs( i ) eq this ) {
                  val i1 = i + 1
                  return if( i == 0 ) {
                     parent.insertOrderBefore( parent /* XXX ouch */, child )
                  } else {
                     cs( i - 1 ).insertOrderBefore( parent, child )
                  }
               }
            i+= 1 }
            error( "Never here" )
         }

         def insertOrderAfter( parent: Node, child: NonEmpty ) : InOrder = {
            val cs = parent.children
            var i = 0; while( true ) {
               if( cs( i ) eq this ) {
                  val i1 = i + 1
                  return if( i1 == cs.size ) {
                     parent.insertOrderAfter( parent /* XXX ouch */, child )
                  } else {
                     cs( i1 ).insertOrderAfter( parent, child )
                  }
               }
            i+= 1 }
            error( "Never here" )
         }
      }
   }

  /**
    * Determines the quadrant index of a point `a` in a square `p` defined
    * by its center `pc` and extent `pe`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `a` lies
    *          outside of `p`.
    */
   private def pointInQuad( pq: Quad, a: Point ) : Int = {
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

   private def quadInQuad( pq: Quad, aq: Quad ) : Int = {
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

   private def unsupportedOp : Nothing = error( "Operation not supported" )

   // http://stackoverflow.com/questions/6156502/integer-in-an-interval-with-maximized-number-of-trailing-zero-bits
   @tailrec private def binSplit( a: Int, b: Int, mask: Int = 0xFFFF0000, shift: Int = 8 ): Int = {
      val gt = a > (b & mask)
      if( shift == 0 ) {
         if( gt ) mask >> 1 else mask
      } else {
        binSplit( a, b, if( gt ) mask >> shift else mask << shift, shift >> 1 )
      }
   }

   private def interestingSquare( pq: Quad, aleft: Int, atop: Int, asize: Int,  b: Point ) : Quad = {
//         val pq            = quad.quadrant( pqidx )
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
         Quad( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )
      } else {
         Quad( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )
      }
   }
}
trait DeterministicSkipQuadTree[ V ] extends SkipQuadTree[ V ] {
//   def insert( point: Point, value: V ) : Unit
//   def toOrderedSeq : Seq[ (Point, V) ]
   def skipList : SkipList[ _ ]
}