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
      val list: SkipList[ Leaf ] = HASkipList.empty[ Leaf ]( MaxLeaf, 2 ) // 2-5 DSL
      def skipList = list

      val numChildren = 4

      // ---- map support ----

      def +=( kv: (Point, V) ) : this.type = {
//         println( "insert : " + kv )
         +=( kv._1, kv._2 )
      }

      def +=( point: Point, value: V ) : this.type = {
         val p0   = tl.findP0( point )
         val leaf = p0.insert( point, value )
         list.add( leaf )
         this
      }

      def get( point: Point ) : Option[ V ] = {
         val p0 = tl.findP0( point )
         var i = 0; while( i < numChildren ) {
            p0.child( i ) match {
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
            val leaf = underlying.next
            (leaf.point, leaf.value)
         }
         def hasNext : Boolean = underlying.hasNext
      }

      sealed trait Child
      sealed trait LeftChild extends Child

      case object Empty extends LeftChild
      sealed trait NonEmpty extends Child {
         def union( mq: Quad, point: Point ) : Quad
         def quadIdxIn( iq: Quad ) : Int
      }
      sealed  trait LeftNonEmpty extends NonEmpty with LeftChild {
         def startOrder: InOrder
         def stopOrder: InOrder
      }

      type InOrder = TotalOrder[ LeftNonEmpty ]

      sealed trait Leaf extends LeftNonEmpty with Ordered[ Leaf ] {
         def point : Point
         def value : V
         def order : InOrder

         def compare( that: Leaf ) : Int = order.compare( that.order )

         def union( mq: Quad, point2: Point ) = {
            val p = point
            interestingSquare( mq, p.x, p.y, 1, point2 )
         }
         def quadIdxIn( iq: Quad ) : Int = pointInQuad( iq, point )
         def startOrder : InOrder   = order
         def stopOrder : InOrder    = order
      }

      sealed trait Node extends NonEmpty {
         def child( idx: Int ) : Child

         def findP0( point: Point ) : LeftNode
         def quad: Quad

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
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => prev.findP0( point )
            }
         }
      }

      sealed trait LeftNode extends Node with LeftNonEmpty {
         def children : Array[ LeftChild ]
         def child( idx: Int ) : Child = children( idx )

         def newLeaf( point: Point, value: V ) : Leaf
         def newNode( iq: Quad ) : LeftNode

         def findP0( point: Point ) : LeftNode = {
            val qidx = pointInQuad( quad, point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => this
            }
         }

         def insert( point: Point, value: V ) : Leaf = {
            val qidx = pointInQuad( quad, point )
            val c    = children
            c( qidx ) match {
               case Empty =>
                  val leaf    = newLeaf( point, value )
                  c( qidx )   = leaf
                  leaf
               case old: LeftNonEmpty =>
                  val qn2     = old.union( quad.quadrant( qidx ), point )
                  val n2      = newNode( qn2 )
                  val c2      = n2.children
                  val oidx    = old.quadIdxIn( qn2 )
                  c2( oidx )  = old
                  val leaf    = n2.newLeaf( point, value )
                  val lidx    = leaf.quadIdxIn( qn2 )
                  c2( lidx )  = leaf
                  c( qidx )   = n2
                  leaf
            }
         }
      }

      sealed trait LeftNodeImpl extends LeftNode {
         val children = Array.fill[ LeftChild ]( 4 )( Empty )

         def newLeaf( point: Point, value: V ) : Leaf = LeafImpl( point, value ) { l =>
            val lne: LeftNonEmpty = l
            ((lne.quadIdxIn( quad ): @switch) match {
               case 0 => startOrder.insertAfter( lne )
               case 1 => children( 0 ) match {
                  case n2: LeftNonEmpty => n2.stopOrder.insertAfter( l )
                  case _ => startOrder.insertAfter( lne )
               }
               case 2 => children( 3 ) match {
                  case n2: LeftNonEmpty => n2.startOrder.insertBefore( l )
                  case _ => stopOrder.insertBefore( lne )
               }
               case 3 => stopOrder.insertBefore( lne )
            }) : InOrder // to satisfy idea's presentation compiler
         }

         @tailrec final def insetStart( n: LeftNonEmpty, idx: Int ) : InOrder = {
            if( idx == -1 ) {
               startOrder.insertAfter( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.startOrder.insertBefore( n )
               case _ => insetStart( n, idx  - 1 )
            }
         }
         @tailrec final def insetStop( n: LeftNonEmpty, idx: Int ) : InOrder = {
            if( idx == 4 ) {
               stopOrder.insertBefore( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.stopOrder.insertAfter( n )
               case _ => insetStop( n, idx + 1 )
            }
         }

         def insets( n: LeftNode, nidx: Int ) : (InOrder, InOrder) = {
            (insetStart( n, nidx ), insetStop( n, nidx ))
         }

         def newNode( iq: Quad ) : LeftNode = InnerLeftNode( this, iq ) { n =>
            insets( n, n.quadIdxIn( quad ))
         }
      }

      final case class TopLeftNode( quad: Quad ) extends LeftNodeImpl {
         val startOrder = TotalOrder[ LeftNonEmpty ]( this )
         val stopOrder  = startOrder.insertAfter( this )
      }

      final case class InnerLeftNode( parent: LeftNode, quad: Quad )( _ins: LeftNode => (InOrder, InOrder) ) extends LeftNodeImpl {
         val (startOrder, stopOrder) = _ins( this )
      }

      object MaxLeaf extends Leaf {
         val point   = Point( Int.MaxValue, Int.MaxValue )
         val value   = null.asInstanceOf[ V ]
         val order   = TotalOrder.max[ LeftNonEmpty ]
      }

      final case class LeafImpl( point: Point, value: V )( _ins: Leaf => InOrder ) extends Leaf {
         val order = _ins( this )
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

//   private def unsupportedOp : Nothing = error( "Operation not supported" )

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