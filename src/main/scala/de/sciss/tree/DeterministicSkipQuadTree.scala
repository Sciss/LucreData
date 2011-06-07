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

import annotation.tailrec
import sys.error

object DeterministicSkipQuadTree {
   def apply[ V ]( quad: Quad ) = T[ V ]( quad )

   def fromMap[ V ]( quad: Quad, m: Map[ Point, V ]) : T[ V ] = {
      val t = T[ V ]( quad )
      m.foreach {
         case (point, value) =>
            t.insert( point, value )
      }
      t
   }

   type InOrder = TotalOrder[ Unit ]

   object T {
      def apply[ V ]( quad: Quad ) = new T[ V ]( quad )
   }
   final class T[ V ] private( _quad: Quad ) {
      private var tailVar: QNodeLike /* QTopNode */ = {
         val inOrder = TotalOrder.empty[ Unit ]
//         val list    = HASkipList.empty[ ]
         val north   = inOrder.append( () )
         val south   = north.insertAfter( () )
         new QTopLeftNode( _quad, north, south, Array.fill[ QChild ]( 4 )( QEmpty ))
      }
//      private val inOrder = TotalOrder.empty

      def tail: QNodeLike = tailVar
      var height: Int = 0

      def insert( point: Point, value: V ) {
         // "To insert or delete a point y into or from S, we first search the
         // quadtree structure to locate y in each Qi. Then we insert or delete y
         // in the binary Q0 ..."
         tailVar.insert0( point, value )
         // "... and update our total order."


//         if( qpred.isEmpty || !flipCoin ) return
//         val hq      = head.quad
//         val qidx    = quadIdx( hq, point )
//         val l       = QLeaf( point, value )
//         do {
//            val children      = new Array[ Q[ V ]]( 4 )
//            children( qidx )  = l
//            tailVar        = new QNode[ V ]( hq, Some( tailVar ))( children )
//         } while( flipCoin )
      }

//      sealed trait Q
      sealed trait QChild /* extends Q */ {
         def push( parent: QNodeLike, qidx: Int, point: Point, value: V ) : QChild
         def asBottomNode : QBottomNode
      }
      case object QEmpty extends QChild {
         def push( pp: QNodeLike, qidx: Int, point: Point, value: V ) : QChild = pp.createChildLeaf( qidx, point, value )
//            new QLeaf( parent, point, value )

         def asBottomNode : QBottomNode = unsupportedOp
      }
      sealed trait QNonEmpty // extends Q

      sealed trait QLeafLike extends QNonEmpty with QBottom {
         def gisqr( mq: Quad, point2: Point ) : Quad = interestingSquare( mq, point.x, point.y, 1, point2 )
         def quadIdx( iq: Quad ) : Int = pointInQuad( iq, point )
         def asBottomNode : QBottomNode = unsupportedOp
         def point: Point
         def value: V
      }

      final class QLeftLeaf( var parent: QNodeLike, val tag: InOrder, val point: Point, val value: V ) extends QLeafLike
      final class QRightLeaf( var parent: QNodeLike, val point: Point, val value: V ) extends QLeafLike

      sealed trait QNodeLike extends QNonEmpty {
         def quad: Quad
         def children: Array[ QChild ]

         var next = Option.empty[ QNodeLike ]

         def quadIdx( iq: Quad ) : Int = quadInQuad( iq, quad )

         def gisqr( mq: Quad, point: Point ) : Quad = {
            val ext = quad.extent
            interestingSquare( mq, quad.cx - ext, quad.cy - ext, ext << 1, point )
         }

         /**
          * Creates and returns a new child node for a given quadrant index regarding this node,
          * and with a given interesting square and child array (which may not
          * be filled yet!)
          */
//         def createChildNode( qidx: Int, iq: Quad, children: Array[ QChild ]) : QBottomNode
         def createChildNode( qidx: Int, ch1: QBottom, point: Point, value: V ) : QBottomNode

         def createChildLeaf( qidx: Int, point: Point, value: V ) : QLeafLike

         /**
          * Inserts a point in Q0
          */
         def insert0( point: Point, value: V ) {
            val qidx = pointInQuad( quad, point )
            children( qidx ) = children( qidx ).push( this, qidx, point, value )
         }
      }

      sealed trait QRightNode extends QNodeLike {
         def prev: QNodeLike

//         def createChildNode( qidx: Int, iq: Quad, ichildren: Array[ QChild ]) : QBottomNode = {
//            new QBottomRightNode( this, prev.children( qidx ).asBottomNode, iq, ichildren )
//         }

         // XXX significant code duplication with QLeftNode
         def createChildNode( qidx: Int, ch1: QBottom, point: Point, value: V ) : QBottomNode = {
            val iq               = ch1.gisqr( quad.quadrant( qidx ), point )
            val qchildren        = new Array[ QChild ]( 4 )
            val nidx             = quadIdx( iq )
            val pidx             = pointInQuad( iq, point )
            qchildren( nidx )    = ch1
            val q                = new QBottomRightNode( this, prev.children( qidx ).asBottomNode, iq, qchildren )
            qchildren( pidx )    = q.createChildLeaf( pidx, point, value )
            ch1.parent           = q
            q
         }

         def createChildLeaf( qidx: Int, point: Point, value: V ) : QLeafLike = new QRightLeaf( this, point, value )
      }

//      sealed trait QTopNode extends QNodeLike

      sealed trait QBottom extends QChild {
         def parent: QNodeLike
         def parent_=( n: QNodeLike ) : Unit

         /**
          * Calculates the interesting square for this object's contents along with a given point
          * and a maximum given outer square
          *
          * @param   mq       the maximum square to occupy
          * @param   point    the point to unite with this node's content
          */
         def gisqr( mq: Quad, point: Point ) : Quad

         /**
          * Calculates the quadrant index in which this node's content will
          * end up, given an interesting square
          *
          * @param   iq    the square in which to place this node's content
          */
         def quadIdx( iq: Quad ) : Int

         def push( pp: QNodeLike, qidx: Int, point: Point, value: V ) : QChild =
            pp.createChildNode( qidx, /* iq, ichildren, */ this, point, value )

////            val iq               = gisqr( pp.quad.quadrant( qidx ), point )
////            val ichildren        = new Array[ QChild ]( 4 )
////            val q                =
//
////            val nidx             = quadIdx( iq )
////            val pidx             = pointInQuad( iq, point )
////            parent               = q
////            ichildren( nidx )    = this
////            ichildren( pidx )    = new QLeaf( q, point, value )
//            q
//         }
      }
      sealed trait QBottomNode extends QNodeLike with QBottom {
         def asBottomNode : QBottomNode = this
      }

      sealed trait QLeftNode extends QNodeLike {
         def north: InOrder
         def south: InOrder

//         def createChildNode( qidx: Int, iq: Quad, ichildren: Array[ QChild ]) : QBottomNode

         def createChildNode( qidx: Int, ch1: QBottom, point: Point, value: V ) : QBottomNode = {
            val iq               = ch1.gisqr( quad.quadrant( qidx ), point )
            val qchildren        = new Array[ QChild ]( 4 )
            val nidx             = quadIdx( iq )
            val pidx             = pointInQuad( iq, point )
            qchildren( nidx )    = ch1
            val qhemi            = if( qidx < 2 ) north else south
            val qnorth           = if( (qidx % 2) == 0 ) qhemi.insertBefore( () ) else qhemi.insertAfter( () )
            val qsouth           = qnorth.insertAfter( () )
            val q                = new QBottomLeftNode( this, iq, qnorth, qsouth, qchildren )
            qchildren( pidx )    = q.createChildLeaf( pidx, point, value )
            ch1.parent           = q
            q
         }

         def createChildLeaf( pidx: Int, point: Point, value: V ) : QLeafLike = {
            val phemi   = if( pidx < 2 ) north else south
            val ptag    = if( (pidx % 2) == 0 ) phemi.insertBefore( () ) else phemi.insertAfter( () )
            new QLeftLeaf( this, ptag, point, value )
         }
      }

      final class QTopLeftNode( val quad: Quad, val north: InOrder, val south: InOrder, val children: Array[ QChild ])
      extends QLeftNode // with QTopNode

      final class QTopRightNode( val prev: QNodeLike, val quad: Quad, val children: Array[ QChild ])
         extends QRightNode // with QTopNode

      final class QBottomLeftNode( var parent: QNodeLike, val quad: Quad, val north: InOrder, val south: InOrder, val children: Array[ QChild ])
      extends QLeftNode with QBottomNode

      final class QBottomRightNode( var parent: QNodeLike, val prev: QNodeLike, val quad: Quad, val children: Array[ QChild ])
      extends QRightNode with QBottomNode
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