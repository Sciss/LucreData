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
      def apply[ V ]( quad: Quad ) = {
         val inOrder = TotalOrder.empty[ Unit ]
         val north   = inOrder.append( () )
         val south   = north.insertAfter( () )
         val root    = new QTopLeftNode[ V ]( quad, north, south, Array.fill[ QChild[ V ]]( 4 )( QEmpty ))
         new T[ V ]( inOrder, root )
      }
   }
   final class T[ V ] private( inOrder: TotalOrder[ Unit ], val head: QTopLeftNode[ V ]) {
      private var tailVar: QTopNode[ V ] = head
//      private val inOrder = TotalOrder.empty

      def tail: QTopNode[ V ] = tailVar
      var height: Int = 0

      def insert( point: Point, value: V ) {
         tailVar.insert0( point, value )
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

   sealed trait Q[ +V ]
   sealed trait QChild[ +V ] extends Q[ V ]
   case object QEmpty extends QChild[ Nothing ]
   sealed trait QNonEmpty[ V ] extends Q[ V ] {
//      /**
//       * Pushes a new point into this node, returning the
//       * new quad and the Q array to be used in the new parent construction
//       */
//      def push( pquad: Quad, point: Point, value: V ) : (Quad, Array[ Q[ V ]])
   }
   final class QLeaf[ V ]( var parent: QNodeLike[ V ], val point: Point, val value: V )
   extends QNonEmpty[ V ] with QBottom[ V ] {
      def gisqr( mq: Quad, point2: Point ) : Quad = interestingSquare( mq, point.x, point.y, 1, point2 )
      def quadIdx( iq: Quad ) : Int = pointInQuad( iq, point )
   }

   sealed trait QNodeLike[ V ] extends QNonEmpty[ V ] {
      def quad: Quad
      def children: Array[ QChild[ V ]]
//      def north: InOrder
//      def south: InOrder

      var next = Option.empty[ QNodeLike[ V ]]

      def quadIdx( iq: Quad ) : Int = quadInQuad( iq, quad )

      def gisqr( mq: Quad, point: Point ) : Quad = {
         val ext = quad.extent
         interestingSquare( mq, quad.cx - ext, quad.cy - ext, ext << 1, point )
      }

//      def next: Option[ QNodeLike[ V ]]
//      def next_=( n: Option[ QNodeLike[ V ]]) : Unit

      /**
       * Creates and returns a new child node for a given quadrant index regarding this node,
       * and with a given interesting square and child array (which may not
       * be filled yet!)
       */
      protected def createChildNode( qidx: Int, iq: Quad, children: Array[ QChild[ V ]]) : QBottomNode[ V ]

      /**
       * Inserts a point in Q0
       */
      def insert0( point: Point, value: V ) {
         val qidx = pointInQuad( quad, point )
         children( qidx ) match {
            case QEmpty => children( qidx ) = new QLeaf( this, point, value )
            case n: QBottom[ V ] =>
               val iq               = n.gisqr( quad.quadrant( qidx ), point )
               val ichildren        = new Array[ QChild[ V ]]( 4 )
               val q                = createChildNode( qidx, iq, ichildren )
               val nidx             = n.quadIdx( iq )
               val pidx             = pointInQuad( iq, point )
               n.parent             = q
               ichildren( nidx )    = n
               ichildren( pidx )    = new QLeaf( q, point, value )
               children( qidx )     = q
         }
      }

//      def push( pquad: Quad, point2: Point, value: V ) : (Quad, Array[ Q[ V ]]) = {
//         val te      = quad.extent
//         val iq      = gisqr( pquad, quad.cx - te, quad.cy - te, te << 1, point2 )
//         val iquads  = new Array[ Q[ V ]]( 4 )
//         val lidx    = quadIdx( iq, quad )
//         iquads( lidx ) = this
//         val pidx    = quadIdx( iq, point2 )
//         iquads( pidx ) = QLeaf( point2, value )
//         (iq, iquads)
//      }
   }

//   sealed trait QTopNode[ V ] extends QNodeLike[ QTopRightNode[ V ], V ]
//   sealed trait QBottomNode[ Parent, V ] extends QNodeLike[ QBottomRightNode[ V ], V ] {
//      def parent: Parent
//   }
//   sealed trait QLeftNode[ Next, V ] extends QNodeLike[ Next, V ]
//   sealed trait QRightNode[ Prev, Next, V ] extends QNodeLike[ Next, V ] {
//      def prev: Prev
//   }
//
   sealed trait QRightNode[ V ] extends QNodeLike[ V ] {
      def prev: QNodeLike[ V ]

      protected def createChildNode( qidx: Int, iq: Quad, ichildren: Array[ QChild[ V ]]) : QBottomNode[ V ] = {
         prev.children( qidx ) match {
            case iprev: QBottomNode[ V ] => new QBottomRightNode[ V ]( this, iprev, iq, ichildren )
            case _ => error( "Previous level doesn't have matching node!" )
         }
      }
   }

   sealed trait QTopNode[ V ] extends QNodeLike[ V ]

   sealed trait QBottom[ V ] extends QChild[ V ] {
      def parent: QNodeLike[ V ]
      def parent_=( n: QNodeLike[ V ]) : Unit

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
   }
   sealed trait QBottomNode[ V ] extends QNodeLike[ V ] with QBottom[ V ]

   sealed trait QLeftNode[ V ] extends QNodeLike[ V ] {
      def north: InOrder
      def south: InOrder

//      protected def createOrders( qidx: Int ) : (InOrder, InOrder) = {
//         val parent = if( qidx < 2 ) north else south
//         (parent.insertBefore( () ), parent.insertAfter( () ))
//      }

      protected def createChildNode( qidx: Int, iq: Quad, ichildren: Array[ QChild[ V ]]) : QBottomNode[ V ] = {
         val hemi    = if( qidx < 2 ) north else south
         val inorth  = hemi.insertBefore( () )
         val isouth  = hemi.insertAfter( () )
         new QBottomLeftNode[ V ]( this, iq, inorth, isouth, ichildren )
      }
   }

   final class QTopLeftNode[ V ]( val quad: Quad, val north: InOrder, val south: InOrder, val children: Array[ QChild[ V ]])
   extends QLeftNode[ V ] with QTopNode[ V ]

   final class QTopRightNode[ V ]( val prev: QNodeLike[ V ], val quad: Quad, val children: Array[ QChild[ V ]])
      extends QRightNode[ V ] with QTopNode[ V ]

   final class QBottomLeftNode[ V ]( var parent: QNodeLike[ V ], val quad: Quad, val north: InOrder, val south: InOrder, val children: Array[ QChild[ V ]])
   extends QLeftNode[ V ] with QBottomNode[ V ]

   final class QBottomRightNode[ V ]( var parent: QNodeLike[ V ], val prev: QNodeLike[ V ], val quad: Quad, val children: Array[ QChild[ V ]])
   extends QRightNode[ V ] with QBottomNode[ V ] {
//      var next = Option.empty[ QNodeLike[ V ]];

      // fix null squares
      {
         var i = 0; while( i < 4 ) {
            if( children( i ) == null ) children( i ) = QEmpty
         i += 1 }
      }

      protected def gisqr( pqidx: Int, aleft: Int, atop: Int, asize: Int,  b: Point ) : Quad = {
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
            Quad( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )
         } else {
            Quad( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )
         }
      }

//      def insertStep( point: Point, value: V ) : Option[ QNode[ V ]] = {
//         val qidx = quadIdx( quad, point )
////         require( qidx >= 0, point.toString + " lies outside of root square " + quad )
//         quads( qidx ) match {
//            case QEmpty =>
//               if( prev.isEmpty /* || (pred.flatMap( _.insertStep( point, value )).nonEmpty && flipCoin) */ ) {
//                  children( qidx ) = QLeaf( point, value )
//                  Some( this )
//               } else None
//
//            case t: QNode =>
//               val tq = t.quad
//               if( tq.contains( point )) {
//                  t.insertStep( point, value )
//               } else {
////                  val qpred = pred.flatMap( _.insertStep( point, value ))
//                  if( prev.isEmpty /* || (qpred.nonEmpty && flipCoin) */ ) {
//                     val te      = tq.extent
//                     val iq      = gisqr( qidx, tq.cx - te, tq.cy - te, te << 1, point )
//                     val iquads  = new Array[ Q[ V ]]( 4 )
//                     val tidx    = quadIdx( iq, tq )
//                     iquads( tidx ) = t
//                     val pidx    = quadIdx( iq, point )
//                     iquads( pidx ) = QLeaf( point, value )
//                     val (north, south) = createOrders( qidx )
//                     val q       = QNode[ V ]( iq, None /* qpred */, north, south )( iquads )
//                     children( qidx ) = q
//                     Some( q )
//                  } else None
//               }
//
//            case l @ QLeaf( point2, value2 ) =>
////               val qpred   = pred.flatMap( _.insertStep( point, value ))
//               if( pred.isEmpty /* || (qpred.nonEmpty && flipCoin) */ ) {
//                  val iq      = gisqr( qidx, point2.x, point2.y, 1, point )
//                  val iquads  = new Array[ Q[ V ]]( 4 )
//                  val lidx    = quadIdx( iq, point2 )
//                  iquads( lidx ) = l
//                  val pidx    = quadIdx( iq, point )
//                  iquads( pidx ) = QLeaf( point, value )
//                  val (north, south) = createOrders( qidx )
//                  val q       = QNode[ V ]( iq, None /* qpred */, north, south )( iquads )
//                  children( qidx ) = q
//                  Some( q )
//               } else None
//         }
//      }
   }
}