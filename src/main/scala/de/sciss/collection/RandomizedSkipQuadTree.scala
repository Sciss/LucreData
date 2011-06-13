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

import sys.error
import collection.mutable.{PriorityQueue, Queue => MQueue, Stack => MStack}
import annotation.{switch, tailrec}

object RandomizedSkipQuadTree {
   def empty[ V ]( quad: Quad ) : RandomizedSkipQuadTree[ V ] = new TreeImpl[ V ]( quad )

   def apply[ V ]( quad: Quad )( xs: (Point, V)* ) : RandomizedSkipQuadTree[ V ] = {
      val t = new TreeImpl[ V ]( quad )
      xs.foreach( t.+=( _ ))
      t
   }

   private def unsupportedOp : Nothing       = error( "Operation not supported" )

//   private object TreeImpl {
//      def apply[ V ]( quad: Quad ) = new TreeImpl[ V ]( quad )
//   }
   private final class TreeImpl[ V ]( _quad: Quad  ) extends RandomizedSkipQuadTree[ V ] {
      val headTree         = Node( _quad, null, null )()
      private var tailVar  = headTree

      def lastTree: QNode = tailVar

      // ---- map support ----

      def +=( kv: (Point, V) ) : this.type = {
         insertLeaf( kv._1, kv._2 )
         this
      }

      override def update( point: Point, value: V ) : Unit = insertLeaf( point, value )

      override def put( point: Point, value: V ) : Option[ V ] = {
         val oldLeaf = insertLeaf( point, value )
         if( oldLeaf == null ) None else Some( oldLeaf.value )
      }

      override def remove( point: Point ) : Option[ V ] = {
         val oldLeaf = removeLeaf( point )
         if( oldLeaf == null ) None else Some( oldLeaf.value )
      }

      def -=( point: Point ) : this.type = {
         removeLeaf( point )
         this
      }

      protected def insertLeaf( point: Point, value: V ) : Leaf = {
         require( _quad.contains( point ), point.toString + " lies out of root square " + _quad )

         val ns      = MStack.empty[ Node ]
         tailVar.findP0( point, ns )
         val l       = ns.top.findLeaf( point )
//println( "for " + point + " leaf is " + l )
         if( l == null ) { // no entry existed for that point
            var coin    = true
            var n: Node = null
            var pr: Node= null
            while( coin && ns.nonEmpty ) {
               n  = ns.pop
               pr = n.insert( point, value, pr )
               coin &= flipCoin
            }
            while( coin ) {
               n        = Node( _quad, null, n )()
               pr       = n.insert( point, value, pr )
               tailVar  = n
               coin    &= flipCoin
            }
         } else { // gotta replace all the existing leaves for that point
            while( ns.nonEmpty ) {
               val n = ns.pop
               n.update( point, value )
            }
         }
         l
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

      protected def removeLeaf( point: Point ) : Leaf = {
         if( !_quad.contains( point )) {
//println( "wooops " + point )
            return null
         }
         tailVar.remove( point )
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
               while( idx > 3 ) {
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

      def rangeQuery( qs: QueryShape ) : Iterator[ (Point, V) ] = new RangeQuery( qs )

      /**
       * XXX TODO: this currently doesn't maintain the lowest-ancestor pointers needed,
       * thus this search may report false positives (i.e. an entry which in fact is not the
       * nearest)
       *
       * XXX TODO: could easily extend from 1 NN to k NN
       */
      def nearestNeighbor( point: Point, abort: Int = 0 ) : Option[ (Point, V) ] = {
         val chDists    = new Array[ Long ]( 4 )
         val abortSq    = {
            val al = abort.toLong
            al * al
         }

         // n0 is in Q0, while _n is any successor of n0, not necessarily the highest
         def findNNTail( n0: Node, equiDist: Long, _n: Node /*, chDists: Array[ Long ] */ ) : Node = {
            // "start from p at the highest level in the
            // skip structure that contains p"
            var n = _n; while( n.next != null ) n = n.next

            // "we first check if q ∈ Q0 has two or more
            // child squares equidistant with p."
            var equiCnt0 = 0
            var equiCh: Node = null
            var i = 0; while( i < 4 ) {
               n0.child( i ) match {
                  case nn0: Node =>
                     val nn0dist = nn0.quad.closestDistanceSq( point )
                     chDists( i ) = nn0dist
                     if( nn0dist == equiDist ) {
                        equiCnt0 += 1
                        equiCh    = nn0
                     }
                  case _ =>
//                     chDists( i ) = Long.MaxValue   // indicates skip
               }
            i += 1 }
            // "If so we stop and return q."
            if( equiCnt0 >= 2 ) return n0

            // "we either go to a child square of q in Qi that is
            // equidistant to p, if such a child square exists,
            // or jump to the next level q ∈ Qi−1."
            while( !(n0 eq n) ) {
               var i = 0; while( i < 4 ) {
                  n.child( i ) match {
                     case nn: Node =>
                        val nndist = nn.quad.closestDistanceSq( point )
                        if( nndist == equiDist ) {
                           var nn0 = nn; while( nn0.prev != null ) nn0 = nn0.prev
                           return findNNTail( nn0, equiDist, nn )
                        }
                     case _ =>
                  }
               i += 1 }
               n = n.prev
            }
            if( equiCnt0 == 0 ) n0 else /* must be 1 */ findNNTail( equiCh, equiDist, equiCh )
         }

         val pri        = PriorityQueue.empty( Ordering.by[ (Long, Node), Long ]( -_._1 )) // reverse!
         var bestLeaf: Leaf = null
         var bestDist   = Long.MaxValue   // all distances here are squared!
         var p          = headTree
         var pdist      = headTree.quad.closestDistanceSq( point )

         while( (p != null) && (bestDist > abortSq) ) {
            val n = findNNTail( p, pdist, p )
            var i = 0; while( i < 4 ) {
               n.child( i ) match {
                  case l: Leaf =>
                     val ld = l.point.distanceSq( point )
                     if( ld < bestDist ) {
                        bestDist = ld
                        bestLeaf = l
                     }
                  case n: Node =>
                     val nd = chDists( i )
                     if( nd < bestDist ) pri += nd -> n
                  case _ =>
               }
            i += 1 }
            if( pri.isEmpty ) {
               p        = null
            } else {
               val tup  = pri.dequeue()
               pdist    = tup._1
               p        = tup._2
            }
         }
         if( bestLeaf != null ) {
            Some( bestLeaf.point -> bestLeaf.value )
         } else None
      }

      private class RangeQuery( qs: QueryShape ) extends Iterator[ (Point, V) ] {
         val stabbing      = MQueue.empty[ (Node, Long) ]
         val in            = MQueue.empty[ NonEmpty ]
         var current : (Point, V) = _
         var hasNext       = true

         stabbing += headTree -> qs.overlapArea( headTree.quad )
         findNextValue

         def next : (Point, V) = {
            if( !hasNext ) throw new NoSuchElementException( "next on empty iterator" )
            val res = current
            findNextValue
            res
         }

         def findNextValue : Unit = while( true ) {
            if( in.isEmpty ) {
               if( stabbing.isEmpty ) {
                  hasNext = false
                  return
               }
               val tup  = stabbing.dequeue()
               val ns   = tup._1                         // stabbing node
               val as   = tup._2
               val nc   = ns.rangeQueryRight( as, qs )   // critical node
               var i = 0; while( i < 4 ) {
                  nc.child( i ) match {
                     case cl: Leaf =>
                        if( qs.contains( cl.point )) in += cl
                     case cn: Node =>
                        val q    = cn.quad
                        val ao   = qs.overlapArea( q )
                        if( ao > 0 ) {
                           if( ao < q.area ) {           // stabbing
                              stabbing += cn -> ao
                           } else {                      // in
                              in += cn
                           }
                        }
                     case _ =>
                  }
               i += 1 }

            } else in.dequeue() match {
               case l: Leaf =>
                  current = (l.point, l.value)
                  return
               case n: Node =>
                  var i = 0; while( i < 4 ) {
                     n.child( i ) match {
                        case ne: NonEmpty => in += ne // sucky `enqueue` creates intermediate Seq because of varargs
                        case _ =>
                     }
                  i += 1 }
            }
         }
      }

      sealed trait Child extends Q
      case object Empty extends Child with QEmpty

      sealed trait NonEmpty extends Child

      final case class Leaf( point: Point, value: V ) extends NonEmpty with QLeaf
      final case class Node( quad: Quad, var parent: Node, prev: Node )( val quads: Array[ Child ] = new Array[ Child ]( 4 ))
      extends NonEmpty with QNode {
         var next: Node = null;

         // fix null squares and link
         {
            var i = 0; while( i < 4 ) {
               if( quads( i ) == null ) quads( i ) = Empty
            i += 1 }

            if( prev != null ) prev.next = this
         }

         def child( idx: Int ) : Child = quads( idx )

         def prevOption = Option( prev: QNode ) // Option( null ) becomes None
         def nextOption = Option( next: QNode ) // Option( null ) becomes None

         def rangeQueryRight( area: Long, qs: QueryShape ) : Node = {
            var i = 0; while( i < 4 ) {
               quads( i ) match {
                  case n2: Node =>
                     val a2 = qs.overlapArea( n2.quad )
                     if( a2 == area ) {
                        if( next != null ) {
                           next.rangeQueryRight( area, qs )
                        } else {
                           n2.rangeQueryLeft( a2, qs )
                        }
                     }
                  case _ =>
               }
            i += 1 }
            // at this point, we know `this` is critical
            if( prev != null ) prev else this
         }

         def rangeQueryLeft( area: Long, qs: QueryShape ) : Node = {
            var i = 0; while( i < 4 ) {
               quads( i ) match {
                  case n2: Node =>
                     val a2 = qs.overlapArea( n2.quad )
                     if( a2 == area ) {
                        if( next != null ) {
                           next.rangeQueryLeft( area, qs )
                        } else {
                           n2.rangeQueryLeft( a2, qs )
                        }
                     }
                  case _ =>
               }
            i += 1 }
            this
         }

         def findP0( point: Point, ns: MStack[ Node ]) /* : Leaf = */ {
            val qidx = quadIdx( quad, point )
            quads( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point, ns )
//               case l: Leaf if( prev == null && l.point == point ) =>
//                  ns.push( this )
//                  l
               case _ =>
                  ns.push( this )
                  if( prev != null ) prev.findP0( point, ns ) else null
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

         def remove( point: Point ) : Leaf = {
            val qidx = quadIdx( quad, point )
            quads( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.remove( point )
               case l: Leaf if( l.point == point ) =>
                  quads( qidx ) = Empty
                  var lonely: NonEmpty = null
                  var numNonEmpty = 0
                  var i = 0; while( i < 4 ) {
                     quads( i ) match {
                        case n: NonEmpty =>
                           numNonEmpty += 1
                           lonely = n
                        case _ =>
                     }
                  i += 1 }
                  if( numNonEmpty == 1 && parent != null ) {   // gotta remove this node and put remaining non empty element in parent
                     if( prev != null ) prev.next = null       // note: since remove is called from Qn to Q0, there is no this.next !
                     val myIdx = quadIdx( parent.quad, quad )
                     parent.quads( myIdx ) = lonely
                     lonely match {
                        case n: Node => n.parent = parent
                        case _ =>
                     }
                  } else if( numNonEmpty == 0 && prev != null ) {  // meaning that this is a root node (but not headTree)
                     prev.next   = null
                     tailVar     = prev
                  }
                  if( prev != null ) prev.remove( point ) else l

               case _ =>
                  if( prev != null ) prev.remove( point ) else null
            }
         }

         /**
          * If a leaf with the given point exists in this node,
          * updates its value accordingly.
          */
         def update( point: Point, value: V ) {
            val qidx = quadIdx( quad, point )
            quads( qidx ) match {
               case l: Leaf if( l.point == point ) => quads( qidx ) = Leaf( point, value )
               case _ =>
            }
         }

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
                  assert( point != point2 )
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