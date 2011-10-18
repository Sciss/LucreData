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
package mutable

import sys.error
import collection.mutable.{PriorityQueue, Queue => MQueue, Stack => MStack}
import annotation.tailrec
import geom.{QueryShape, DistanceMeasure, Quad, PointLike}

object RandomizedSkipQuadTree {
   def empty[ A ]( quad: Quad )( implicit view: A => PointLike ) : RandomizedSkipQuadTree[ A ] = new TreeImpl[ A ]( quad, view )

   def apply[ A <% PointLike ]( quad: Quad )( xs: A* ) : RandomizedSkipQuadTree[ A ] = {
      val t = empty[ A ]( quad )
      xs.foreach( t.+=( _ ))
      t
   }

//   private def unsupportedOp : Nothing = error( "Operation not supported" )

//   private object TreeImpl {
//      def apply[ V ]( quad: Quad ) = new TreeImpl[ V ]( quad )
//   }
   private final class TreeImpl[ A ]( _quad: Quad, val pointView: A => PointLike ) extends RandomizedSkipQuadTree[ A ] {
      val headTree         = Node( _quad, null, null )()
      private var tailVar  = headTree

      def lastTree: QNode = tailVar

      // ---- map support ----

      def +=( elem: A ) : this.type = {
         insertLeaf( elem )
         this
      }

//      override def update( point: PointLike, value: A ) : Unit = insertLeaf( point, value )

      override def add( elem: A ) : Boolean = {
         val oldLeaf = insertLeaf( elem )
         if( oldLeaf == null ) true else !oldLeaf.value.equals( elem )
      }

      def update( elem: A ) : Option[ A ] = {
         val oldLeaf = insertLeaf( elem )
         if( oldLeaf == null ) None else Some( oldLeaf.value )
      }

      override def remove( elem: A ) : Boolean = {
         val oldLeaf = removeLeaf( pointView( elem ))
         oldLeaf != null
      }

      def removeAt( point: PointLike ) : Option[ A ] = {
         val oldLeaf = removeLeaf( point )
         if( oldLeaf == null ) None else Some( oldLeaf.value )
      }

      def -=( elem: A ) : this.type = {
         removeLeaf( pointView( elem ))
         this
      }

      protected def insertLeaf( value: A ) : Leaf = {
         val point = pointView( value )
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
               n        = Node( _quad, null, tailVar )()
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

      override def contains( elem: A ) : Boolean = {
         val point = pointView( elem )
         if( !_quad.contains( point )) return false
         val l = tailVar.findLeaf( point )
         if( l == null ) false else l.value.equals( elem )
      }

      override def isDefinedAt( point: PointLike ) : Boolean = {
         if( !_quad.contains( point )) return false
         tailVar.findLeaf( point ) != null
      }

//      def apply( point: PointLike ) : A = {
//         val leaf = tailVar.findLeaf( point )
//         if( leaf == null ) throw new java.util.NoSuchElementException( "key not found: " + point )
//         leaf.value
//      }

      def get( point: PointLike ) : Option[ A ] = {
         val leaf = tailVar.findLeaf( point )
         if( leaf == null ) None else Some( leaf.value )
      }

      protected def removeLeaf( point: PointLike ) : Leaf = {
         if( !_quad.contains( point )) {
//println( "wooops " + point )
            return null
         }
         tailVar.remove( point )
      }

      def iterator = new Iterator[ A ] {
         val stack   = MStack.empty[ (Node, Int) ]
         var n       = headTree
         var leaf: Leaf = _
         var idx     = 0
         var hasNext = true

         prepareNext()

         def prepareNext() {
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

         def next() : A = {
            require( hasNext, "Iterator exhausted" )
            val res = leaf.value
            prepareNext()
            res
         }
      }

      def rangeQuery( qs: QueryShape ) : Iterator[ A ] = new RangeQuery( qs )

      private final class VisitedNode( val n: Node, val minDist: Long /* , maxDist: Long */) extends Ordered[ VisitedNode ] {
         def compare( that: VisitedNode ) = -(minDist.compareTo( that.minDist ))
      }

//      private def identify( n: Node ) : String = {
//         val nq      = n.quad
//         val names   = Array( "NE", "NW", "SW", "SE" )
//         val lb      = IndexedSeq.newBuilder[ String ]
//         @tailrec def add( quad: Quad ) {
//            val idx = quad.indexOf( nq )
//            if( idx >= 0 ) {
//               lb += names( idx )
//               add( quad.quadrant( idx ))
//            }
//         }
//         add( quad )
//         lb.result().mkString( " -> " )
//      }

      override def nearestNeighbor( point: PointLike, metric: DistanceMeasure ) : A = {
         val res = nn( point, metric )
         if( res != null ) res.value else throw new NoSuchElementException( "nearestNeighbor on an empty tree" )
      }

      def nearestNeighborOption( point: PointLike, metric: DistanceMeasure ) : Option[ A ] = {
         val res = nn( point, metric )
         if( res != null ) Some( res.value ) else None
      }

//      /*
//algorithm "minimum empty rectangle search"
//- given a point 'q' whose location is unknown, but for which
//  a function 'compare( p, q, i_d)' exists which compares
//  the coordinates of p and q in dimension axis 1 <= i_d <= d,
//  yielding -1 for q < p, 0 for q == p, and 1 for q > p,
//  find the smallest rectangle r_res in the quadtree which
//  contains q
//
//- r_res = root square
//- beginne im root quadrat in Ql
//- for each quadrant, test whether q_res overlaps _the quadrant bounds_
//  (i.e. no matter whether it contains a node, a point, or is empty)
//  --> = def. of interesting square
//- wenn #interesting squares is != 1, return and push the interesting squares onto the prio
//- wenn #interesting squares is == 1
//      step $
//        - if quadrant contains a node -> descend into the node
//        - if quadrant is empty -> if it exists, go to Qi-1 and repeat from $
//        - if quadrant contains leaf ->
//              narrow r_res with respect to the point in that leaf, then:
//              if it exists, go to Qi-1 and repeat from $
//       */
//      def isomorphicQuery( orient: A => Int ) : RectangleLike = {
//
//error( "WRONG. See new algorithm 19-Jul-11" )
//
//         var qLeft   = _quad.left
//         var qTop    = _quad.top
//         var qRight  = _quad.right
//         var qBottom = _quad.bottom
//         var qp : PointLike = null
//         val coll    = MStack.empty[ Node ]
//
//         def isInteresting( q: Quad ) = {
//            qLeft <= q.right  && qRight  >= q.left &&
//            qTop  <= q.bottom && qBottom >= q.top
//         }
//
//         def checkLeaf( n: Node, i: Int, l: Leaf ) {
//            val v    = l.value
//            val p    = pointView( v )
//            val px   = p.x
//            val py   = p.y
//
//            def descend() {
//               var prev = n.prev
//               while( prev != null ) {
//                  prev.child( i ) match {
//                     case n2: Node =>
//                        if( isInteresting( n2.quad )) coll.push( n2 )
//                        prev = null
//                     case _ => // skip as long as the same leaf appears
//                        prev = prev.prev
//                  }
//               }
//            }
//
//            // first see if this point is inside the result rect
//            if( px >= qLeft && px <= qRight && py >= qTop && py <= qBottom ) {
//               // then determine orientation to narrow result rect
//               //  5   4    6
//               //    +---+
//               //  1 | 0 |  2
//               //    +---+
//               //  9   8   10
//               val ori  = orient( v )
//               if( ori == 0 ) {          // hit!
////                     return p
//                  qp    = p
//               }
//               val hOri = ori & 3
//               val vOri = ori >> 2
//               // now do the narrowing
//               if( hOri == 1 ) {             // v is left to query rect
//                  qLeft    = px + 1
//               } else if( hOri == 2 ) {      // v is right to query rect
//                  qRight   = px - 1
//               } else {                      // v.x == q.x
//                  qLeft    = px
//                  qRight   = px
//               }
//               if( vOri == 1 ) {             // v is below query rect
//                  qBottom  = py + 1
//               } else if( vOri == 2 ) {      // v is above query rect
//                  qTop     = py - 1
//               } else {                      // v.y == q.y
//                  qBottom  = py
//                  qTop     = py
//               }
//println( "__q-rect reduced to " + qLeft + ", " + qTop + ", " + qRight + ", " + qBottom )
//               descend()
//            } else {
//               if( isInteresting( n.quad.quadrant( i ))) descend()
//            }
//         }
//
//         def checkNode( n2: Node ) {
//            if( isInteresting( n2.quad )) coll.push( n2 )
//         }
//
//         def checkChildren( n: Node ) {
//            var i = 0; while( i < 4 ) {
//               n.child( i ) match {
//                  case l: Leaf   => checkLeaf( n, i, l )
//                  case n2: Node  => checkNode( n2 )
//                  case Empty =>
//                     if( isInteresting( n.quad.quadrant( i ))) {
//                        var prev = n.prev
//                        while( prev != null ) {
//                           prev.child( i ) match {
//                              case n2: Node =>
//                                 checkNode( n2 )
//                                 prev = null
//                              case l: Leaf =>
//                                 checkLeaf( prev, i, l )
//                                 prev = null
//                              case _ =>   // skip as long the same empty quad appears
//                                 prev = prev.prev
//                           }
//                        }
//                     }
//               }
//            i += 1 }
//         }
//
//         var n = tailVar
//         while( true ) {
//            checkChildren( n )
//            if( qp != null ) return qp  // hit
//            do {
//               if( coll.isEmpty ) {
//                  // WARNING: qRight - qLeft + 1 may well produce 0x80000000 !
//                  return Rectangle( qLeft, qTop, qRight - qLeft + 1, qBottom - qTop + 1 )
//               }
//               n = coll.pop()
//            } while( !isInteresting( n.quad ))
//         }
//         error( "never here" )
//      }

      private def nn( point: PointLike, metric: DistanceMeasure ) : Leaf = {
         var bestLeaf: Leaf      = null
         var bestDist            = Long.MaxValue   // all distances here are squared!
         val pri                 = PriorityQueue.empty[ VisitedNode ]
         val acceptedChildren    = new Array[ VisitedNode ]( 4 )
         var numAcceptedChildren = 0
         var rmax                = Long.MaxValue
//         val abortSq    = {
//            val al = abort.toLong
//            al * al
//         }

         def recheckRMax {
            var j = 0; while( j < numAcceptedChildren ) {
               if( acceptedChildren( j ).minDist > rmax ) {  // immediately kick it out
                  numAcceptedChildren -= 1
                  var k = j; while( k < numAcceptedChildren ) {
                     acceptedChildren( k ) = acceptedChildren( k + 1 )
                  k += 1 }
               }
            j += 1 }
         }

         @tailrec def findNNTail( n0: Node ) {
            numAcceptedChildren = 0
            var accept1Idx = 0
            val oldRMax1 = rmax
            var i = 0; while( i < 4 ) {
               n0.child( i ) match {
                  case l: Leaf =>
                     val ldist = metric.distance( point, pointView( l.value ))
                     if( ldist < bestDist ) {
                        bestDist = ldist
                        bestLeaf = l
                        if( bestDist < rmax ) {
//println( "      : leaf " + l.point + " - " + bestDist )
                           rmax = bestDist
                        }
                     }

                  case c: Node =>
                     val cq            = c.quad
                     val cMinDist      = metric.minDistance( point, cq )
                     if( cMinDist <= rmax ) {   // otherwise we're out already
                        val cMaxDist   = metric.maxDistance( point, cq )
                        if( cMaxDist < rmax ) {
//println( "      : node " + cq + " " + identify( c ) + " - " + cMaxDist )
                           rmax = cMaxDist
                        }
                        acceptedChildren( numAcceptedChildren ) = new VisitedNode( c, cMinDist /*, cMaxDist */)
                        accept1Idx = i
                        numAcceptedChildren += 1
                     }

                  case _ =>
               }
            i += 1 }

            if( rmax != oldRMax1 ) recheckRMax

            // Unless exactly one child is accepted, round is over
            if( numAcceptedChildren != 1 ) return

            // Otherwise find corresponding node in highest level, and descend
            var dn   = acceptedChildren( 0 ).n
            val qdn  = dn.quad
            var succ = n0.next
            while( succ != null ) {
               succ.child( accept1Idx ) match {
                  case dn2: Node if( dn2.quad == qdn ) =>
                     dn    = dn2
                     succ  = succ.next
                  case _ =>
                     succ  = null
               }
            }

            // now go left
            while( dn.prev != null ) dn = dn.prev
            findNNTail( dn )
         }

         var n0 = headTree
         while( true ) {
//println( "ROUND : " + identify( n0 ))
            findNNTail( n0 )
            if( bestDist <= 0L ) return bestLeaf
            var i = 0; while( i < numAcceptedChildren ) {
//println( "++ " + identify( acceptedChildren( i ).n ) + " - " + acceptedChildren( i ).minDist )
               pri += acceptedChildren( i )
            i += 1 }
            var vis: VisitedNode = null
            do {
               if( pri.isEmpty ) {
                  return bestLeaf
               } else {
                  vis = pri.dequeue()
               }
            } while( vis.minDist > rmax )
            n0 = vis.n
         }
         error( "never here" )
      }

      private class RangeQuery( qs: QueryShape ) extends Iterator[ A ] {
         val stabbing      = MQueue.empty[ (Node, Long) ]
         val in            = MQueue.empty[ NonEmpty ]
         var current : A   = _
         var hasNext       = true

         stabbing += headTree -> qs.overlapArea( headTree.quad )
         findNextValue()

         def next() : A = {
            if( !hasNext ) throw new NoSuchElementException( "next on empty iterator" )
            val res = current
            findNextValue()
            res
         }

         def findNextValue() : Unit = while( true ) {
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
                        if( qs.contains( pointView( cl.value ))) in += cl
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
                  current = l.value
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

      final case class Leaf( value: A ) extends NonEmpty with QLeaf
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

         def findP0( point: PointLike, ns: MStack[ Node ]) /* : Leaf = */ {
            val qidx = quad.indexOf( point )
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

         def findLeaf( point: PointLike ) : Leaf = {
            val qidx = quad.indexOf( point )
            quads( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
               case l: Leaf if( pointView( l.value ) == point ) => l
               case _ => if( prev == null ) null else prev.findLeaf( point )
            }
         }

         def findSameSquare( iq: Quad ) : Node = if( quad == iq ) this else parent.findSameSquare( iq )

         def remove( point: PointLike ) : Leaf = {
            val qidx = quad.indexOf( point )
            quads( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.remove( point )
               case l: Leaf if( pointView( l.value ) == point ) =>
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
                     val myIdx = parent.quad.indexOf( quad )
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
         def update( point: PointLike, value: A ) {
            val qidx = quad.indexOf( point )
            quads( qidx ) match {
               case l: Leaf if( pointView( l.value ) == point ) => quads( qidx ) = Leaf( value )
               case _ =>
            }
         }

         def insert( point: PointLike, value: A, prevP: Node ) : Node = {
            val qidx = quad.indexOf( point )
            val l    = Leaf( value )
            quads( qidx ) match {
               case Empty =>
                  quads( qidx ) = l
                  this

               case t: Node =>
                  val tq      = t.quad
//                  assert( !tq.contains( point ))
                  val te      = tq.extent
                  val iq      = gisqr( qidx, tq.cx - te, tq.cy - te, te << 1, point )
                  val iquads  = new Array[ Child ]( 4 )
                  val tidx    = iq.indexOf( tq )
                  iquads( tidx ) = t
                  val pidx    = iq.indexOf( point )
                  iquads( pidx ) = l
                  val qpred   = if( prevP == null ) null else prevP.findSameSquare( iq )
                  val q       = Node( iq, this, qpred )( iquads )
                  t.parent    = q
                  quads( qidx ) = q
                  q

               case l2: Leaf =>
//                  assert( point != point2 )
                  val point2  = pointView( l2.value )
                  val iq      = gisqr( qidx, point2.x, point2.y, 1, point )
                  val iquads  = new Array[ Child ]( 4 )
                  val lidx    = iq.indexOf( point2 )
                  iquads( lidx ) = l2
                  val pidx    = iq.indexOf( point )
                  iquads( pidx ) = l
                  val qpred   = if( prevP == null ) null else prevP.findSameSquare( iq )
                  val q       = Node( iq, this, qpred )( iquads )
                  quads( qidx ) = q
                  q
            }
         }

         private def gisqr( pqidx: Int, aleft: Int, atop: Int, asize: Int,  b: PointLike ) : Quad = {
            val pq            = quad.quadrant( pqidx )
            val tlx           = pq.left   // pq.cx - pq.extent
            val tly           = pq.top    // pq.cy - pq.extent
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
               Quad( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )
            } else {
               Quad( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )
            }
         }
      }
   }

   val random = new util.Random()
   private def flipCoin : Boolean = random.nextBoolean()

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