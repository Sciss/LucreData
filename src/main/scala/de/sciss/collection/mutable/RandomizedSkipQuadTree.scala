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
 */

package de.sciss.collection
package mutable

import collection.mutable.{Stack => MStack}
import geom.{Quad, PointLike}

object RandomizedSkipQuadTree {
   def empty[ A ]( quad: Quad )( implicit view: A => PointLike ) : SkipQuadTree[ A ] = new TreeImpl[ A ]( quad, view )

   def apply[ A <% PointLike ]( quad: Quad )( xs: A* ) : SkipQuadTree[ A ] = {
      val t = empty[ A ]( quad )
      xs.foreach( t.+=( _ ))
      t
   }

//   private def unsupportedOp : Nothing = error( "Operation not supported" )

//   private object TreeImpl {
//      def apply[ V ]( quad: Quad ) = new TreeImpl[ V ]( quad )
//   }
   private final class TreeImpl[ A ]( quad: Quad, val pointView: A => PointLike )
   extends impl.SkipQuadTreeImpl[ A ] {
      val headTree         = new Node( quad, null, null )
      private var tailVar  = headTree

      def lastTree: QNode  = tailVar

      protected def findLeaf( point: PointLike ) : Leaf = tailVar.findLeaf( point )

      protected def insertLeaf( value: A ) : Leaf = {
         val point = pointView( value )
         require( quad.contains( point ), point.toString + " lies out of root square " + quad )

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
               n        = new Node( quad, null, tailVar )
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

      protected def removeLeaf( point: PointLike ) : Leaf = {
         if( !quad.contains( point )) {
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
               n.children( idx ) match {
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

//      def rangeQuery( qs: QueryShape ) : Iterator[ A ] = new RangeQuery( qs )

      private final class VisitedNode( val n: Node, val minDist: Long /* , maxDist: Long */) extends Ordered[ VisitedNode ] {
         def compare( that: VisitedNode ) = -(minDist.compareTo( that.minDist ))
      }

      sealed trait Child extends Q
      case object Empty extends Child with QEmpty

      sealed trait NonEmpty extends Child

      final case class Leaf( value: A ) extends NonEmpty with QLeaf

      final class Node( val quad: Quad, var parent: Node, val prev: Node, val children: Array[ Child ] = new Array[ Child ]( 4 ))
      extends NonEmpty with QNode {
         var next: Node = null;

         // fix null squares and link
         {
            var i = 0; while( i < 4 ) {
               if( children( i ) == null ) children( i ) = Empty
            i += 1 }

            if( prev != null ) prev.next = this
         }

         def child( idx: Int ) : Child = children( idx )

         def findP0( point: PointLike, ns: MStack[ Node ]) /* : Leaf = */ {
            val qidx = quad.indexOf( point )
            children( qidx ) match {
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
            children( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
               case l: Leaf if( pointView( l.value ) == point ) => l
               case _ => if( prev == null ) null else prev.findLeaf( point )
            }
         }

         def findSameSquare( iq: Quad ) : Node = if( quad == iq ) this else parent.findSameSquare( iq )

         def remove( point: PointLike ) : Leaf = {
            val qidx = quad.indexOf( point )
            children( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.remove( point )
               case l: Leaf if( pointView( l.value ) == point ) =>
                  children( qidx ) = Empty
                  var lonely: NonEmpty = null
                  var numNonEmpty = 0
                  var i = 0; while( i < 4 ) {
                     children( i ) match {
                        case n: NonEmpty =>
                           numNonEmpty += 1
                           lonely = n
                        case _ =>
                     }
                  i += 1 }
                  if( numNonEmpty == 1 && parent != null ) {   // gotta remove this node and put remaining non empty element in parent
                     if( prev != null ) prev.next = null       // note: since remove is called from Qn to Q0, there is no this.next !
                     val myIdx = parent.quad.indexOf( quad )
                     parent.children( myIdx ) = lonely
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
            children( qidx ) match {
               case l: Leaf if( pointView( l.value ) == point ) => children( qidx ) = Leaf( value )
               case _ =>
            }
         }

         def insert( point: PointLike, value: A, prevP: Node ) : Node = {
            val qidx = quad.indexOf( point )
            val l    = Leaf( value )
            children( qidx ) match {
               case Empty =>
                  children( qidx ) = l
                  this

               case t: Node =>
                  val tq      = t.quad
//                  assert( !tq.contains( point ))
                  val te      = tq.extent
                  val iq      = quad.quadrant( qidx ).greatestInteresting( tq.cx - te, tq.cy - te, te << 1, point )
                  val iquads  = new Array[ Child ]( 4 )
                  val tidx    = iq.indexOf( tq )
                  iquads( tidx ) = t
                  val pidx    = iq.indexOf( point )
                  iquads( pidx ) = l
                  val qpred   = if( prevP == null ) null else prevP.findSameSquare( iq )
                  val q       = new Node( iq, this, qpred, iquads )
                  t.parent    = q
                  children( qidx ) = q
                  q

               case l2: Leaf =>
//                  assert( point != point2 )
                  val point2  = pointView( l2.value )
                  val iq      = quad.quadrant( qidx ).greatestInteresting( point2.x, point2.y, 1, point )
                  val iquads  = new Array[ Child ]( 4 )
                  val lidx    = iq.indexOf( point2 )
                  iquads( lidx ) = l2
                  val pidx    = iq.indexOf( point )
                  iquads( pidx ) = l
                  val qpred   = if( prevP == null ) null else prevP.findSameSquare( iq )
                  val q       = new Node( iq, this, qpred, iquads )
                  children( qidx ) = q
                  q
            }
         }
      }
   }

   val random = new util.Random()
   private def flipCoin : Boolean = random.nextBoolean()
}
//trait RandomizedSkipQuadTree[ V ] extends SkipQuadTree[ V ]