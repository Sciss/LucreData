/*
*  SkipOctreeImpl.scala
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

package de.sciss.collection.mutable
package impl

import annotation.tailrec
import collection.mutable.{PriorityQueue, Queue => MQueue}
import de.sciss.collection.geom.{Space, QueryShape, DistanceMeasure}

trait SkipOctreeImpl[ D <: Space[ D ], A ] extends SkipOctree[ D, A ] {
   // ---- map support ----

   final def +=( elem: A ) : this.type = {
      insertLeaf( elem )
      this
   }

   final override def add( elem: A ) : Boolean = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf == null ) true else oldLeaf.value != elem
   }

   final def update( elem: A ) : Option[ A ] = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf == null ) None else Some( oldLeaf.value )
   }

   final override def remove( elem: A ) : Boolean = {
      val oldLeaf = removeLeaf( pointView( elem ))
      oldLeaf != null
   }

   final def removeAt( point: D#Point ) : Option[ A ] = {
      val oldLeaf = removeLeaf( point )
      if( oldLeaf == null ) None else Some( oldLeaf.value )
   }

   final def -=( elem: A ) : this.type = {
      removeLeaf( pointView( elem ))
      this
   }

   final override def contains( elem: A ) : Boolean = {
      val point = pointView( elem )
      if( !hyperCube.contains( point )) return false
      val l = findLeaf( point )
      if( l == null ) false else l.value == elem
   }

   final override def isDefinedAt( point: D#Point ) : Boolean = {
      if( !hyperCube.contains( point )) return false
      findLeaf( point ) != null
   }

   final def get( point: D#Point ) : Option[ A ] = {
      if( !hyperCube.contains( point )) return None
      val l = findLeaf( point )
      if( l == null ) None else Some( l.value )
   }

   final override def nearestNeighbor( point: D#Point, metric: DistanceMeasure[ D ]) : A = {
      val res = nn( point, metric )
      if( res != null ) res.value else throw new NoSuchElementException( "nearestNeighbor on an empty tree" )
   }

   final def nearestNeighborOption( point: D#Point, metric: DistanceMeasure[ D ]) : Option[ A ] = {
      val res = nn( point, metric )
      if( res != null ) Some( res.value ) else None
   }

   final override def isEmpty : Boolean = {
      val n = headTree
      var i = 0; while( i < 4 ) {
         n.child( i ) match {
            case e: QEmpty =>
            case _ => return false
         }
      i += 1 }
      true
   }

   final def numLevels : Int = {
      var n = headTree
      val t = lastTree
      var i = 1; while( !(n eq t) ) {
         n = n.nextOption.orNull
         i += 1
      }
      i
   }

   final def rangeQuery( qs: QueryShape[ D ]) : Iterator[ A ] = new RangeQuery( qs )

   protected def insertLeaf( elem: A ) : QLeaf
   protected def removeLeaf( point: D#Point ) : QLeaf
   protected def findLeaf( point: D#Point ) : QLeaf

   private final class RangeQuery( qs: QueryShape[ D ]) extends Iterator[ A ] {
      val stabbing      = MQueue.empty[ (QNode, D#BigNum) ]
      val in            = MQueue.empty[ QNonEmpty ]
      var current : A   = _
      var hasNext       = true

      stabbing += headTree -> qs.overlapArea( headTree.hyperCube )
      findNextValue()

      def rangeQueryLeft( node: QNode, area: D#BigNum, qs: QueryShape[ D ]) : QNode = {
         var i = 0; while( i < 4 ) {
            node.child( i ) match {
               case n2: QNode =>
                  val a2 = qs.overlapArea( n2.hyperCube )
                  if( a2 == area ) {
                     val next = node.next
                     if( next != null ) {
                        rangeQueryLeft( next, area, qs )
                     } else {
                        rangeQueryLeft( n2, a2, qs )
                     }
                  }
               case _ =>
            }
         i += 1 }
         node
      }

      def rangeQueryRight( node: QNode, area: D#BigNum, qs: QueryShape[ D ]) : QNode = {
         var i = 0; while( i < 4 ) {
            node.child( i ) match {
               case n2: QNode =>
                  val a2 = qs.overlapArea( n2.hyperCube )
                  if( a2 == area ) {
                     val next = node.next
                     if( next != null ) {
                        rangeQueryRight( next, area, qs )
                     } else {
                        rangeQueryLeft( n2, a2, qs )
                     }
                  }
               case _ =>
            }
         i += 1 }
         // at this point, we know `this` is critical
         val prev = node.prev
         if( prev != null ) prev else node
      }

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
            val ns   = tup._1                            // stabbing node
            val as   = tup._2
//            val nc   = ns.rangeQueryRight( as, qs )    // critical node
            val nc   = rangeQueryRight( ns, as, qs )     // critical node
            var i = 0; while( i < 4 ) {
               nc.child( i ) match {
                  case cl: QLeaf =>
                     if( qs.contains( pointView( cl.value ))) in += cl
                  case cn: QNode =>
                     val q    = cn.hyperCube
                     val ao   = qs.overlapArea( q )
                     if( space.bigGtZero( ao )) {
                        if( space.bigGt( q.area, ao )) { // ao < q.area  // stabbing
                           stabbing += cn -> ao
                        } else {                         // in
                           in += cn
                        }
                     }
                  case _ =>
               }
            i += 1 }

         } else in.dequeue() match {
            case l: QLeaf =>
               current = l.value
               return
            case n: QNode =>
               var i = 0; while( i < 4 ) {
                  n.child( i ) match {
                     case ne: QNonEmpty => in += ne   // sucky `enqueue` creates intermediate Seq because of varargs
                     case _ =>
                  }
               i += 1 }
         }
      }
   }

   private def nn( point: D#Point, metric: DistanceMeasure[ D ]) : QLeaf = {
      var bestLeaf: QLeaf     = null
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

      @tailrec def findNNTail( n0: QNode ) {
         numAcceptedChildren = 0
         var accept1Idx = 0
         val oldRMax1 = rmax
         var i = 0; while( i < 4 ) {
            n0.child( i ) match {
               case l: QLeaf =>
                  val ldist = metric.distance( point, pointView( l.value ))
                  if( ldist < bestDist ) {
                     bestDist = ldist
                     bestLeaf = l
                     if( bestDist < rmax ) {
//println( "      : leaf " + l.point + " - " + bestDist )
                        rmax = bestDist
                     }
                  }

               case c: QNode =>
                  val cq            = c.hyperCube
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
         val qdn  = dn.hyperCube
         var succ = n0.next
         while( succ != null ) {
            succ.child( accept1Idx ) match {
               case dn2: QNode if( dn2.hyperCube == qdn ) =>
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
         findNNTail( n0 )
         if( bestDist <= 0L ) return bestLeaf
         var i = 0; while( i < numAcceptedChildren ) {
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
      sys.error( "never here" )
   }

   private final class VisitedNode( val n: QNode, val minDist: Long ) extends Ordered[ VisitedNode ] {
      def compare( that: VisitedNode ) = -(minDist.compareTo( that.minDist ))
   }
}