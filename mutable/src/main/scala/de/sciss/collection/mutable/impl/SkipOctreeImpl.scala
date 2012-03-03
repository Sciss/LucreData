/*
 *  SkipOctreeImpl.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

//object SkipOctreeImpl {
//   var RANGE_LEFT    = 0
//   var RANGE_RIGHT   = 0
//}
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

   final def removeAt( point: D#PointLike ) : Option[ A ] = {
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

   final override def isDefinedAt( point: D#PointLike ) : Boolean = {
      if( !hyperCube.contains( point )) return false
      findLeaf( point ) != null
   }

   final def get( point: D#PointLike ) : Option[ A ] = {
      if( !hyperCube.contains( point )) return None
      val l = findLeaf( point )
      if( l == null ) None else Some( l.value )
   }

   final override def nearestNeighbor[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ]) : A = {
      val res = new NN( point, metric ).find
      if( res != null ) res.value else throw new NoSuchElementException( "nearestNeighbor on an empty tree" )
   }

   final def nearestNeighborOption[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ]) : Option[ A ] = {
      val res = new NN( point, metric ).find
      if( res != null ) Some( res.value ) else None
   }

   final override def isEmpty : Boolean = {
      val n = headTree
      var i = 0; while( i < numOrthants ) {
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

   final def rangeQuery[ @specialized( Long ) Area ]( qs: QueryShape[ Area, D ]) : Iterator[ A ] = new RangeQuery( qs )

   protected def insertLeaf( elem: A ) : QLeaf
   protected def removeLeaf( point: D#PointLike ) : QLeaf
   protected def findLeaf( point: D#PointLike ) : QLeaf

   private final class RangeQuery[ @specialized( Long ) Area ]( qs: QueryShape[ Area, D ]) extends Iterator[ A ] {
      val stabbing      = MQueue.empty[ (QNode, Area) ]  // Tuple2 is specialized for Long, too!
      val in            = MQueue.empty[ QNonEmpty ]
      var current : A   = _      // overwritten by initial run of `findNextValue`
      var hasNext       = true   // eventually set to `false` by `findNextValue`

      stabbing += headTree -> qs.overlapArea( headTree.hyperCube )
      findNextValue()

      // search downwards:
      // "At each square q ∈ Qi we either go to a child square in Qi
      // that covers the same area of R ∪ A as p does, if such a child
      // square exists, or jump to the next level q ∈ Qi−1."
      @tailrec def findEquiStabbingTail( node: QNode, area: Area ) : QNode = {
         var pi = node
         var i = 0; while( i < numOrthants ) {
            pi.child( i ) match {
               case pic: QNode =>
                  val a2 = qs.overlapArea( pic.hyperCube )
                  if( a2 == area ) {
                     pi = pic
                     i  = 0   // start over in child
                  } else {
                     i += 1
                  }
               case _ => i += 1
            }
         }
         // ... or jump to the next (previous) level
         val prev = pi.prev
         if( prev == null ) pi else findEquiStabbingTail( prev, area )
      }

      // the movement from Q0 to Qj
      // "assuming that p is not critical in Q0, we promote to Qj where Qj is the highest
      // level in which p is not a critical square"
      //
      // definition of critical square:
      // "a stabbing node of Qi whose child nodes are either not stabbing, or still
      // stabbing but cover less volume of R than p does."
      // ; bzw. umgedreht: eine unkritische node ist eine, in der es mindestens eine stabbing node
      // mit derselben ueberlappungsflaeche gibt!
      //
      // definition stabbing: 0 < overlap-area < area-of-p
      def findHighestUncritical( p0: QNode, area: Area ) : QNode = {
         var pi         = p0.next
         if( pi == null ) return p0
         var uncritical = p0
         var i = 0
         while( i < numOrthants ) {
            pi.child( i ) match {
               case ci: QNode =>
                  val a2 = qs.overlapArea( ci.hyperCube )
                  if( a2 == area ) {   // that means node is uncritical
                     uncritical  = pi
                     pi          = pi.next
                     if( pi == null ) return uncritical
                     i           = 0   // restart in next level
                  } else {
                     i          += 1
                  }
               case _ => i += 1
            }
         }
         uncritical
      }

      def next() : A = {
         if( !hasNext ) throw new NoSuchElementException( "next on empty iterator" )
         val res = current
         findNextValue()
         res
      }

      def findNextValue() { while( true ) {
         if( in.isEmpty ) {
            if( stabbing.isEmpty ) {
               hasNext = false
               return
            }
            val tup  = stabbing.dequeue()
            val ns   = tup._1                            // stabbing node
            val as   = tup._2                            // overlapping area with query shape
            val hi   = findHighestUncritical( ns, as )   // find highest uncritical hyper-cube of the stabbing node
            val nc   = findEquiStabbingTail( hi, as )    // now traverse towards Q0 to find the critical square

            var i = 0; while( i < numOrthants ) {
               nc.child( i ) match {
                  case cl: QLeaf =>
                     if( qs.contains( pointView( cl.value ))) in += cl
                  case cn: QNode =>
                     val q    = cn.hyperCube
                     val ao   = qs.overlapArea( q )
                     // test for stabbing or inclusion:
                     // inclusion: overlap-area == area-of-p
                     // stabbing: 0 < overlap-area < area-of-p
                     if( qs.isAreaNonEmpty( ao )) {      // q is _not_ out
                        if( qs.isAreaGreater( q, ao )) { // q is stabbing
                           stabbing += cn -> ao
                        } else {                         // q is in
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
               var i = 0; while( i < numOrthants ) {
                  n.child( i ) match {
                     case ne: QNonEmpty => in += ne   // sucky `enqueue` creates intermediate Seq because of varargs
                     case _ =>
                  }
               i += 1 }
         }
      }}
   }

   private final class NN[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ]) {
      private var bestLeaf: QLeaf     = null
      private var bestDist            = metric.maxValue // Long.MaxValue   // all distances here are squared!
      private val pri                 = PriorityQueue.empty[ VisitedNode ]
      private val acceptedChildren    = new Array[ VisitedNode ]( numOrthants )
      private var numAcceptedChildren = 0
      private var rmax                = metric.maxValue // Long.MaxValue

      def recheckRMax {
         var j = 0; while( j < numAcceptedChildren ) {
//            if( space.bigGt( acceptedChildren( j ).minDist, rmax )) { ... }
            if( metric.isMeasureGreater( acceptedChildren( j ).minDist, rmax )) {  // immediately kick it out
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
         var i = 0; while( i < numOrthants ) {
            n0.child( i ) match {
               case l: QLeaf =>
                  val ldist = metric.distance( point, pointView( l.value ))
//                  if( space.bigGt( bestDist, ldist )) { ... }
                  if( metric.isMeasureGreater( bestDist, ldist )) {
                     bestDist = ldist
                     bestLeaf = l
//                     if( space.bigGt( rmax, bestDist )) { ... }
                     if( metric.isMeasureGreater( rmax, bestDist )) {
//println( "      : leaf " + l.point + " - " + bestDist )
                        rmax = bestDist
                     }
                  }

               case c: QNode =>
                  val cq            = c.hyperCube
                  val cMinDist      = metric.minDistance( point, cq )
//                  if( space.bigGeq( rmax, cMinDist )) { ... }
                  if( !metric.isMeasureGreater( cMinDist, rmax )) {   // otherwise we're out already
                     val cMaxDist   = metric.maxDistance( point, cq )
//                     if( space.bigGt( rmax, cMaxDist )) { ... }
                     if( metric.isMeasureGreater( rmax, cMaxDist )) {
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

      def find : QLeaf = {
         var n0 = headTree
         while( true ) {
            findNNTail( n0 )
//            if( space.bigLeqZero( bestDist )) return bestLeaf
//            if( bestDist == metric.minValue ) return bestLeaf
            if( metric.isMeasureZero( bestDist )) return bestLeaf
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
//            while( space.bigGt( vis.minDist, rmax ))
            } while( metric.isMeasureGreater( vis.minDist, rmax ))
            n0 = vis.n
         }
         sys.error( "never here" )
      }

      private final class VisitedNode( val n: QNode, val minDist: M ) extends Ordered[ VisitedNode ] {
         def compare( that: VisitedNode ) = metric.compareMeasure( that.minDist, minDist )
      }
   }
}