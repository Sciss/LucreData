/*
 *  DeterministicSkipOctree.scala
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
package txn

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.mutable.{PriorityQueue, Queue => MQueue}
import annotation.tailrec
import geom.{QueryShape, DistanceMeasure, Space}
import de.sciss.lucrestm.{DataOutput, DataInput, Serializer, Sys}

/**
 * A transactional determinstic skip octree as outlined in the paper by Eppstein et al.
 * It is constructed from a given space (dimensions) and a skip-gap parameter
 * which determines the kind of skip list which is used to govern the
 * level decimation.
 *
 * The tree is a mutable data structure which supports lookup, insertion and removal
 * in O(log n), as well as efficient range queries and nearest neighbour search.
 *
 * The current implementation, backed by `impl.SkipOctreeImpl`, uses the types of
 * the `geom` package, assuming that coordinates are integers, with the maximum
 * root hyper-cube given by a span from `0` to `0x7FFFFFFF` (e.g. in `Space.TwoDim`,
 * this is `Square( 0x40000000, 0x40000000, 0x40000000 )`.
 */
object DeterministicSkipOctree {
   def empty[ S <: Sys[ S ], D <: Space[ D ], A ]( space: D, hyperCube: D#HyperCube, skipGap: Int = 2 )
                                                 ( implicit view: A => D#Point, tx: S#Tx, system: S,
                                                   smf: Manifest[ S ],
                                                   dmf: Manifest[ D ],
                                                   amf: Manifest[ A ]) : DeterministicSkipOctree[ S, D, A ] = {

      new Impl[ S, D, A ]( space, hyperCube, view, { implicit impl =>
         import impl.{numOrthants, topNodeSer, rightNodeSer}
         val rootOrder     = TotalOrder.empty[ S ]().root
         val children      = new Array[ LeftChild[ S, D, A ]]( numOrthants )
         val headRight     = system.newRef[ RightNode[ S, D, A ]]( null )
         val head          = new TopLeftNode[ S, D, A ]( hyperCube, rootOrder, children, headRight )
         val lastTreeRef   = system.newRef[ TopNode[ S, D, A ]]( head )
         val skipList      = HASkipList.empty[ S, Leaf[ S, D, A ]]( skipGap, impl )
         (head, lastTreeRef, skipList)
      })
   }

//   def apply[ S <: Sys[ S ], D <: Space[ D ], A ]( space: D, hyperCube: D#HyperCube, skipGap: Int = 2 )
//                                                 ( xs: A* )( implicit view: A => D#Point, tx: S#Tx,
//                                                  system: S ) : SkipOctree[ S, D, A ] = {
//      val t = empty[ S, D, A ]( space, hyperCube, skipGap )
//      xs.foreach( t.+=( _ ))
//      t
//   }

   private type Order[ S <: Sys[ S ]] = TotalOrder.SetEntry[ S ]

   private final class NodeSer[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Serializer[ Node[ S, D, A ]] {
      def read( in: DataInput ) : Node[ S, D, A ] = sys.error( "TODO" )
      def write( node: Node[ S, D, A ], out: DataOutput ) {
         sys.error( "TODO" )
      }
   }

   private final class TopNodeSer[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Serializer[ TopNode[ S, D, A ]] {
      def read( in: DataInput ) : TopNode[ S, D, A ] = sys.error( "TODO" )
      def write( node: TopNode[ S, D, A ], out: DataOutput ) {
         sys.error( "TODO" )
      }
   }

   private final class LeftNodeSer[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Serializer[ LeftNode[ S, D, A ]] {
      def read( in: DataInput ) : LeftNode[ S, D, A ] = sys.error( "TODO" )
      def write( node: LeftNode[ S, D, A ], out: DataOutput ) {
         sys.error( "TODO" )
      }
   }

   private final class RightNodeSer[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Serializer[ RightNode[ S, D, A ]] {
      def read( in: DataInput ) : RightNode[ S, D, A ] = sys.error( "TODO" )
      def write( node: RightNode[ S, D, A ], out: DataOutput ) {
         sys.error( "TODO" )
      }
   }

   private final class Impl[ S <: Sys[ S ], D <: Space[ D ], A ]
      ( val space: D, val hyperCube: D#HyperCube, val pointView: A => D#Point,
        _initFun: Impl[ S, D, A ] => (TopLeftNode[ S, D, A ], S#Ref[ TopNode[ S, D, A ]], SkipList[ S, Leaf[ S, D, A ]]))
      ( implicit val system: S )
   extends DeterministicSkipOctree[ S, D, A ]
   with SkipList.KeyObserver[ S#Tx, Leaf[ S, D, A ]]
   with Ordering[ S#Tx, Leaf[ S, D, A ]]
   with Serializer[ Leaf[ S, D, A ]] {
      // tree =>

      implicit private def impl = this

      implicit val leftNodeSer: Serializer[ LeftNode[ S, D, A ]] = new LeftNodeSer[ S, D, A ]
      implicit val rightNodeSer: Serializer[ RightNode[ S, D, A ]] = new RightNodeSer[ S, D, A ]
      implicit val topNodeSer: Serializer[ TopNode[ S, D, A ]] = new TopNodeSer[ S, D, A ]
      implicit val nodeSer: Serializer[ Node[ S, D, A ]]       = new NodeSer[ S, D, A ]

      val (headTree, lastTreeRef, skipList) = _initFun( this )

      def numOrthants: Int = 1 << space.dim  // 4 for R2, 8 for R3, 16 for R4, etc.
//      val totalOrder = TotalOrder.empty[ S ]()

//      private var lastTree: TopNode = TopLeftNode
//      private val skipList: SkipList[ Leaf ] = {
////         implicit def maxKey = MaxKey( MaxLeaf )
//         HASkipList.empty[ Leaf ]( _skipGap, KeyObserver )
//      }

//      def head : Node = TopLeftNode
//      def lastTree : Node = lastTree
      
      def lastTree( implicit tx: S#Tx ) : TopNode[ S, D, A ] = lastTreeRef.get
      def lastTree_=( node: TopNode[ S, D, A ])( implicit tx: S#Tx ) {
         lastTreeRef.set( node )
      }

      def add( elem: A )( implicit tx: S#Tx ) : Boolean = {
         val oldLeaf = insertLeaf( elem )
         if( oldLeaf == null ) true else oldLeaf.value != elem
      }

      def update( elem: A )( implicit tx: S#Tx ) : Option[ A ] = {
         val oldLeaf = insertLeaf( elem )
         if( oldLeaf == null ) None else Some( oldLeaf.value )
      }

      def remove( elem: A )( implicit tx: S#Tx ) : Boolean = {
         val oldLeaf = removeLeaf( pointView( elem ))
         oldLeaf != null
      }

      def removeAt( point: D#Point )( implicit tx: S#Tx ) : Option[ A ] = {
         val oldLeaf = removeLeaf( point )
         if( oldLeaf == null ) None else Some( oldLeaf.value )
      }

      def contains( elem: A )( implicit tx: S#Tx ) : Boolean = {
         val point = pointView( elem )
         if( !hyperCube.contains( point )) return false
         val l = findLeaf( point )
         if( l == null ) false else l.value == elem
      }

      def isDefinedAt( point: D#Point )( implicit tx: S#Tx ) : Boolean = {
         if( !hyperCube.contains( point )) return false
         findLeaf( point ) != null
      }

      def get( point: D#Point )( implicit tx: S#Tx ) : Option[ A ] = {
         if( !hyperCube.contains( point )) return None
         val l = findLeaf( point )
         if( l == null ) None else Some( l.value )
      }

      def nearestNeighbor[ @specialized( Long ) M ]( point: D#Point, metric: DistanceMeasure[ M, D ])
                                                   ( implicit tx: S#Tx ) : A = {
         val res = new NN( point, metric ).find()
         if( res != null ) res.value else throw new NoSuchElementException( "nearestNeighbor on an empty tree" )
      }

      def nearestNeighborOption[ @specialized( Long ) M ]( point: D#Point, metric: DistanceMeasure[ M, D ])
                                                         ( implicit tx: S#Tx ) : Option[ A ] = {
         val res = new NN( point, metric ).find()
         if( res != null ) Some( res.value ) else None
      }

      def isEmpty( implicit tx: S#Tx ) : Boolean = {
         val n = headTree
         val sz = numOrthants
         var i = 0; while( i < sz ) {
            if( !n.child( i ).isEmpty ) return false
         i += 1 }
         true
      }

      def numLevels( implicit tx: S#Tx ) : Int = {
         var n: Node[ S, D, A ] = headTree
         val t = lastTree
         var i = 1; while( !(n eq t) ) {
            n = n.next
            i += 1
         }
         i
      }

      def +=( elem: A )( implicit tx: S#Tx ) : this.type = {
         insertLeaf( elem )
         this
      }

      def -=( elem: A )( implicit tx: S#Tx ) : this.type = {
         removeLeaf( pointView( elem ))
         this
      }

      def rangeQuery[ @specialized( Long ) Area ]( qs: QueryShape[ Area, D ])( implicit tx: S#Tx ) : Iterator[ A ] = {
         val q = new RangeQuery( qs )
         q.findNextValue()
         q
      }

      def toIndexedSeq( implicit tx: S#Tx ) : IIdxSeq[ A ] = iterator.toIndexedSeq
      def toList( implicit tx: S#Tx ) : List[ A ] = iterator.toList
      def toSeq(  implicit tx: S#Tx ) : Seq[  A ] = iterator.toSeq
      def toSet(  implicit tx: S#Tx ) : Set[  A ] = iterator.toSet

      private def findLeaf( point: D#Point )( implicit tx: S#Tx ) : Leaf[ S, D, A ] = {
         val p0 = lastTree.findP0( point )
         p0.findImmediateLeaf( point )
      }

      private def insertLeaf( elem: A )( implicit tx: S#Tx ) : Leaf[ S, D, A ] = {
         val point   = pointView( elem )
         require( hyperCube.contains( point ), point.toString + " lies out of root hyper-cube " + hyperCube )

         val p0      = lastTree.findP0( point )
         val oldLeaf = p0.findImmediateLeaf( point )
         if( oldLeaf == null ) {
            val leaf = p0.insert( point, elem )
            skipList.add( leaf )
         } else {
//            oldLeaf.value = elem
            // remove previous leaf
            removeImmediateLeaf( oldLeaf )
            // search anew
            val p0b        = lastTree.findP0( point )
            assert( p0b.findImmediateLeaf( point ) == null )
            val leaf = p0.insert( point, elem )
            skipList.add( leaf )
         }
         oldLeaf
      }

      private def removeLeaf( point: D#Point )( implicit tx: S#Tx ) : Leaf[ S, D, A ] = {
         if( !hyperCube.contains( point )) return null

         // "To insert or delete a point y into or from S, we first search the
         // quadtree structure to locate y in each Qi ..."
         val p0 = lastTree.findP0( point )

         // "... Then we insert or delete y
         // in the binary Q0 and update our total order."

         val l = p0.findImmediateLeaf( point )
         if( l != null ) {
            removeImmediateLeaf( l )
         }
         l
      }

      private def removeImmediateLeaf( l: Leaf[ S, D, A ])( implicit tx: S#Tx ) {
         // this will trigger removals from upper levels
         skipList.remove( l )
         // be careful: p0 at this point may be invalid
         // as the skiplist removal might have merged
         // it with its parent(s). we thus need to find
         // the parent of l in Q0 again!

         val p = l.parent
         p.removeImmediateLeaf( l )
         assert( l.parent == null, "Internal error - leaf should be removed by now : " + l )
      }

      def iterator( implicit tx: S#Tx ) : Iterator[ A ] = skipList.iterator.map( _.value )

//      private final class Iter( underlying: Iterator[ Leaf[ S, D, A ]]) extends Iterator[ A ] {
//         def next() : A = {
//            val leaf = underlying.next()
//            leaf.value
//         }
//         def hasNext : Boolean = underlying.hasNext
//      }

      // ---- Serializer[ Leaf[ ... ]] ----

      def read( in: DataInput ) : Leaf[ S, D, A ] = {
         sys.error( "TODO" )
      }

      def write( leaf: Leaf[ S, D, A ], out: DataOutput ) {
         sys.error( "TODO" )
      }

      // ---- Ordering[ Leaf[ ... ]] ----

      /**
       * Leafs are ordered by the tree's in-order traversal,
       * where the quadrants I+II and III+IV can be thought
       * of as dummy nodes to binarize the octree. That is
       * to say, in a node, the child order corresponds to
       * their quadrant indices (I < II < III < IV).
       */
      def compare( a: Leaf[ S, D, A ], b: Leaf[ S, D, A ])( implicit tx: S#Tx ) : Int = {
//         order.compare( that.order )
         val t1 = a.order.tag
         val t2 = b.order.tag
         if( t1 < t2 ) -1 else if( t1 > t2 ) 1 else 0
      }


      // ---- KeyObserver ----

      def keyUp( l: Leaf[ S, D, A ])( implicit tx: S#Tx ) {
//println( "up : " + l )
         // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
         //  then traverse upwards in Qi until we find the lowest
         //  ancestor q of x which is also interesting in Qi+1.
         //  (This is the reversed process of searching x in Qi
         //  with q = pi,start = pi+1,end so it takes at most 6
         //  steps by Lemma 5.) Then we go to the same square q
         //  in Qi+1 and insert x."

         val pNext0     = l.parent.findPN // ( path, 0 )
         val pNext      = if( pNext0 == null ) { // create new level
            val sz      = numOrthants
            val ch      = new Array[ RightChild[ S, D, A ]]( sz )
            val nextRef = system.newRef[ RightNode[ S, D, A ]]( null )
            val prev    = lastTree
            val res     = new TopRightNode( hyperCube, prev, ch, nextRef )
            prev.next   = res
            lastTree    = res
            res
         } else pNext0
         pNext.insert( l ) // , path )
      }

      def keyDown( l: Leaf[ S, D, A ])( implicit tx: S#Tx ) {
//println( "down : " + l )
         // "To delete x from Qi we go from xi to the smallest interesting
         //  square pi(x) containing x in Qi following the pointers. Then
         //  the deletion given pi(x) is as described in Section 2.3."

         val p = l.parent
         p.removeImmediateLeaf( l )
      }
   }

   private final class NN[ S <: Sys[ S ], D <: Space[ D ], A, @specialized( Long ) M ](
      point: D#Point, metric: DistanceMeasure[ M, D ])( implicit impl: Impl[ S, D, A ])
   extends scala.math.Ordering[ VisitedNode[ S, D, A, M ]] {
      private type Vis = VisitedNode[ S, D, A, M ]

      import impl.{headTree, numOrthants, pointView}
      private val sz                         = numOrthants
      private var bestLeaf: Leaf[ S, D, A ]  = null
      private var bestDist                   = metric.maxValue // Long.MaxValue   // all distances here are squared!
      private val pri                        = PriorityQueue.empty[ Vis ]( this )
      private val acceptedChildren           = new Array[ Vis ]( sz )
      private var numAcceptedChildren        = 0
      private var rmax                       = metric.maxValue // Long.MaxValue

      def recheckRMax() {
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

      @tailrec def findNNTail( n0: Node[ S, D, A ])( implicit tx: S#Tx ) {
         numAcceptedChildren = 0
         var accept1Idx = 0
         val oldRMax1 = rmax
         var i = 0; while( i < sz ) {
            val n0c = n0.child( i )
            if( n0c.isLeaf ) {
               val l = n0c.asLeaf
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
            } else if( n0c.isNode ) {
               val c             = n0c.asNode
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
                  acceptedChildren( numAcceptedChildren ) = new VisitedNode[ S, D, A, M ]( c, cMinDist /*, cMaxDist */)
                  accept1Idx = i
                  numAcceptedChildren += 1
               }
            }
         i += 1 }

         if( rmax != oldRMax1 ) recheckRMax()

         // Unless exactly one child is accepted, round is over
         if( numAcceptedChildren != 1 ) return

         // Otherwise find corresponding node in highest level, and descend
         var dn   = acceptedChildren( 0 ).n
         val qdn  = dn.hyperCube
         var succ = n0.next
         while( succ != null ) {
            val c = succ.child( accept1Idx )
            succ  = if( c.isNode ) {
               val dn2 = c.asNode
               if( dn2.hyperCube == qdn ) {
                  dn    = dn2
                  succ.next
               } else {
                  null
               }
            } else {
               null
            }
         }

         // now go left
         @tailrec def findLeft( n: Node[ S, D, A ]) : Node[ S, D, A ] = {
            val prev = n.prev
            if( prev == null ) n else findLeft( prev )
         }
//         while( dn.prev != null ) dn = dn.prev
//         findNNTail( dn )
         findNNTail( findLeft( dn ))
      }

      def find()( implicit tx: S#Tx ) : Leaf[ S, D, A ] = {
         var n0: Node[ S, D, A ] = headTree
         while( true ) {
            findNNTail( n0 )
//            if( space.bigLeqZero( bestDist )) return bestLeaf
//            if( bestDist == metric.minValue ) return bestLeaf
            if( metric.isMeasureZero( bestDist )) return bestLeaf
            var i = 0; while( i < numAcceptedChildren ) {
               pri += acceptedChildren( i )
            i += 1 }
            var vis: VisitedNode[ S, D, A, M ] = null
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

      def compare( a: Vis, b: Vis ) = metric.compareMeasure( b.minDist, a.minDist )
   }

   private final class VisitedNode[ S <: Sys[ S ], D <: Space[ D ], A, @specialized( Long ) M ]
   ( val n: Node[ S, D, A ], val minDist: M ) /* extends Ordered[ VisitedNode[ S, D, A, M ]] */ {
//      def compare( that: VisitedNode[ S, D, A, M ]) = metric.compareMeasure( that.minDist, minDist )
   }

   // note: Iterator is not specialized, hence we can safe use the effort to specialize in A anyway
   private final class RangeQuery[ S <: Sys[ S ], D <: Space[ D ], /* @specialized( Int, Long ) */ A,  @specialized( Long ) Area ]
   ( qs: QueryShape[ Area, D ])( implicit impl: Impl[ S, D, A ]) extends Iterator[ A ] {
      import impl.{headTree, numOrthants, pointView, system}

      val sz            = numOrthants
      val stabbing      = MQueue.empty[ (Node[ S, D, A ], Area) ]  // Tuple2 is specialized for Long, too!
      val in            = MQueue.empty[ NonEmpty[ S, D, A ]]
      var current : A   = _      // overwritten by initial run of `findNextValue`
      var hasNext       = true   // eventually set to `false` by `findNextValue`

      stabbing += headTree -> qs.overlapArea( headTree.hyperCube )
//      findNextValue()

      // search downwards:
      // "At each square q ∈ Qi we either go to a child square in Qi
      // that covers the same area of R ∪ A as p does, if such a child
      // square exists, or jump to the next level q ∈ Qi−1."
      @tailrec def findEquiStabbingTail( node: Node[ S, D, A ], area: Area ) : Node[ S, D, A ] = {
         var pi = node
         var i = 0; while( i < sz ) {
            val c = pi.child( i )
            if( c.isNode ) {
               val pic = c.asNode
               val a2 = qs.overlapArea( pic.hyperCube )
               if( a2 == area ) {
                  pi = pic
                  i  = 0   // start over in child
               } else {
                  i += 1
               }
            } else {
               i += 1
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
      def findHighestUncritical( p0: Node[ S, D, A ], area: Area )( implicit tx: S#Tx ) : Node[ S, D, A ] = {
         var pi         = p0.next
         if( pi == null ) return p0
         var uncritical = p0
         var i = 0
         while( i < sz ) {
            val c = pi.child( i )
            if( c.isNode ) {
               val ci = c.asNode
               val a2 = qs.overlapArea( ci.hyperCube )
               if( a2 == area ) {   // that means node is uncritical
                  uncritical  = pi
                  pi          = pi.next
                  if( pi == null ) return uncritical
                  i           = 0   // restart in next level
               } else {
                  i          += 1
               }
            } else {
               i += 1
            }
         }
         uncritical
      }

      def next() : A = {
         if( !hasNext ) throw new NoSuchElementException( "next on empty iterator" )
         val res = current
         system.atomic { implicit tx => findNextValue() }
         res
      }

      def nextTxn()( implicit tx: S#Tx ) : A = {
         if( !hasNext ) throw new NoSuchElementException( "next on empty iterator" )
         val res = current
         findNextValue()
         res
      }

      def findNextValue()( implicit tx: S#Tx ) { while( true ) {
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

            var i = 0; while( i < sz ) {
               val c = nc.child( i )
               if( c.isLeaf ) {
                  val cl = c.asLeaf
                  if( qs.contains( pointView( cl.value ))) in += cl
               } else if( c.isNode ) {
                  val cn = c.asNode
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
               }
            i += 1 }

         } else {
            val c = in.dequeue()
            if( c.isLeaf ) {
               val l = c.asLeaf
               current = l.value
               return
            } else {
               val n = c.asNode
               var i = 0; while( i < sz ) {
                  val cc = n.child( i )
                  if( !cc.isEmpty ) {
                     in += cc.asNonEmpty // sucky `enqueue` creates intermediate Seq because of varargs
                  }
                  i += 1 }
            }
         }
      }}
   }

   /**
    * A child is an object that can be
    * stored in a orthant of a node.
    */
   /* private */ sealed trait Child[ S <: Sys[ S ], D <: Space[ D ], A ] /* extends Q */ {
      def shortString : String
      def isEmpty : Boolean
      def isLeaf : Boolean
      def isNode : Boolean
      def asLeaf : Leaf[ S, D, A ]
      def asNode : Node[ S, D, A ]
      def asNonEmpty : NonEmpty[ S, D, A ]
   }
   /**
    * A left child is one which is stored in Q0.
    * This is either empty or an object (`LeftInnerNonEmpty`)
    * which provides start and stop markers for its
    * position in the in-order list.
    * That is, `LeftChild` is either of
    *
    * - `Empty`
    * - `Leaf`
    * - `InnerLeftNode`
    */
   /* private */ sealed trait LeftChild[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Child[ S, D, A ] {
      private[DeterministicSkipOctree] def asLeftInnerNonEmpty : LeftInnerNonEmpty[ S, D, A ]
   }

   /**
    * A right child is one which is stored in Qi, i > 0.
    * This is either empty or an instance of `RightInnerNonEmpty`.
    * That is, `RightChild` is either of
    *
    * - `Empty`
    * - `Leaf`
    * - `InnerRightNode`
    */
   /* private */ sealed trait RightChild[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Child[ S, D, A ] {
      private[DeterministicSkipOctree] def asRightInnerNonEmpty : RightInnerNonEmpty[ S, D, A ]
   }

   /**
    * A dummy object indicating a vacant orthant in a node.
    */
   private final class Empty[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends LeftChild[ S, D, A ] with RightChild[ S, D, A ] /* with QEmpty */ {
      def shortString = "empty"
      override def toString = shortString
      def isEmpty = true
      def isLeaf  = false
      def isNode = false
      def asLeaf : Leaf[ S, D, A ] = opNotSupported
      def asNode : Node[ S, D, A ] = opNotSupported
      def asNonEmpty : NonEmpty[ S, D, A ] = opNotSupported
      private[DeterministicSkipOctree] def asRightInnerNonEmpty : RightInnerNonEmpty[ S, D, A ] = opNotSupported
      private[DeterministicSkipOctree] def asLeftInnerNonEmpty : LeftInnerNonEmpty[ S, D, A ] = opNotSupported
   }

   /**
    * An object denoting a filled orthant of a node.
    * This is either a leaf or another node. This trait
    * supports the operations of calculating the greatest
    * interesting hyper-cube with regard to another point,
    * as well as determining its position within an
    * encompassing hyper-cube.
    */
   /* private */ sealed trait NonEmpty[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Child[ S, D, A ] {
      /**
       * Computes the greatest interesting hyper-cube within
       * a given hyper-cube `mq` so that this (leaf's or node's)
       * hyper-cube and the given point will be placed in
       * separated orthants of this resulting hyper-cube.
       */
      private[DeterministicSkipOctree] def union( mq: D#HyperCube, point: D#Point )( implicit impl: Impl[ S, D, A ]) : D#HyperCube

      /**
       * Queries the orthant index for this (leaf's or node's) hyper-cube
       * with respect to a given outer hyper-cube `iq`.
       */
      private[DeterministicSkipOctree] def orthantIndexIn( iq: D#HyperCube )( implicit impl: Impl[ S, D, A ]) : Int

      final def isEmpty = false
      final def asNonEmpty : NonEmpty[ S, D, A ] = this
   }

   /**
    * A tree element in Q0 has markers for the
    * in-order traversal.
    */
   /* private */ sealed trait LeftNonEmpty[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends NonEmpty[ S, D, A ] {
      /**
       * A marker in the in-order list corresponding to
       * the beginning of the objects 'interval'. That is
       * to say, if this object is a leaf, this marker is
       * the leaf's position in the in-order list. If this
       * object is a node, all children of the node's subtree
       * appear right to this marker in the in-order. Thus
       * the `startOrder` and `stopOrder` form the interval
       * borders of the sub-tree.
       */
      private[DeterministicSkipOctree] def startOrder: TotalOrder.SetEntry[ S ]
      /**
       * A marker in the in-order list corresponding to
       * the ending of the objects 'interval'. That is
       * to say, if this object is a leaf, this marker is
       * the leaf's position in the in-order list. If this
       * object is a node, all children of the node's subtree
       * appear left to this marker in the in-order. Thus
       * the `startOrder` and `stopOrder` form the interval
       * borders of the sub-tree.
       */
      private[DeterministicSkipOctree] def stopOrder: TotalOrder.SetEntry[ S ]
   }

   /**
    * A common trait used in pattern matching, comprised of `Leaf` and `InnerLeftNode`.
    */
   /* private */ sealed trait LeftInnerNonEmpty[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends LeftNonEmpty[ S, D, A ] with InnerNonEmpty[ S, D, A ] with LeftChild[ S, D, A ] {
      private[DeterministicSkipOctree] def parentLeft_=( p: LeftNode[ S, D, A ])( implicit tx: S#Tx ) : Unit
      final private[DeterministicSkipOctree] def asLeftInnerNonEmpty : LeftInnerNonEmpty[ S, D, A ] = this
   }

   /**
    * A common trait used in pattern matching, comprised of `Leaf` and `InnerRightNode`.
    */
   /* private */ sealed trait RightInnerNonEmpty[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends InnerNonEmpty[ S, D, A ] with RightChild[ S, D, A ] {
      private[DeterministicSkipOctree] def parentRight_=( p: RightNode[ S, D, A ])( implicit tx: S#Tx ) : Unit
      final private[DeterministicSkipOctree] def asRightInnerNonEmpty : RightInnerNonEmpty[ S, D, A ] = this
   }

   /**
    * A leaf in the octree, carrying a map entry
    * in the form of a point and associated value.
    * Note that a single instance of a leaf is used
    * across the levels of the octree! That means
    * that multiple child pointers may go to the
    * same leaf, while the parent of a leaf always
    * points into the highest level octree that
    * the leaf resides in, according to the skiplist.
    */
   /* private */ final class Leaf[ S <: Sys[ S ], D <: Space[ D ], A ](
      val point: D#Point, val value: A, val order: Order[ S ], parentRef: S#Ref[ Node[ S, D, A ]])
   extends LeftInnerNonEmpty[ S, D, A ] with RightInnerNonEmpty[ S, D, A ] /* with Ordered[ Leaf[ S, D, A ]] */ /* with QLeaf */ {
//      private var parentVar: Node[ S, D, A ] = null

      private[DeterministicSkipOctree] def parentLeft_=( p: LeftNode[ S, D, A ])( implicit tx: S#Tx )   { parent_=( p )}
      private[DeterministicSkipOctree] def parentRight_=( p: RightNode[ S, D, A ])( implicit tx: S#Tx ) { parent_=( p )}
      private[DeterministicSkipOctree] def parent( implicit tx: S#Tx ): Node[ S, D, A ] = parentRef.get
      private[DeterministicSkipOctree] def parent_=( p: Node[ S, D, A ])( implicit tx: S#Tx ) { parentRef.set( p )}

      def isLeaf  = true
      def isNode  = false
      def asLeaf : Leaf[ S, D, A ]  = this
      def asNode : Node[ S, D, A ]  = opNotSupported

      private[DeterministicSkipOctree] def union( mq: D#HyperCube, point2: D#Point )( implicit impl: Impl[ S, D, A ]) = {
         import impl.pointView
         val point   = pointView( value )
         mq.greatestInteresting( point, point2 )
      }

      private[DeterministicSkipOctree] def orthantIndexIn( iq: D#HyperCube )( implicit impl: Impl[ S, D, A ]) : Int = {
         import impl.pointView
         iq.indexOf( pointView( value ))
      }

      /**
       * For a leaf (which does not have a subtree),
       * the `startOrder` is identical to its `order`.
       */
      private[DeterministicSkipOctree] def startOrder : Order[ S ] = order

      /**
       * For a leaf (which does not have a subtree),
       * the `stopOrder` is identical to its `order`.
       */
      private[DeterministicSkipOctree] def stopOrder : Order[ S ] = order

      def shortString = "leaf(" + point + ")"
      override def toString = "Leaf(" + point + ", " + value + ")"

      private[DeterministicSkipOctree] def dispose()( implicit tx: S#Tx, impl: Impl[ S, D, A]) {
         import impl.system
//         parentVar   = null
         parentRef.dispose()
//         system.disposeRef( parentRef )
//            value       = null.asInstanceOf[ A ]
         order.remove()
      }
   }

   /**
    * Nodes are defined by a hyperCube area as well as a list of children,
    * as well as a pointer `next` to the corresponding node in the
    * next highest tree. A `Node` also provides various search methods.
    */
   /* private */ sealed trait Node[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends NonEmpty[ S, D, A ] /* with QNode */ {
      /**
       * Returns the child for a given
       * orthant index
       */
      def child( idx: Int ) : Child[ S, D, A ]

      /**
       * Finds to smallest interesting hyper-cube
       * in Q0, containing a given point. This method
       * traverses downwards into its children, or,
       * if the "bottom" has been reached, tries to
       * continue in Qi-1.
       *
       * @return  the node defined by the given search `point`, or `null`
       *          if no such node exists.
       */
      private[DeterministicSkipOctree] def findP0( point: D#Point ) : LeftNode[ S, D, A ]

      /**
       * Assuming that the given `leaf` is a child of this node,
       * removes the child from this node's children. This method
       * will perform further clean-up such as merging this node
       * with its parent if it becomes uninteresting as part of the
       * removal.
       */
      private[DeterministicSkipOctree] def removeImmediateLeaf( leaf: Leaf[ S, D, A ])
                                                              ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) : Unit

      /**
       * Returns the hyper-cube covered by this node
       */
      def hyperCube: D#HyperCube

      /**
       * Returns the corresponding interesting
       * node in Qi+1, or `null` if no such
       * node exists.
       */
      final private[DeterministicSkipOctree] def next( implicit tx: S#Tx ) : RightNode[ S, D, A ] = nextRef.get

      final def nextOption( implicit tx: S#Tx ) : Option[ Node[ S, D, A ]] = Option( next )

      /**
       * Sets the corresponding interesting
       * node in Qi+1.
       */
      final private[DeterministicSkipOctree] def next_=( node: RightNode[ S, D, A ])( implicit tx: S#Tx ) {
         nextRef.set( node )
      }

      protected def nextRef: S#Ref[ RightNode[ S, D, A ]]

      private[DeterministicSkipOctree] def prev: Node[ S, D, A ]

      private[DeterministicSkipOctree] final def union( mq: D#HyperCube, point2: D#Point )
                                                      ( implicit impl: Impl[ S, D, A ]) = {
         val q = hyperCube
         mq.greatestInteresting( q, point2 )
      }

      private[DeterministicSkipOctree] final def orthantIndexIn( iq: D#HyperCube )
                                                               ( implicit impl: Impl[ S, D, A ]) : Int = iq.indexOf( hyperCube )

      /**
       * The reverse process of `findP0`: Finds the lowest
       * common ancestor interesting node of this node
       * which is also contained in Qi+1. Returns this node
       * in Qi+1, or null if no such node exists.
       */
      private[DeterministicSkipOctree] def findPN( implicit tx: S#Tx ) : RightNode[ S, D, A ]

      /**
       * Called when a leaf has been removed from the node.
       * The node may need to cleanup after this, e.g. promote
       * an underfull node upwards.
       */
      protected def leafRemoved()( implicit tx: S#Tx, impl: Impl[ S, D, A ]) : Unit

      def nodeName : String
      final def shortString = nodeName + "(" + hyperCube + ")"

//      override def toString = shortString +
//         Seq.tabulate( numOrthants )( i => child( i ).shortString )
//            .mkString( " : children = [", ", ", "]" )

//      final def prevOption: Option[ Node[ S, D, A ]] = Option( prev )
//      final def nextOption: Option[ Node[ S, D, A ]] = Option( next )

      final def isLeaf  = false
      final def isNode  = true
      final def asNode : Node[ S, D, A ] = this
      final def asLeaf : Leaf[ S, D, A ] = opNotSupported
   }

   /**
    * An inner non empty tree element has a mutable parent node.
    */
   /* private */ sealed trait InnerNonEmpty[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends NonEmpty[ S, D, A ] {
      private[DeterministicSkipOctree] def parent( implicit tx: S#Tx ): Node[ S, D, A ]
   }

   /**
    * Utility trait which elements the rightward search `findPN`.
    */
   private sealed trait InnerNode[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Node[ S, D, A ] with InnerNonEmpty[ S, D, A ] {
      private[DeterministicSkipOctree] final def findPN( implicit tx: S#Tx ) : RightNode[ S, D, A ] = {
         val n = next
         if( n == null ) parent.findPN else n
      }
   }

   /**
    * A right tree node implementation provides more specialized child nodes
    * of type `RightChild`. It furthermore defines the node in Qi-1 via the
    * `prev` method.
    */
   /* private */ sealed trait RightNode[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Node[ S, D, A ] with NonEmpty[ S, D, A ] {
//      final val children = Array.fill[ RightChild[ S, D, A ]]( numOrthants )( Empty )
      private[DeterministicSkipOctree] def children: Array[ RightChild[ S, D, A ]]
//      private[DeterministicSkipOctree] final var next : RightNode[ S, D, A ] = null
//      final private[DeterministicSkipOctree] def next( implicit tx: S#Tx ) : RightNode[ S, D, A ] = nextRef.get
//      final private[DeterministicSkipOctree] def next_=( node: RightNode[ S, D, A ])( implicit tx: S#Tx ) {
//         nextRef.set( node )
//      }
//
//      protected def nextRef: S#Ref[ RightNode[ S, D, A ]]

      private[DeterministicSkipOctree] def prev : Node[ S, D, A ]
      final def child( idx: Int ) : RightChild[ S, D, A ] = children( idx )

      private[DeterministicSkipOctree] final def findP0( point: D#Point ) : LeftNode[ S, D, A ] = {
         val qidx = hyperCube.indexOf( point )
         val c = children( qidx )
         val n = if( c.isNode ) {
            val cn = c.asNode
            if( cn.hyperCube.contains( point )) cn else prev
         } else {
            prev
         }
         n.findP0( point )
      }

      /**
       * Promotes a leaf that exists in Qi-1 to this
       * tree, by inserting it into this node which
       * is its interesting node in Qi.
       *
       * If the result of insertion is a new child node
       * below this node, this intermediate node will
       * be connected to Qi by looking for the corresponding
       * hyper-cube in the given search path that led here
       * (i.e. that was constructed in `findPN`).
       *
       * This method also sets the parent of the leaf
       * accordingly.
       */
      private[DeterministicSkipOctree] final def insert( leaf: Leaf[ S, D, A ])
                                                       ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         import impl.pointView
         val point   = pointView( leaf.value )
         val qidx    = hyperCube.indexOf( point )
         val c       = children( qidx )
         if( c.isEmpty ) {
            children( qidx )  = leaf
            leaf.parent       = this
         } else {
            val old = c.asRightInnerNonEmpty
            // determine the greatest interesting square for the new
            // intermediate node to create
            val qn2     = old.union( hyperCube.orthant( qidx ), point )
            // find the corresponding node in the lower tree
            var pPrev   = prev
            while( pPrev.hyperCube != qn2 ) pPrev = pPrev.child( leaf.orthantIndexIn( pPrev.hyperCube )).asNode
            val n2      = newNode( qidx, pPrev, qn2 )

            val c2      = n2.children
            val oidx    = old.orthantIndexIn( qn2 )
            c2( oidx )  = old
            // This is a tricky bit! And a reason
            // why should eventually try to do without
            // parent pointers at all. Since `old`
            // may be a leaf whose parent points
            // to a higher level tree, we need to
            // check first if the parent is `this`,
            // and if so, adjust the parent to point
            // to the new intermediate node `ne`!
            if( old.parent == this ) old.parentRight_=( n2 )
            val lidx    = leaf.orthantIndexIn( qn2 )
            c2( lidx )  = leaf
            leaf.parent = n2
         }
      }

      /*
       * Instantiates an appropriate
       * sub-node whose parent is this node, and whose predecessor
       * in the lower octree is given.
       *
       * @param   qidx  the orthant index in this node where the node belongs
       * @param   prev  the new node's prev field, i.e. its correspondant in
       *                Qi-1
       * @param   iq    the hyper-cube for the new node
       * @return  the new node which has already been inserted into this node's
       *          children at index `qidx`.
       */
      @inline private def newNode( qidx: Int, prev: Node[ S, D, A ], iq: D#HyperCube )
                                 ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) : InnerRightNode[ S, D, A ] = {
         import impl.{system, rightNodeSer}
         val sz         = children.length
         val ch         = new Array[ RightChild[ S, D, A ]]( sz )
         val parentRef  = system.newRef[ RightNode[ S, D, A ]]( this )
         val rightRef   = system.newRef[ RightNode[ S, D, A ]]( null )
         val n          = new InnerRightNode( parentRef, prev, iq, ch, rightRef )
         prev.next      = n
         children( qidx ) = n
         n
      }

      private[DeterministicSkipOctree] final def removeImmediateLeaf( leaf: Leaf[ S, D, A ])
                                                                    ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         val sz = children.length
         var qidx = 0; while( qidx < sz /* numOrthants */) {
            if( children( qidx ) == leaf ) {
               children( qidx ) = new Empty[ S, D, A ]
               var newParent  = prev
               var pidx       = qidx
               while( true ) {
                  val c = newParent.child( pidx )
                  if( c.isNode ) {
                     val sn = c.asNode
                     newParent   = sn
                     pidx        = leaf.orthantIndexIn( sn.hyperCube )
                  } else {
                     val sl = c.asLeaf
                     assert( sl == leaf, "Internal error - diverging leaves : " + leaf + " versus " + sl )
                     leafRemoved()
                     leaf.parent = newParent
                     return
                  }
               }
            }
         qidx += 1 }
      }
   }

   /**
    * A left tree node implementation provides more specialized child nodes
    * of type `LeftChild`. It furthermore defines a resolution method
    * `findImmediateLeaf` which is typically called after arriving here
    * from a `findP0` call.
    */
   private sealed trait LeftNode[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Node[ S, D, A ] with LeftNonEmpty[ S, D, A ] {
      /**
       * For a `LeftNode`, all its children are more specific
       * -- they are instances of `LeftChild` and thus support
       * order intervals.
       */
//      final val children = Array.fill[ LeftChild[ S, D, A ]]( numOrthants )( Empty )
      private[DeterministicSkipOctree] def children: Array[ LeftChild[ S, D, A ]]
//      private[DeterministicSkipOctree] final var next : RightNode[ S, D, A ] = null

      /**
       * The stop-order of a left node is now always implicitly defined.
       * It is not a real entry in the total-order. Instead it is either
       * the start-order, if the node is empty, otherwise the stop-order
       * of the right-most non-empty child of the node. Since only `append`
       * is used on the order entries, this totally suffices for maintaining
       * the tree's binarization.
       */
      private[DeterministicSkipOctree] final def stopOrder : Order[ S ] = {
         var res = startOrder
         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = children( i )
            if( !c.isEmpty ) {
               res = c.asLeftInnerNonEmpty.stopOrder
            }
         i +=1 }
         res
      }

      /**
       * Note that `prev` will not be called as part of this octree implementation
       * which smartly distinguishes between left and right nodes. It is merely here
       * to satisfy the `Node` interface of `SkipOctree`.
       */
      private[DeterministicSkipOctree] final def prev : Node[ S, D, A ] = null

      final def child( idx: Int ) : Child[ S, D, A ] = children( idx )

      private[DeterministicSkipOctree] final def findP0( point: D#Point ) : LeftNode[ S, D, A ] = {
         val qidx = hyperCube.indexOf( point )
         val c    = children( qidx )
         if( c.isNode ) {
            val n = c.asNode
            if( n.hyperCube.contains( point )) n.findP0( point )
            else this
         } else this
      }

      /**
       * After arriving at this node from a `findP0` call, this resolves
       * the given point to an actual leaf.
       *
       * @return  the `Leaf` child in this node associated with the given
       *          `point`, or `null` if no such leaf exists.
       */
      private[DeterministicSkipOctree] final def findImmediateLeaf( point: D#Point )
                                                                  ( implicit impl: Impl[ S, D, A ]) : Leaf[ S, D, A ] = {
         import impl.pointView
         val qidx = hyperCube.indexOf( point )
         val c = children( qidx )
         if( c.isLeaf ) {
            val l = c.asLeaf
            if( pointView( l.value ) == point ) l else null
         } else null
      }

      private[DeterministicSkipOctree] final def removeImmediateLeaf( leaf: Leaf[ S, D, A ])
                                                                    ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         val sz = children.length
         var qidx = 0; while( qidx < sz ) {
            if( children( qidx ) == leaf ) {
               children( qidx ) = new Empty[ S, D, A ]
               leafRemoved()
               leaf.dispose()
               return
            }
         qidx += 1 }
      }

      private[DeterministicSkipOctree] final def insert( point: D#Point, value: A )
                                                       ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) : Leaf[ S, D, A ] = {
         val qidx = hyperCube.indexOf( point )
         val c    = children( qidx )
         if( c.isEmpty ) {
            newLeaf( qidx, point, value ) // (this adds it to the children!)
         } else {
            val old = c.asLeftInnerNonEmpty
            // define the greatest interesting square for the new node to insert
            // in this node at qidx:
            val qn2              = old.union( hyperCube.orthant( qidx ), point )
            // create the new node (this adds it to the children!)
            val n2               = newNode( qidx, qn2 )
            val oidx             = old.orthantIndexIn( qn2 )
            n2.children( oidx )  = old
            val lidx             = qn2.indexOf( point )
            // This is a tricky bit! And a reason
            // why should eventually try to do without
            // parent pointers at all. Since `old`
            // may be a leaf whose parent points
            // to a higher level tree, we need to
            // check first if the parent is `this`,
            // and if so, adjust the parent to point
            // to the new intermediate node `ne`!
            if( old.parent == this ) old.parentLeft_=( n2 )
            n2.newLeaf( lidx, point, value )
         }
      }

      /**
       * Instantiates an appropriate
       * leaf whose parent is this node, and which should be
       * ordered according to its position in this node.
       *
       * @param   qidx  the orthant index of the new leaf in this node
       * @param   point the point associated with the new leaf
       * @param   value the value associated with the new leaf
       * @return  the new leaf which has already assigned this node as
       *          parent and is already stored in this node's children
       *          at index `qidx`
       */
      private def newLeaf( qidx: Int, point: D#Point, value: A )
                         ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) : Leaf[ S, D, A ] = {
         import impl.{nodeSer, system}
         val parentRef  = system.newRef[ Node[ S, D, A ]]( this )
         val l          = new Leaf( point, value, newChildOrder( qidx ), parentRef )
//         l.parent = this
         children( qidx ) = l
         l
      }

      /**
       * Creates a new entry in the total-order for a new child to be
       * inserted into this node. This is determined by the following rules:
       *
       * - if this leaf is the first non-empty child in the node,
       *   insert it after the start-order of this node into the
       *   the total order
       * - otherwise, insert the leaf after the right-most child's stop-order
       *   which comes before the new leaf.
       *
       * @param   qidx  the orthant index at which the child will be inserted
       * @return  the entry in the total-order to associate with the child
       *          (in the case of a node, the start-order)
       */
      private def newChildOrder( qidx: Int )( implicit tx: S#Tx ) : Order[ S ] = {
         var pre = startOrder
         var i = 0; while( i < qidx ) {
            val c = children( i )
            if( !c.isEmpty ) {
               pre = c.asLeftInnerNonEmpty.stopOrder
            }
         i +=1 }
         pre.append()
      }

      /*
       * Instantiates an appropriate
       * sub-node whose parent is this node, and which should be
       * ordered according to its position in this node.
       *
       * @param   qidx  the orthant index of the new node in this (parent) node
       * @param   iq    the hyper-cube of the new node
       * @return  the new node which has already assigned this node as
       *          parent and is already stored in this node's children
       *          at index `qidx`
       */
      @inline private def newNode( qidx: Int, iq: D#HyperCube )
                                 ( implicit tx: S#Tx, impl: Impl[ S, D, A ]) : InnerLeftNode[ S, D, A ] = {
         import impl.{leftNodeSer, rightNodeSer, system}
         val sz         = children.length
         val ch         = new Array[ LeftChild[ S, D, A ]]( sz )
         val parentRef  = system.newRef[ LeftNode[ S, D, A ]]( this )
         val rightRef   = system.newRef[ RightNode[ S, D, A ]]( null )
         val n          = new InnerLeftNode( parentRef, iq, newChildOrder( qidx ), ch, rightRef )
         children( qidx ) = n
         n
      }
   }

   /* private */ sealed trait TopNode[ S <: Sys[ S ], D <: Space[ D ], A ]
   extends Node[ S, D, A ] {
//      final def hyperCube : D#HyperCube = tree.hyperCube
      private[DeterministicSkipOctree] final def findPN( implicit tx: S#Tx ) : RightNode[ S, D, A ] = next
   }

   private final class TopLeftNode[ S <: Sys[ S ], D <: Space[ D ], A ](
      val hyperCube: D#HyperCube, val startOrder: Order[ S ],
      val children: Array[ LeftChild[ S, D, A ]], protected val nextRef: S#Ref[ RightNode[ S, D, A ]])
   extends LeftNode[ S, D, A ] with TopNode[ S, D, A ] {
//         val startOrder = totalOrder.root
//      def startOrder = _rootOrder

      // that's alright, we don't need to do anything special here
      protected def leafRemoved()( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {}

      def nodeName = "top-left"
   }

   private final class InnerLeftNode[ S <: Sys[ S ], D <: Space[ D ], A ](
      parentRef: S#Ref[ LeftNode[ S, D, A ]], val hyperCube: D#HyperCube, val startOrder: Order[ S ],
      val children: Array[ LeftChild[ S, D, A ]], protected val nextRef: S#Ref[ RightNode[ S, D, A ]])
   extends LeftNode[ S, D, A ] with InnerNode[ S, D, A ] with LeftInnerNonEmpty[ S, D, A ] {
      def nodeName = "inner-left"

      private[DeterministicSkipOctree] def parentLeft_=( p: LeftNode[ S, D, A ])( implicit tx: S#Tx ) { parent = p }
      private[DeterministicSkipOctree] def parent( implicit tx: S#Tx ) : LeftNode[ S, D, A ] = parentRef.get
      private[DeterministicSkipOctree] def parent_=( node: LeftNode[ S, D, A ])( implicit tx: S#Tx ) {
         parentRef.set( node )
      }

      // might become important with persistent implementation
      private[DeterministicSkipOctree] def dispose()( implicit tx: S#Tx ) {
         assert( next == null )
         startOrder.remove()
      }

      // make sure the node is not becoming uninteresting, in which case
      // we need to merge upwards
      protected def leafRemoved()( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         var lonely: LeftInnerNonEmpty[ S, D, A ] = null
         var numNonEmpty = 0
         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = children( i )
            if( !c.isEmpty ) {
               numNonEmpty += 1
               lonely = c.asLeftInnerNonEmpty
            }
         i += 1 }
         if( numNonEmpty == 1 ) {   // gotta remove this node and put remaining non empty element in parent
            val myIdx = parent.hyperCube.indexOf( hyperCube )
            parent.children( myIdx ) = lonely
            if( lonely.parent == this ) lonely.parentLeft_=( parent )
            dispose()
         }
      }
   }

   /**
    * Note that this instantiation sets the `prev`'s `next` field to this new node.
    */
   private final class TopRightNode[ S <: Sys[ S ], D <: Space[ D ], A ](
      val hyperCube: D#HyperCube, val prev: TopNode[ S, D, A ], val children: Array[ RightChild[ S, D, A ]],
      protected val nextRef: S#Ref[ RightNode[ S, D, A ]])
   extends RightNode[ S, D, A ] with TopNode[ S, D, A ] {

//      prev.next = this

      def nodeName = "top-right"

      // remove this node if it empty now and right-node tree
      protected def leafRemoved()( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         if( next != null ) return

         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = children( i )
            if( !c.isEmpty ) return // node not empty, abort the check
         i += 1 }

         // ok, we are the right most tree and the node is empty...
         dispose()
      }

      private[DeterministicSkipOctree] def dispose()( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         import impl.lastTree
         assert( next == null )
         assert( lastTree == this )
         lastTree     = prev
         prev.next   = null
//            prev        = null
      }
   }

   /**
    * Note that this instantiation sets the `prev`'s `next` field to this new node.
    */
   private final class InnerRightNode[ S <: Sys[ S ], D <: Space[ D ], A ](
      parentRef: S#Ref[ RightNode[ S, D, A ]], val prev: Node[ S, D, A ], val hyperCube: D#HyperCube,
      val children: Array[ RightChild[ S, D, A ]], protected val nextRef: S#Ref[ RightNode[ S, D, A ]])
   extends RightNode[ S, D, A ] with InnerNode[ S, D, A ] with RightInnerNonEmpty[ S, D, A ] {

//      prev.next = this

      def nodeName = "inner-right"

      private[DeterministicSkipOctree] def parentRight_=( p: RightNode[ S, D, A ])( implicit tx: S#Tx ) { parent = p }

      private[DeterministicSkipOctree] def dispose()( implicit tx: S#Tx ) {
         assert( next == null )
         prev.next   = null
//            prev        = null
      }

      private[DeterministicSkipOctree] def parent( implicit tx: S#Tx ) : RightNode[ S, D, A ] = parentRef.get
      private[DeterministicSkipOctree] def parent_=( node: RightNode[ S, D, A ])( implicit tx: S#Tx ) {
         parentRef.set( node )
      }

      // make sure the node is not becoming uninteresting, in which case
      // we need to merge upwards
      protected def leafRemoved()( implicit tx: S#Tx, impl: Impl[ S, D, A ]) {
         var lonely: RightInnerNonEmpty[ S, D, A ] = null
         var numNonEmpty = 0
         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = children( i )
            if( !c.isEmpty ) {
               numNonEmpty += 1
               lonely = c.asRightInnerNonEmpty
            }
         i += 1 }
         if( numNonEmpty == 1 ) {   // gotta remove this node and put remaining non empty element in parent
            val myIdx = parent.hyperCube.indexOf( hyperCube )
            parent.children( myIdx ) = lonely
            if( lonely.parent == this ) lonely.parentRight_=( parent )
            dispose()
         }
      }
   }

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )
}
sealed trait DeterministicSkipOctree[ S <: Sys[ S ], D <: Space[ D ], A ]
extends SkipOctree[ S, D, A ] {
   def headTree : DeterministicSkipOctree.Node[ S, D, A ]
   def lastTree( implicit tx: S#Tx ) : DeterministicSkipOctree.TopNode[ S, D, A ]
}