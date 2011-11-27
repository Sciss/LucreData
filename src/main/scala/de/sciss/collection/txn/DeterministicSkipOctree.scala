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
import annotation.{switch, tailrec}
import de.sciss.collection.geom.{QueryShape, DistanceMeasure, Space}
import de.sciss.lucrestm.{MutableReader, DataOutput, DataInput, Mutable, Serializer, Sys}

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
   def empty[ S <: Sys[ S ], D <: Space[ D ], A ]( hyperCube: D#HyperCube, skipGap: Int = 2 )
                                                 ( implicit view: A => D#PointLike, tx: S#Tx, system: S, space: D,
                                                   keySerializer: Serializer[ A ],
                                                   hyperSerializer: Serializer[ D#HyperCube ],
                                                   smf: Manifest[ S ],
                                                   dmf: Manifest[ D ],
                                                   amf: Manifest[ A ]) : DeterministicSkipOctree[ S, D, A ] = {

      new ImplNew[ S, D, A ]( skipGap, system.newID, hyperCube, view )
   }

   def reader[ S <: Sys[ S ], D <: Space[ D ], A ](
      implicit view: A => D#PointLike, system: S, space: D,
      keySerializer: Serializer[ A ], hyperSerializer: Serializer[ D#HyperCube ],
      smf: Manifest[ S ], dmf: Manifest[ D ], amf: Manifest[ A ]
   ) : MutableReader[ S, DeterministicSkipOctree[ S, D, A ]] = new OctreeReader[ S, D, A ]

   private val SER_VERSION = 0

   private final class OctreeReader[ S <: Sys[ S ], D <: Space[ D ], A ](
      implicit view: A => D#PointLike, system: S, space: D,
      keySerializer: Serializer[ A ], hyperSerializer: Serializer[ D#HyperCube ],
      smf: Manifest[ S ], dmf: Manifest[ D ], amf: Manifest[ A ]
   ) extends MutableReader[ S, DeterministicSkipOctree[ S, D, A ]] {
      def readData( in: DataInput, id: S#ID ) : DeterministicSkipOctree[ S, D, A ] = {
         val version = in.readUnsignedByte()
         require( version == SER_VERSION, "Incompatible serialized version (found " + version +
            ", required " + SER_VERSION + ")." )

         val hyperCube  = hyperSerializer.read( in )
         new ImplRead[ S, D, A ]( id, hyperCube, view, in )
      }
   }

   private final class ImplRead[ S <: Sys[ S ], D <: Space[ D ], A ]( val id: S#ID, val hyperCube: D#HyperCube,
                                                                      val pointView: A => D#PointLike, in: DataInput )
                                                                    ( implicit val system: S, val space: D,
                                                                      val keySerializer: Serializer[ A ],
                                                                      val hyperSerializer: Serializer[ D#HyperCube ])
   extends DeterministicSkipOctree[ S, D, A ] {
      val totalOrder = {
         implicit val orderReader = TotalOrder.Set.reader[ S ]()
         system.readMut[ TotalOrder.Set[ S ]]( in )
      }
      val skipList = {
         implicit val ord  = LeafOrdering
         implicit val r1   = LeafReader
         implicit val r2   = HASkipList.reader[ S, LeafImpl ]( KeyObserver )
         system.readMut[ HASkipList[ S, LeafImpl ]]( in )
      }
      val headTree = {
         implicit val r3   = TopLeftBranchReader
         system.readMut[ TopLeftBranch ]( in )
      }
      val lastTreeRef = {
         implicit val r4   = TopBranchReader
         system.readRef[ TopBranch ]( in )
      }
   }

   private final class ImplNew[ S <: Sys[ S ], D <: Space[ D ], A ]( skipGap: Int, val id: S#ID,
                                                                     val hyperCube: D#HyperCube,
                                                                     val pointView: A => D#PointLike )
                                                                   ( implicit tx: S#Tx, val system: S,
                                                                     val space: D, val keySerializer: Serializer[ A ],
                                                                     val hyperSerializer: Serializer[ D#HyperCube ])

   extends DeterministicSkipOctree[ S, D, A ] {
      val totalOrder = TotalOrder.Set.empty[ S ]()
      val skipList   = {
         implicit val ord  = LeafOrdering
         implicit val kser = LeafReader
         HASkipList.empty[ S, LeafImpl ]( skipGap, KeyObserver )
      }
      val headTree = {
         val sz            = numOrthants
         val ch            = system.newRefArray[ LeftChild ]( sz )
         implicit val r1   = LeftChildReader
         var i = 0; while( i < sz ) {
            ch( i )        = system.newRef[ LeftChild ]( null )
         i += 1 }
         implicit val r2   = RightBranchReader
         val headRight     = system.newRef[ RightBranch ]( null )
         new TopLeftBranch( system.newID, hyperCube, totalOrder.root, ch, headRight )
      }
      val lastTreeRef = {
         implicit val r3   = TopBranchReader
         system.newRef[ TopBranch ]( headTree )
      }
   }

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )
}
sealed trait DeterministicSkipOctree[ S <: Sys[ S ], D <: Space[ D ], A ]
extends SkipOctree[ S, D, A ] {
   import DeterministicSkipOctree.{SER_VERSION, opNotSupported}

   private type Order = TotalOrder.Set.Entry[ S ]

   implicit def system: S
   implicit def space: D
   implicit def keySerializer: Serializer[ A ]
   implicit def hyperSerializer: Serializer[ D#HyperCube ]

   protected def totalOrder: TotalOrder.Set[ S ]
   protected def skipList: HASkipList[ S, LeafImpl ]
   protected def headTree: TopLeftBranch
   protected def lastTreeRef: S#Ref[ TopBranch ]

   implicit protected object LeafOrdering extends Ordering[ S#Tx, LeafImpl ] {
      /**
       * Leafs are ordered by the tree's in-order traversal,
       * where the quadrants I+II and III+IV can be thought
       * of as dummy nodes to binarize the octree. That is
       * to say, in a node, the child order corresponds to
       * their quadrant indices (I < II < III < IV).
       */
      def compare( a: LeafImpl, b: LeafImpl )( implicit tx: S#Tx ) : Int = {
//         order.compare( that.order )
         val t1 = a.order.tag
         val t2 = b.order.tag
         if( t1 < t2 ) -1 else if( t1 > t2 ) 1 else 0
      }
   }

   implicit protected object RightBranchReader extends MutableReader[ S, RightBranch ] {
      def readData( in: DataInput, id: S#ID ) : RightBranch = {
         (in.readUnsignedByte(): @switch) match {
            case 4 => readTopRightBranch( in, id )
            case 5 => readRightChildBranch( in, id )
         }
      }
   }

   implicit protected object BranchReader extends MutableReader[ S, BranchImpl ] {
      def readData( in: DataInput, id: S#ID ) : BranchImpl = {
         (in.readUnsignedByte(): @switch) match {
            case 2 => readTopLeftBranch( in, id )
            case 3 => readLeftChildBranch( in, id )
            case 4 => readTopRightBranch( in, id )
            case 5 => readRightChildBranch( in, id )
         }
      }
   }

   implicit protected object TopBranchReader extends MutableReader[ S, TopBranch ] {
      def readData( in: DataInput, id: S#ID ) : TopBranch = {
         (in.readUnsignedByte(): @switch) match {
            case 2 => readTopLeftBranch( in, id )
            case 4 => readTopRightBranch( in, id )
         }
      }
   }

   implicit protected object LeftChildReader extends MutableReader[ S, LeftChild ] {
      def readData( in: DataInput, id: S#ID ) : LeftChild = {
         (in.readUnsignedByte(): @switch) match {
            case 0 => null
            case 1 => readLeaf( in, id )
            case 3 => readLeftChildBranch( in, id )
         }
      }
   }

   implicit protected object LeftBranchReader extends MutableReader[ S, LeftBranch ] {
      def readData( in: DataInput, id: S#ID ) : LeftBranch = {
         (in.readUnsignedByte(): @switch) match {
            case 2 => readTopLeftBranch( in, id )
            case 3 => readLeftChildBranch( in, id )
         }
      }
   }

   implicit protected object RightChildReader extends MutableReader[ S, RightChild ] {
      def readData( in: DataInput, id: S#ID ) : RightChild = {
         (in.readUnsignedByte(): @switch) match {
            case 0 => null
            case 1 => readLeaf( in, id )
            case 5 => readRightChildBranch( in, id )
         }
      }
   }

   implicit protected object TopLeftBranchReader extends MutableReader[ S, TopLeftBranch ] {
      def readData( in: DataInput, id: S#ID ) : TopLeftBranch = {
         val b = in.readUnsignedByte()
         require( b == 2, b.toString )
         readTopLeftBranch( in, id )
      }
   }

   implicit protected object TopRightBranchReader extends MutableReader[ S, TopRightBranch ] {
      def readData( in: DataInput, id: S#ID ) : TopRightBranch = {
         val b = in.readUnsignedByte()
         require( b == 4, b.toString )
         readTopRightBranch( in, id )
      }
   }

   implicit protected object LeafReader
   extends Serializer[ LeafImpl ] with MutableReader[ S, LeafImpl ] {
      def write( l: LeafImpl, out: DataOutput ) { l.write( out )}

      def read( in: DataInput ) : LeafImpl = system.readMut[ LeafImpl ]( in )( this )

      def readData( in: DataInput, id: S#ID ) : LeafImpl = {
         val b = in.readUnsignedByte()
         require( b == 1, b.toString )
         readLeaf( in, id )
      }
   }

   implicit protected object KeyObserver extends SkipList.KeyObserver[ S#Tx, LeafImpl ] {
      def keyUp( l: LeafImpl )( implicit tx: S#Tx ) {
//println( "up : " + l )
         // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
         //  then traverse upwards in Qi until we find the lowest
         //  ancestor q of x which is also interesting in Qi+1.
         //  (This is the reversed process of searching x in Qi
         //  with q = pi,start = pi+1,end so it takes at most 6
         //  steps by Lemma 5.) Then we go to the same square q
         //  in Qi+1 and insert x."

         val pNext0     = l.parent.findPN // ( path, 0 )
         val pNext      = if( pNext0 eq null ) { // create new level
            val sz      = numOrthants
            val ch      = system.newRefArray[ RightChild ]( sz )
            var i = 0; while( i < sz ) {
               ch( i )  = system.newRef[ RightChild ]( null )
            i += 1 }
            val nextRef = system.newRef[ RightBranch ]( null )
            val prev    = lastTree
            val res     = new TopRightBranch( system.newID, /* impl */ hyperCube, prev, ch, nextRef )
            prev.next   = res
            lastTree    = res
            res
         } else pNext0
         pNext.insert( l )
      }

      def keyDown( l: LeafImpl )( implicit tx: S#Tx ) {
//println( "down : " + l )
         // "To delete x from Qi we go from xi to the smallest interesting
         //  square pi(x) containing x in Qi following the pointers. Then
         //  the deletion given pi(x) is as described in Section 2.3."

         val p = l.parent
         p.removeImmediateLeaf( l )
      }
   }


//      val (totalOrder, skipList)    = _scaffFun( this )
//      val (headTree, lastTreeRef)   = _treeFun( this )

   def numOrthants: Int = 1 << space.dim  // 4 for R2, 8 for R3, 16 for R4, etc.

   protected def writeData( out: DataOutput ) {
      out.writeUnsignedByte( SER_VERSION )
      hyperSerializer.write( hyperCube, out )
      totalOrder.write( out )
      skipList.write( out )
      headTree.write( out )
      lastTreeRef.write( out )
   }

   protected def disposeData()( implicit tx: S#Tx ) {
      // dispose tree
      var t: BranchImpl  = lastTree
      val sz         = numOrthants

      def disposeBranch( b: BranchImpl ) {
         var i = 0; while( i < sz ) {
            val c = b.child( i )
            if( c ne null ) {
               if( c.isLeaf ) {
                  c.asLeaf.dispose()
               } else disposeBranch( c.asBranch )
            }
         i += 1 }
         b.dispose()
      }

      while( t ne null ) {
         val p = t.prev
         disposeBranch( t )
         t = p
      }

      lastTreeRef.dispose()
      totalOrder.dispose()
      skipList.dispose()
   }

   def lastTree( implicit tx: S#Tx ) : TopBranch = lastTreeRef.get
   def lastTree_=( node: TopBranch )( implicit tx: S#Tx ) {
      lastTreeRef.set( node )
   }

   def size( implicit tx: S#Tx ) : Int = skipList.size

   def add( elem: A )( implicit tx: S#Tx ) : Boolean = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf eq null ) true else {
         val res = oldLeaf.value != elem
         oldLeaf.dispose()
         res
      }
   }

   def update( elem: A )( implicit tx: S#Tx ) : Option[ A ] = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf eq null ) None else {
         val res = Some( oldLeaf.value )
         oldLeaf.dispose()
         res
      }
   }

   def remove( elem: A )( implicit tx: S#Tx ) : Boolean = {
      val oldLeaf = removeLeaf( pointView( elem ))
      if( oldLeaf eq null ) false else {
         oldLeaf.dispose()
         true
      }
   }

   def removeAt( point: D#PointLike )( implicit tx: S#Tx ) : Option[ A ] = {
      val oldLeaf = removeLeaf( point )
      if( oldLeaf eq null ) None else {
         val res = Some( oldLeaf.value )
         oldLeaf.dispose()
         res
      }
   }

   def contains( elem: A )( implicit tx: S#Tx ) : Boolean = {
      val point = pointView( elem )
      if( !hyperCube.contains( point )) return false
      val l = findLeaf( point )
      if( l eq null ) false else l.value == elem
   }

   def isDefinedAt( point: D#PointLike )( implicit tx: S#Tx ) : Boolean = {
      if( !hyperCube.contains( point )) return false
      findLeaf( point ) ne null
   }

   def get( point: D#PointLike )( implicit tx: S#Tx ) : Option[ A ] = {
      if( !hyperCube.contains( point )) return None
      val l = findLeaf( point )
      if( l eq null ) None else Some( l.value )
   }

   def nearestNeighbor[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ])
                                                ( implicit tx: S#Tx ) : A = {
      val res = new NN( point, metric ).find()
      if( res ne null ) res.value else throw new NoSuchElementException( "nearestNeighbor on an empty tree" )
   }

   def nearestNeighborOption[ @specialized( Long ) M ]( point: D#PointLike, metric: DistanceMeasure[ M, D ])
                                                      ( implicit tx: S#Tx ) : Option[ A ] = {
      val res = new NN( point, metric ).find()
      if( res ne null ) Some( res.value ) else None
   }

   def isEmpty( implicit tx: S#Tx ) : Boolean = {
      val n = headTree
      val sz = numOrthants
      var i = 0; while( i < sz ) {
         if( n.child( i ) ne null ) return false
      i += 1 }
      true
   }

   def numLevels( implicit tx: S#Tx ) : Int = {
      var n: BranchImpl = headTree.next
      var i = 1; while( n ne null ) {
         n = n.next
         i += 1
      }
      i
   }

   def +=( elem: A )( implicit tx: S#Tx ) : this.type = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf ne null ) oldLeaf.dispose()
      this
   }

   def -=( elem: A )( implicit tx: S#Tx ) : this.type = {
      val oldLeaf = removeLeaf( pointView( elem ))
      if( oldLeaf ne null ) oldLeaf.dispose()
      this
   }

   def rangeQuery[ @specialized( Long ) Area ]( qs: QueryShape[ Area, D ])( implicit tx: S#Tx ) : Iterator[ A ] = {
      val q = new RangeQuery( qs )
      q.findNextValue()
      q
   }

   def toIndexedSeq( implicit tx: S#Tx ) : IIdxSeq[ A ] = iterator.toIndexedSeq
   def toList( implicit tx: S#Tx ) : List[ A ] = iterator.toList
   def toSeq(  implicit tx: S#Tx ) : Seq[  A ] = iterator.toIndexedSeq // note that `toSeq` produces a `Stream` !!
   def toSet(  implicit tx: S#Tx ) : Set[  A ] = iterator.toSet

   private def findLeaf( point: D#PointLike )( implicit tx: S#Tx ) : LeafImpl = {
      val p0 = lastTree.findP0( point )
      p0.findImmediateLeaf( point )
   }

   // WARNING: if the returned oldLeaf is defined, the caller is
   // responsible for disposing it (after extracting useful information such as its value)
   private def insertLeaf( elem: A )( implicit tx: S#Tx ) : LeafImpl = {
      val point   = pointView( elem )
      require( hyperCube.contains( point ), point.toString + " lies out of root hyper-cube " + hyperCube )

      val p0      = lastTree.findP0( point )
      val oldLeaf = p0.findImmediateLeaf( point )
      if( oldLeaf eq null ) {
         val leaf = p0.insert( point, elem )
         skipList.add( leaf )
      } else {
//            oldLeaf.value = elem
         // remove previous leaf
         removeImmediateLeaf( oldLeaf )
         // search anew
         val p0b        = lastTree.findP0( point )
         assert( p0b.findImmediateLeaf( point ) eq null ) // XXX
         val leaf = p0b.insert( point, elem )
         skipList.add( leaf )
      }
      oldLeaf
   }

   // WARNING: if the returned oldLeaf is defined, the caller is
   // responsible for disposing it (after extracting useful information such as its value)
   private def removeLeaf( point: D#PointLike )( implicit tx: S#Tx ) : LeafImpl = {
      if( !hyperCube.contains( point )) return null

      // "To insert or delete a point y into or from S, we first search the
      // quadtree structure to locate y in each Qi ..."
      val p0 = lastTree.findP0( point )

      // "... Then we insert or delete y
      // in the binary Q0 and update our total order."

      val l = p0.findImmediateLeaf( point )
      if( l ne null ) {
         removeImmediateLeaf( l )
      }
      l
   }

   private def removeImmediateLeaf( l: LeafImpl )( implicit tx: S#Tx ) {
      // this will trigger removals from upper levels
      skipList.remove( l )
      // be careful: p0 at this point may be invalid
      // as the skiplist removal might have merged
      // it with its parent(s). we thus need to find
      // the parent of l in Q0 again!

      val p = l.parent
      p.removeImmediateLeaf( l )
//         assert( l.parent eq null, "Internal error - leaf should be removed by now : " + l )
   }

   def iterator( implicit tx: S#Tx ) : Iterator[ A ] = skipList.iterator.map( _.value )

   private final class NN[ @specialized( Long ) M ](
      point: D#PointLike, metric: DistanceMeasure[ M, D ])
   extends scala.math.Ordering[ VisitedNode[ M ]] {
      private type Vis = VisitedNode[ M ]

      private val sz                         = numOrthants
      private var bestLeaf: LeafImpl             = null
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

      @tailrec def findNNTail( n0: BranchImpl )( implicit tx: S#Tx ) {
         numAcceptedChildren = 0
         var accept1Idx = 0
         val oldRMax1 = rmax
         var i = 0; while( i < sz ) {
            val n0c = n0.child( i )
            if( n0c ne null ) {
               if( n0c.isLeaf ) {
                  val l = n0c.asLeaf
                  val ldist = metric.distance( point, pointView( l.value ))
                  if( metric.isMeasureGreater( bestDist, ldist )) {
                     bestDist = ldist
                     bestLeaf = l
                     if( metric.isMeasureGreater( rmax, bestDist )) {
                        rmax = bestDist
                     }
                  }
               } else /* if( n0c.isNode ) */ {
                  val c             = n0c.asBranch
                  val cq            = c.hyperCube
                  val cMinDist      = metric.minDistance( point, cq )
                  if( !metric.isMeasureGreater( cMinDist, rmax )) {   // otherwise we're out already
                     val cMaxDist   = metric.maxDistance( point, cq )
                     if( metric.isMeasureGreater( rmax, cMaxDist )) {
                        rmax = cMaxDist
                     }
                     acceptedChildren( numAcceptedChildren ) = new VisitedNode[ M ]( c, cMinDist /*, cMaxDist */)
                     accept1Idx = i
                     numAcceptedChildren += 1
                  }
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
         while( succ ne null ) {
            val c = succ.child( accept1Idx )
            succ  = if( (c ne null) && c.isBranch ) {
               val dn2 = c.asBranch
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
         @tailrec def findLeft( n: BranchImpl ) : BranchImpl = {
            val prev = n.prev
            if( prev eq null ) n else findLeft( prev )
         }
//         while( dn.prev ne null ) dn = dn.prev
//         findNNTail( dn )
         findNNTail( findLeft( dn ))
      }

      def find()( implicit tx: S#Tx ) : LeafImpl = {
         var n0: BranchImpl = headTree
         while( true ) {
            findNNTail( n0 )
//            if( space.bigLeqZero( bestDist )) return bestLeaf
//            if( bestDist == metric.minValue ) return bestLeaf
            if( metric.isMeasureZero( bestDist )) return bestLeaf
            var i = 0; while( i < numAcceptedChildren ) {
               pri += acceptedChildren( i )
            i += 1 }
            var vis: Vis = null
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

   private final class VisitedNode[ @specialized( Long ) M ]( val n: BranchImpl, val minDist: M )

   // note: Iterator is not specialized, hence we can safe use the effort to specialize in A anyway
   private final class RangeQuery[ @specialized( Long ) Area ]( qs: QueryShape[ Area, D ]) extends Iterator[ A ] {
      val sz            = numOrthants
      val stabbing      = MQueue.empty[ (BranchImpl, Area) ]  // Tuple2 is specialized for Long, too!
      val in            = MQueue.empty[ Node ]
      var current : A   = _      // overwritten by initial run of `findNextValue`
      var hasNext       = true   // eventually set to `false` by `findNextValue`

      stabbing += headTree -> qs.overlapArea( headTree.hyperCube )
//      findNextValue()

      // search downwards:
      // "At each square q ∈ Qi we either go to a child square in Qi
      // that covers the same area of R ∪ A as p does, if such a child
      // square exists, or jump to the next level q ∈ Qi−1."
      @tailrec def findEquiStabbingTail( node: BranchImpl, area: Area )( implicit tx: S#Tx ) : BranchImpl = {
         var pi = node
         var i = 0; while( i < sz ) {
            val c = pi.child( i )
            if( (c ne null) && c.isBranch ) {
               val pic = c.asBranch
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
         if( prev eq null ) pi else findEquiStabbingTail( prev, area )
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
      def findHighestUncritical( p0: BranchImpl, area: Area )( implicit tx: S#Tx ) : BranchImpl = {
         var pi         = p0.next
         if( pi eq null ) return p0
         var uncritical = p0
         var i = 0
         while( i < sz ) {
            val c = pi.child( i )
            if( (c ne null) && c.isBranch ) {
               val ci = c.asBranch
               val a2 = qs.overlapArea( ci.hyperCube )
               if( a2 == area ) {   // that means node is uncritical
                  uncritical  = pi
                  pi          = pi.next
                  if( pi eq null ) return uncritical
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
               if( c ne null ) {
                  if( c.isLeaf ) {
                     val cl = c.asLeaf
                     if( qs.contains( pointView( cl.value ))) in += cl
                  } else /* if( c.isNode ) */ {
                     val cn = c.asBranch
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
               }
            i += 1 }

         } else {
            val c = in.dequeue()
            if( c.isLeaf ) {
               val l = c.asLeaf
               current = l.value
               return
            } else {
               val n = c.asBranch
               var i = 0; while( i < sz ) {
                  val cc = n.child( i )
                  if( cc ne null ) {
                     in += cc // .asNonEmpty // sucky `enqueue` creates intermediate Seq because of varargs
                  }
                  i += 1 }
            }
         }
      }}
   }


   /**
    * A node is an object that can be
    * stored in a orthant of a branch.
    */
   protected sealed trait Node {
      def shortString : String
//      def isEmpty : Boolean
      def isLeaf : Boolean
      def isBranch : Boolean
      def asLeaf : LeafImpl
      def asBranch : BranchImpl
//      def asNonEmpty : NonEmpty[ S, D, A ]

      /**
       * Computes the greatest interesting hyper-cube within
       * a given hyper-cube `mq` so that this (leaf's or node's)
       * hyper-cube and the given point will be placed in
       * separated orthants of this resulting hyper-cube.
       */
      def union( mq: D#HyperCube, point: D#PointLike ) : D#HyperCube

      /**
       * Queries the orthant index for this (leaf's or node's) hyper-cube
       * with respect to a given outer hyper-cube `iq`.
       */
      def orthantIndexIn( iq: D#HyperCube ) : Int
   }

   /**
    * A tree element in Q0 has markers for the
    * in-order traversal.
    */
   protected sealed trait LeftNode extends Node {
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
      def startOrder: Order
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
      def stopOrder( implicit tx: S#Tx ): Order
   }

   /**
    * A common trait used in pattern matching, comprised of `Leaf` and `LeftChildBranch`.
    */
   protected sealed trait LeftChild extends LeftNode with Child with Mutable[ S ] {
      def updateParentLeft( p: LeftBranch )( implicit tx: S#Tx ) : Unit
   }

   /**
    * A common trait used in pattern matching, comprised of `Leaf` and `RightChildBranch`.
    */
   protected sealed trait RightChild extends Child with Mutable[ S ] {
      def updateParentRight( p: RightBranch )( implicit tx: S#Tx ) : Unit
   }

   /*
    * Serialization-id: 1
    */
   private def readLeaf( in: DataInput, id: S#ID ) : LeafImpl = {
      val value      = keySerializer.read( in )
      val order      = totalOrder.read( in )
      val parentRef  = system.readRef[ BranchImpl ]( in )
      new LeafImpl( id, value, order, parentRef )
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
   protected final class LeafImpl( val id: S#ID, val value: A, val order: Order, parentRef: S#Ref[ BranchImpl ])
   extends LeftChild with RightChild {
      def updateParentLeft( p: LeftBranch )( implicit tx: S#Tx )   { parent_=( p )}
      def updateParentRight( p: RightBranch )( implicit tx: S#Tx ) { parent_=( p )}
      def parent( implicit tx: S#Tx ): BranchImpl = parentRef.get
      def parent_=( p: BranchImpl )( implicit tx: S#Tx ) { parentRef.set( p )}

      def isLeaf  = true
      def isBranch  = false
      def asLeaf : LeafImpl = this
      def asBranch : BranchImpl = opNotSupported

      protected def disposeData()( implicit tx: S#Tx ) {
         order.dispose()
         parentRef.dispose()
      }

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 1 )
         keySerializer.write( value, out )
         order.write( out )
         parentRef.write( out )
      }

      def union( mq: D#HyperCube, point2: D#PointLike ) = {
         val point = pointView( value )
         mq.greatestInteresting( point, point2 )
      }

      def orthantIndexIn( iq: D#HyperCube ) : Int = {
         iq.indexOf( pointView( value ))
      }

      /**
       * For a leaf (which does not have a subtree),
       * the `startOrder` is identical to its `order`.
       */
      def startOrder : Order = order

      /**
       * For a leaf (which does not have a subtree),
       * the `stopOrder` is identical to its `order`.
       */
      def stopOrder( implicit tx: S#Tx ) : Order = order

      def shortString = "leaf(" + value + ")"
      override def toString() = "Leaf(" + value + ")"

      def removeAndDispose()( implicit tx: S#Tx ) {
         totalOrder.remove( order )
         dispose()
      }
   }

   /**
    * Nodes are defined by a hyperCube area as well as a list of children,
    * as well as a pointer `next` to the corresponding node in the
    * next highest tree. A `Branch` also provides various search methods.
    */
   protected sealed trait BranchImpl extends Node with Mutable[ S ] {
      /**
       * Returns the child for a given
       * orthant index
       */
      def child( idx: Int )( implicit tx: S#Tx ) : Node

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
      def findP0( point: D#PointLike )( implicit tx: S#Tx ) : LeftBranch

      /**
       * Assuming that the given `leaf` is a child of this node,
       * removes the child from this node's children. This method
       * will perform further clean-up such as merging this node
       * with its parent if it becomes uninteresting as part of the
       * removal.
       */
      def removeImmediateLeaf( leaf: LeafImpl )( implicit tx: S#Tx ) : Unit

      /**
       * Returns the hyper-cube covered by this node
       */
      def hyperCube: D#HyperCube

      /**
       * Returns the corresponding interesting
       * node in Qi+1, or `null` if no such
       * node exists.
       */
      final def next( implicit tx: S#Tx ) : RightBranch = nextRef.get

      final def nextOption( implicit tx: S#Tx ) : Option[ BranchImpl ] = Option( next )

      /**
       * Sets the corresponding interesting
       * node in Qi+1.
       */
      final def next_=( node: RightBranch )( implicit tx: S#Tx ) {
         nextRef.set( node )
      }

      protected def nextRef: S#Ref[ RightBranch ]

      def prev: BranchImpl

      final def prevOption: Option[ BranchImpl ] = Option( prev )

      final def union( mq: D#HyperCube, point2: D#PointLike ) = {
         val q = hyperCube
         mq.greatestInteresting( q, point2 )
      }

      final def orthantIndexIn( iq: D#HyperCube ) : Int = iq.indexOf( hyperCube )

      /**
       * The reverse process of `findP0`: Finds the lowest
       * common ancestor interesting node of this node
       * which is also contained in Qi+1. Returns this node
       * in Qi+1, or null if no such node exists.
       */
      def findPN( implicit tx: S#Tx ) : RightBranch

      /**
       * Called when a leaf has been removed from the node.
       * The node may need to cleanup after this, e.g. promote
       * an underfull node upwards.
       */
      protected def leafRemoved()( implicit tx: S#Tx ) : Unit

      def nodeName : String
      final def shortString = nodeName + "(" + hyperCube + ")"

//      override def toString = shortString +
//         Seq.tabulate( numOrthants )( i => child( i ).shortString )
//            .mkString( " : children = [", ", ", "]" )

      final def isLeaf  = false
      final def isBranch  = true
      final def asBranch : BranchImpl = this
      final def asLeaf : LeafImpl = opNotSupported
   }

   /**
    * An inner non empty tree element has a mutable parent node.
    */
   sealed trait Child extends Node {
      def parent( implicit tx: S#Tx ): BranchImpl
   }

   /**
    * Utility trait which elements the rightward search `findPN`.
    */
   protected sealed trait ChildBranch extends BranchImpl with Child {
      final def findPN( implicit tx: S#Tx ) : RightBranch = {
         val n = next
         if( n eq null ) parent.findPN else n
      }
   }

   /**
    * A right tree node implementation provides more specialized child nodes
    * of type `RightChild`. It furthermore defines the node in Qi-1 via the
    * `prev` method.
    */
   protected sealed trait RightBranch extends BranchImpl with Mutable[ S ] {
      protected def children: Array[ S#Ref[ RightChild ]]

      def prev : BranchImpl
      final def child( idx: Int )( implicit tx: S#Tx ) : RightChild = children( idx ).get
      final def updateChild( idx: Int, c: RightChild )( implicit tx: S#Tx ) {
         children( idx ).set( c )
      }

      final def findP0( point: D#PointLike )( implicit tx: S#Tx ) : LeftBranch = {
         val qidx = hyperCube.indexOf( point )
         val c = child( qidx )
         val n = if( (c ne null) && c.isBranch ) {
            val cn = c.asBranch
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
      final def insert( leaf: LeafImpl )( implicit tx: S#Tx ) {
         val point   = pointView( leaf.value )
         val qidx    = hyperCube.indexOf( point )
         val old      = child( qidx )
         if( old eq null ) {
            updateChild( qidx, leaf )
            leaf.parent       = this
         } else {
            // determine the greatest interesting square for the new
            // intermediate node to create
            val qn2     = old.union( hyperCube.orthant( qidx ), point )
            // find the corresponding node in the lower tree
            var pPrev   = prev
            while( pPrev.hyperCube != qn2 ) {
//               pPrev = pPrev.child( leaf.orthantIndexIn( pPrev.hyperCube )).asBranch
               pPrev = pPrev.child( pPrev.hyperCube.indexOf( point )).asBranch
            }
            val n2      = newNode( qidx, pPrev, qn2 )

//            val c2      = n2.children
            val oidx    = old.orthantIndexIn( qn2 )
//            c2( oidx )  = old
            n2.updateChild( oidx, old )
            // This is a tricky bit! And a reason
            // why should eventually try to do without
            // parent pointers at all. Since `old`
            // may be a leaf whose parent points
            // to a higher level tree, we need to
            // check first if the parent is `this`,
            // and if so, adjust the parent to point
            // to the new intermediate node `ne`!
            if( old.parent == this ) old.updateParentRight( n2 )
//            val lidx    = leaf.orthantIndexIn( qn2 )
            val lidx    = qn2.indexOf( point )
//            c2( lidx )  = leaf
            n2.updateChild( lidx, leaf )
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
      @inline private def newNode( qidx: Int, prev: BranchImpl, iq: D#HyperCube )
                                 ( implicit tx: S#Tx ) : RightChildBranch = {
         val sz         = children.length
//         val ch         = new Array[ RightChild[ S, D, A ]]( sz )
         val ch         = system.newRefArray[ RightChild ]( sz )
         var i = 0; while( i < sz ) {
            ch( i )     = system.newRef[ RightChild ]( null )
         i += 1 }
         val parentRef  = system.newRef[ RightBranch ]( this )
         val rightRef   = system.newRef[ RightBranch ]( null )
         val n          = new RightChildBranch( system.newID, parentRef, prev, iq, ch, rightRef )
         prev.next      = n
         updateChild( qidx, n )
         n
      }

      final def removeImmediateLeaf( leaf: LeafImpl )( implicit tx: S#Tx ) {
         val point   = pointView( leaf.value )
//         val qidx    = leaf.orthantIndexIn( hyperCube )
         val qidx    = hyperCube.indexOf( point )
         assert( child( qidx ) == leaf, "Internal error - expected leaf not found" )
         updateChild( qidx, null )
         var newParent  = prev
         var pidx       = qidx
         while( true ) {
            val c = newParent.child( pidx )
            if( c.isBranch ) {
               val sn = c.asBranch
               newParent   = sn
//               pidx        = leaf.orthantIndexIn( sn.hyperCube )
               pidx        = sn.hyperCube.indexOf( point )
            } else {
               val sl = c.asLeaf
               assert( sl == leaf, "Internal error - diverging leaves : " + leaf + " versus " + sl )
               leafRemoved()
               leaf.parent = newParent
               return
            }
         }
      }
   }

   /**
    * A left tree node implementation provides more specialized child nodes
    * of type `LeftChild`. It furthermore defines a resolution method
    * `findImmediateLeaf` which is typically called after arriving here
    * from a `findP0` call.
    */
   protected sealed trait LeftBranch extends BranchImpl with LeftNode with Mutable[ S ] {
      /**
       * For a `LeftBranch`, all its children are more specific
       * -- they are instances of `LeftChild` and thus support
       * order intervals.
       */
      protected def children: Array[ S#Ref[ LeftChild ]]

      /**
       * The stop-order of a left node is now always implicitly defined.
       * It is not a real entry in the total-order. Instead it is either
       * the start-order, if the node is empty, otherwise the stop-order
       * of the right-most non-empty child of the node. Since only `append`
       * is used on the order entries, this totally suffices for maintaining
       * the tree's binarization.
       */
      final def stopOrder( implicit tx: S#Tx ) : Order = {
         var res = startOrder
         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = child( i )
            if( c ne null ) {
               res = c.stopOrder
            }
         i +=1 }
         res
      }

      /**
       * Note that `prev` will not be called as part of this octree implementation
       * which smartly distinguishes between left and right nodes. It is merely here
       * to satisfy the `Branch` interface of `SkipOctree`.
       */
      final def prev : BranchImpl = null

      final def child( idx: Int )( implicit tx: S#Tx ) : LeftChild = children( idx ).get
      final def updateChild( idx: Int, c: LeftChild )( implicit tx: S#Tx ) {
         children( idx ).set( c )
      }

      final def findP0( point: D#PointLike )( implicit tx: S#Tx ) : LeftBranch = {
         val qidx = hyperCube.indexOf( point )
         val c    = child( qidx )
         if( (c ne null) && c.isBranch ) {
            val n = c.asBranch
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
      final def findImmediateLeaf( point: D#PointLike )( implicit tx: S#Tx ) : LeafImpl = {
         val qidx = hyperCube.indexOf( point )
         val c = child( qidx )
         if( (c ne null) && c.isLeaf ) {
            val l = c.asLeaf
            if( pointView( l.value ) == point ) l else null
         } else null
      }

      final def removeImmediateLeaf( leaf: LeafImpl )( implicit tx: S#Tx ) {
         val point   = pointView( leaf.value )
         val qidx    = hyperCube.indexOf( point )
//         val sz      = children.length
         assert( child( qidx ) == leaf, "Internal error - expected leaf not found" )
         updateChild( qidx, null )
         leafRemoved()
         leaf.removeAndDispose()
      }

      final def insert( point: D#PointLike, value: A )( implicit tx: S#Tx ) : LeafImpl = {
         val qidx = hyperCube.indexOf( point )
         val old  = child( qidx )
         if( old eq null ) {
            newLeaf( qidx, /* point, */ value ) // (this adds it to the children!)
         } else {
            // define the greatest interesting square for the new node to insert
            // in this node at qidx:
            val qn2              = old.union( hyperCube.orthant( qidx ), point )
            // create the new node (this adds it to the children!)
            val n2               = newNode( qidx, qn2 )
            val oidx             = old.orthantIndexIn( qn2 )
//            n2.children( oidx )  = old
            n2.updateChild( oidx, old )
            val lidx             = qn2.indexOf( point )
            // This is a tricky bit! And a reason
            // why should eventually try to do without
            // parent pointers at all. Since `old`
            // may be a leaf whose parent points
            // to a higher level tree, we need to
            // check first if the parent is `this`,
            // and if so, adjust the parent to point
            // to the new intermediate node `ne`!
            if( old.parent == this ) old.updateParentLeft( n2 )
            n2.newLeaf( lidx, /* point, */ value )
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
      private def newLeaf( qidx: Int, value: A )( implicit tx: S#Tx ) : LeafImpl = {
         val parentRef  = system.newRef[ BranchImpl ]( this )
         val l          = new LeafImpl( system.newID, value, newChildOrder( qidx ), parentRef )
         updateChild( qidx, l )
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
      private def newChildOrder( qidx: Int )( implicit tx: S#Tx ) : Order = {
         var pre = startOrder
         var i = 0; while( i < qidx ) {
            val c = child( i )
            if( c ne null ) {
               pre = c.stopOrder
            }
         i +=1 }
         totalOrder.insertAfter( pre )
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
      @inline private def newNode( qidx: Int, iq: D#HyperCube )( implicit tx: S#Tx ) : LeftChildBranch = {
         val sz         = children.length
         val ch         = system.newRefArray[ LeftChild ]( sz )
         var i = 0; while( i < sz ) {
            ch( i )     = system.newRef[ LeftChild ]( null )
         i += 1 }
         val parentRef  = system.newRef[ LeftBranch ]( this )
         val rightRef   = system.newRef[ RightBranch ]( null )
         val n          = new LeftChildBranch( system.newID, parentRef, iq, newChildOrder( qidx ), ch, rightRef )
         updateChild( qidx, n )
         n
      }
   }

   protected sealed trait TopBranch extends BranchImpl with Mutable[ S ] {
      final def findPN( implicit tx: S#Tx ) : RightBranch = next
   }

   /*
    * Serialization-id: 2
    */
   private def readTopLeftBranch( in: DataInput, id: S#ID ) : TopLeftBranch = {
      val startOrder = totalOrder.read( in )
      val sz         = numOrthants
      val ch         = system.newRefArray[ LeftChild ]( sz )
      var i = 0; while( i < sz ) {
         ch( i )     = system.readRef[ LeftChild ]( in )
      i += 1 }
      val nextRef    = system.readRef[ RightBranch ]( in )
      new TopLeftBranch( id, hyperCube, startOrder, ch, nextRef )
   }
   protected final class TopLeftBranch( val id: S#ID, val hyperCube: D#HyperCube, val startOrder: Order,
                                      protected val children: Array[ S#Ref[ LeftChild ]],
                                      protected val nextRef: S#Ref[ RightBranch ])
   extends LeftBranch with TopBranch with Mutable[ S ] {

      // that's alright, we don't need to do anything special here
      protected def leafRemoved()( implicit tx: S#Tx ) {}

      protected def disposeData()( implicit tx: S#Tx ) {
         // startOrder.dispose() -- no, because tree will call totalOrder.dispose anyway!
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).dispose()
         i += 1 }
         nextRef.dispose()
      }

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 2 )
         // no need to write the hyperCube?
         startOrder.write( out )
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).write( out )
         i += 1 }
         nextRef.write( out )
      }

      def nodeName = "top-left"
   }

   /*
    * Serialization-id: 3
    */
   private def readLeftChildBranch( in: DataInput, id: S#ID ) : LeftChildBranch = {
      val parentRef  = system.readRef[ LeftBranch ]( in )
      val hyperCube  = hyperSerializer.read( in )
      val startOrder = totalOrder.read( in )
      val sz         = numOrthants
      val ch         = system.newRefArray[ LeftChild ]( sz )
      var i = 0; while( i < sz ) {
         ch( i )     = system.readRef[ LeftChild ]( in )
      i += 1 }
      val nextRef    = system.readRef[ RightBranch ]( in )
      new LeftChildBranch( id, parentRef, hyperCube, startOrder, ch, nextRef )
   }
   private final class LeftChildBranch( val id: S#ID, parentRef: S#Ref[ LeftBranch ], val hyperCube: D#HyperCube,
                                        val startOrder: Order,
                                        protected val children: Array[ S#Ref[ LeftChild ]],
                                        protected val nextRef: S#Ref[ RightBranch ])
   extends LeftBranch with ChildBranch with LeftChild {
      def nodeName = "inner-left"

      def updateParentLeft( p: LeftBranch )( implicit tx: S#Tx ) { parent = p }
      def parent( implicit tx: S#Tx ) : LeftBranch = parentRef.get
      def parent_=( node: LeftBranch )( implicit tx: S#Tx ) {
         parentRef.set( node )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         parentRef.dispose()
         startOrder.dispose()
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).dispose()
         i += 1 }
         nextRef.dispose()
      }

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 3 )
         parentRef.write( out )
         hyperSerializer.write( hyperCube, out )
         startOrder.write( out )
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).write( out )
         i += 1 }
         nextRef.write( out )
      }

      private def removeAndDispose()( implicit tx: S#Tx ) {
         assert( next eq null )
         totalOrder.remove( startOrder )
         dispose()
      }

      // make sure the node is not becoming uninteresting, in which case
      // we need to merge upwards
      protected def leafRemoved()( implicit tx: S#Tx ) {
         var lonely: LeftChild = null
         var numNonEmpty = 0
         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = child( i )
            if( c ne null ) {
               numNonEmpty += 1
               lonely = c
            }
         i += 1 }
         if( numNonEmpty == 1 ) {   // gotta remove this node and put remaining non empty element in parent
            val myIdx = parent.hyperCube.indexOf( hyperCube )
            val p = parent
            p.updateChild( myIdx, lonely )
            if( lonely.parent == this ) lonely.updateParentLeft( p )
            removeAndDispose()
         }
      }
   }

   /*
    * Serialization-id: 4
    */
   private def readTopRightBranch( in: DataInput, id: S#ID ) : TopRightBranch = {
      val prev = system.readMut[ TopBranch ]( in )
      val sz   = numOrthants
      val ch   = system.newRefArray[ RightChild ]( sz )
      var i = 0; while( i < sz ) {
         ch( i ) = system.readRef[ RightChild ]( in )
      i += 1 }
      val nextRef = system.readRef[ RightBranch ]( in )
      new TopRightBranch( id, hyperCube, prev, ch, nextRef )
   }
   protected final class TopRightBranch( val id: S#ID, val hyperCube: D#HyperCube, val prev: TopBranch,
                                       protected val children: Array[ S#Ref[ RightChild ]],
                                       protected val nextRef: S#Ref[ RightBranch ])
   extends RightBranch with TopBranch {

      def nodeName = "top-right"

//      def hyperCube = impl.hyperCube

      protected def disposeData()( implicit tx: S#Tx ) {
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).dispose()
         i += 1 }
         nextRef.dispose()
      }

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 4 )
         // no need to write the hypercube!
         prev.write( out )
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).write( out )
         i += 1 }
         nextRef.write( out )
      }

      // remove this node if it empty now and right-node tree
      protected def leafRemoved()( implicit tx: S#Tx ) {
         if( next ne null ) return

         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = child( i )
            if( c ne null ) return // node not empty, abort the check
         i += 1 }

         // ok, we are the right most tree and the node is empty...
         removeAndDispose()
      }

      private def removeAndDispose()( implicit tx: S#Tx ) {
         assert( next eq null )
         assert( lastTree == this )
         lastTree    = prev
         prev.next   = null
         dispose()
      }
   }

   /*
    * Serialization-id: 5
    */
   private def readRightChildBranch( in: DataInput, id: S#ID ) : RightChildBranch = {
      val parentRef  = system.readRef[ RightBranch ]( in )
      val prev       = system.readMut[ BranchImpl ]( in )
      val hyperCube  = hyperSerializer.read( in )
      val sz         = numOrthants
      val ch         = system.newRefArray[ RightChild ]( sz )
      var i = 0; while( i < sz ) {
         ch( i )     = system.readRef[ RightChild ]( in )
      i += 1 }
      val nextRef    = system.readRef[ RightBranch ]( in )
      new RightChildBranch( id, parentRef, prev, hyperCube, ch, nextRef )
   }
   private final class RightChildBranch( val id: S#ID, parentRef: S#Ref[ RightBranch ],
                                         val prev: BranchImpl, val hyperCube: D#HyperCube,
                                         protected val children: Array[ S#Ref[ RightChild ]],
                                         protected val nextRef: S#Ref[ RightBranch ])
   extends RightBranch with ChildBranch with RightChild {

      def nodeName = "inner-right"

      def updateParentRight( p: RightBranch )( implicit tx: S#Tx ) { parent = p }

      protected def disposeData()( implicit tx: S#Tx ) {
         parentRef.dispose()
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).dispose()
         i += 1 }
         nextRef.dispose()
      }

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 5 )
         parentRef.write( out )
         prev.write( out )
         hyperSerializer.write( hyperCube, out )
         var i = 0; val sz = children.length; while( i < sz ) {
            children( i ).write( out )
         i += 1 }
         nextRef.write( out )
      }

      private def removeAndDispose()( implicit tx: S#Tx ) {
         assert( next eq null )
         prev.next   = null
//            prev        = null
         dispose()
      }

      def parent( implicit tx: S#Tx ) : RightBranch = parentRef.get
      def parent_=( node: RightBranch )( implicit tx: S#Tx ) {
         parentRef.set( node )
      }

      // make sure the node is not becoming uninteresting, in which case
      // we need to merge upwards
      protected def leafRemoved()( implicit tx: S#Tx ) {
         var lonely: RightChild = null
         var numNonEmpty = 0
         val sz = children.length
         var i = 0; while( i < sz ) {
            val c = child( i )
            if( c ne null ) {
               numNonEmpty += 1
               lonely = c
            }
         i += 1 }
         if( numNonEmpty == 1 ) {   // gotta remove this node and put remaining non empty element in parent
            val myIdx = parent.hyperCube.indexOf( hyperCube )
            val p = parent
            p.updateChild( myIdx, lonely )
            if( lonely.parent == this ) lonely.updateParentRight( p )
            removeAndDispose()
         }
      }
   }
}
