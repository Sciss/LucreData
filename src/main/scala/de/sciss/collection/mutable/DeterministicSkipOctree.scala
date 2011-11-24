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
package mutable

import geom.Space

/**
 * A determinstic skip octree as outlined in the paper by Eppstein et al.
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
   def empty[ D <: Space[ D ], A ]( space: D, hyperCube: D#HyperCube, skipGap: Int = 2 )
                                ( implicit view: A => D#Point ) : SkipOctree[ D, A ] =
      new TreeImpl[ D, A ]( space, hyperCube, skipGap, view )

   def apply[ D <: Space[ D ], A <% D#Point ]( space: D, hyperCube: D#HyperCube, skipGap: Int = 2 )( xs: A* ) : SkipOctree[ D, A ] = {
      val t = empty[ D, A ]( space, hyperCube, skipGap )
      xs.foreach( t.+=( _ ))
      t
   }

   private final class TreeImpl[ D <: Space[ D ], A ]( val space: D, val hyperCube: D#HyperCube, _skipGap: Int, val pointView: A => D#Point )
   extends impl.SkipOctreeImpl[ D, A ] {
      tree =>

      val numOrthants: Int = 1 << space.dim  // 4 for R2, 8 for R3, 16 for R4, etc.
      val totalOrder = TotalOrder()

//      val totalOrder = TotalOrder( new TotalOrder.RelabelObserver {
//         def beforeRelabeling( first: TotalOrder.EntryLike, num: Int ) {
//
//var last = first; var i = 1; while( i < num ) { last = last.next; i += 1 }
//println( "BEFORE RELABEL : " + first + " - " + last + " (num = " + num + ")" )
//         }
//         def afterRelabeling( first: TotalOrder.EntryLike, num: Int ) {
//var last = first; var i = 1; while( i < num ) { last = last.next; i += 1 }
//println( "AFTER  RELABEL : " + first + " - " + last + " (num = " + num + ")" )
//         }
//      })

      private var tailVar: TopNode = TopLeftNode
      private val skipList: SkipList[ Leaf ] = {
         implicit def maxKey = MaxKey( MaxLeaf )
         if( _skipGap < 2 ) {
            require( _skipGap == 1, "Illegal skipGap value (" + _skipGap + ")" )
            LLSkipList.empty[ Leaf ]( KeyObserver )
         } else {
            HASkipList.empty[ Leaf ]( _skipGap, KeyObserver )
         }
      }

      def headTree : QNode = TopLeftNode
      def lastTree : QNode = tailVar

      type InOrder = totalOrder.Entry

      protected def findLeaf( point: D#Point ) : QLeaf = {
         val p0 = tailVar.findP0( point )
         p0.findImmediateLeaf( point )
      }

      protected def insertLeaf( elem: A ) : QLeaf = {
         val point   = pointView( elem )
         require( hyperCube.contains( point ), point.toString + " lies out of root hyper-cube " + hyperCube )

         val p0      = tailVar.findP0( point )
         val oldLeaf = p0.findImmediateLeaf( point )
         if( oldLeaf == null ) {
            val leaf = p0.insert( point, elem )
//if( leaf.order == totalOrder.max ) {
//   println( "Ouch!" )
//}
//try {
            skipList.add( leaf )
//} catch {
//   case e =>
//      println( "Wappa dappa. leaf.order = " + leaf.order + " (prev =" + leaf.order.prev + ", next =" + leaf.order.next + ")" )
//      throw e
//}
         } else {
            oldLeaf.value = elem
         }
         oldLeaf
      }

      protected def removeLeaf( point: D#Point ) : QLeaf = {
         if( !hyperCube.contains( point )) return null

         // "To insert or delete a point y into or from S, we first search the
         // quadtree structure to locate y in each Qi ..."
         val p0 = tailVar.findP0( point )

         // "... Then we insert or delete y
         // in the binary Q0 and update our total order."

         val l = p0.findImmediateLeaf( point )
         if( l != null ) {
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

         l
      }

      def iterator: Iterator[ A ] = new Iter( skipList.iterator )

      private final class Iter( underlying: Iterator[ Leaf ]) extends Iterator[ A ] {
         def next() : A = {
            val leaf = underlying.next()
            leaf.value
         }
         def hasNext : Boolean = underlying.hasNext

//         override def toString() = "Octree.Iterator"
      }

      private object KeyObserver extends SkipList.KeyObserver[ Leaf ] {
         def keyUp( l: Leaf ) {
//println( "up : " + l )
            // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
            //  then traverse upwards in Qi until we find the lowest
            //  ancestor q of x which is also interesting in Qi+1.
            //  (This is the reversed process of searching x in Qi
            //  with q = pi,start = pi+1,end so it takes at most 6
            //  steps by Lemma 5.) Then we go to the same square q
            //  in Qi+1 and insert x."

            val pNext0  = l.parent.findPN // ( path, 0 )
            val pNext   = if( pNext0 == null ) { // create new level
               val res = new TopRightNode( tailVar )
               tailVar = res
               res
            } else pNext0
            pNext.insert( l ) // , path )
         }

         def keyDown( l: Leaf ) {
//println( "down : " + l )
            // "To delete x from Qi we go from xi to the smallest interesting
            //  square pi(x) containing x in Qi following the pointers. Then
            //  the deletion given pi(x) is as described in Section 2.3."

            val p = l.parent
            p.removeImmediateLeaf( l )
         }
      }

      /**
       * A child is an object that can be
       * stored in a orthant of a node.
       */
      private sealed trait Child extends Q {
         def shortString : String
      }
      /**
       * A left child is one which is stored in Q0.
       * This is either empty or an object (`LeftChild`)
       * which provides start and stop markers for its
       * position in the in-order list.
       * That is, `LeftChild` is either of
       *
       * - `Empty`
       * - `Leaf`
       * - `InnerLeftBranch`
       */
      private sealed trait LeftChild extends Child

      /**
       * A right child is one which is stored in Qi, i > 0.
       * This is either empty or an instance of `RightChild`.
       * That is, `RightChild` is either of
       *
       * - `Empty`
       * - `Leaf`
       * - `InnerRightBranch`
       */
      private sealed trait RightChild extends Child

      /**
       * A dummy object indicating a vacant orthant in a node.
       */
      private case object Empty extends LeftChild with RightChild with QEmpty {
         def shortString = "empty"
         override def toString = shortString
      }

      /**
       * An object denoting a filled orthant of a node.
       * This is either a leaf or another node. This trait
       * supports the operations of calculating the greatest
       * interesting hyper-cube with regard to another point,
       * as well as determining its position within an
       * encompassing hyper-cube.
       */
      private sealed trait NonEmpty extends Child {
         /**
          * Computes the greatest interesting hyper-cube within
          * a given hyper-cube `mq` so that this (leaf's or node's)
          * hyper-cube and the given point will be placed in
          * separated orthants of this resulting hyper-cube.
          */
         def union( mq: D#HyperCube, point: D#Point ) : D#HyperCube

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
      private sealed trait LeftNonEmpty extends NonEmpty {
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
         def startOrder: InOrder
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
         def stopOrder: InOrder
      }

      /**
       * A common trait used in pattern matching, comprised of `Leaf` and `InnerLeftBranch`.
       */
      private sealed trait LeftInnerNonEmpty extends LeftNonEmpty with InnerNonEmpty with LeftChild {
         def parentLeft_=( p: LeftNode ) : Unit
      }

      /**
       * A common trait used in pattern matching, comprised of `Leaf` and `InnerRightBranch`.
       */
      private sealed trait RightInnerNonEmpty extends InnerNonEmpty with RightChild {
         def parentRight_=( p: RightNode ) : Unit
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
      private sealed trait Leaf extends LeftInnerNonEmpty with RightInnerNonEmpty with Ordered[ Leaf ] with QLeaf {
         def value : A
         def value_=( v: A ) : Unit  // hmmm, not so nice...

         final def parentLeft_=( p: LeftNode )   { parent_=( p )}
         final def parentRight_=( p: RightNode ) { parent_=( p )}
         def parent_=( p: Node ) : Unit

         /**
          * The position of this leaf in the in-order list.
          */
         def order : InOrder

         /**
          * Leafs are ordered by the tree's in-order traversal,
          * where the quadrants I+II and III+IV can be thought
          * of as dummy nodes to binarize the octree. That is
          * to say, in a node, the child order corresponds to
          * their quadrant indices (I < II < III < IV).
          */
         final def compare( that: Leaf ) : Int = order.compare( that.order )

         final def union( mq: D#HyperCube, point2: D#Point ) = {
            val point   = pointView( value )
            mq.greatestInteresting( point, point2 )
         }

         final def orthantIndexIn( iq: D#HyperCube ) : Int = iq.indexOf( pointView( value ))

         /**
          * For a leaf (which does not have a subtree),
          * the `startOrder` is identical to its `order`.
          */
         final def startOrder : InOrder = order

         /**
          * For a leaf (which does not have a subtree),
          * the `stopOrder` is identical to its `order`.
          */
         final def stopOrder : InOrder = order

         def dispose() : Unit
      }

      /**
       * Nodes are defined by a hyperCube area as well as a list of children,
       * as well as a pointer `next` to the corresponding node in the
       * next highest tree. A `Branch` also provides various search methods.
       */
      private sealed trait Node extends NonEmpty with QNode {
         /**
          * Returns the child for a given
          * orthant index
          */
         def child( idx: Int ) : Child

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
         def findP0( point: D#Point ) : LeftNode

         /**
          * Assuming that the given `leaf` is a child of this node,
          * removes the child from this node's children. This method
          * will perform further clean-up such as merging this node
          * with its parent if it becomes uninteresting as part of the
          * removal.
          */
         def removeImmediateLeaf( leaf: Leaf ) : Unit

         /**
          * Returns the hyper-cube covered by this node
          */
         def hyperCube: D#HyperCube

         /**
          * Returns the corresponding interesting
          * node in Qi+1, or `null` if no such
          * node exists.
          */
         def next: RightNode

         /**
          * Sets the corresponding interesting
          * node in Qi+1.
          */
         def next_=( n: RightNode ) : Unit

         final def union( mq: D#HyperCube, point2: D#Point ) = {
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
         def findPN : RightNode

         /**
          * Called when a leaf has been removed from the node.
          * The node may need to cleanup after this, e.g. promote
          * an underfull node upwards.
          */
         protected def leafRemoved() : Unit

         def nodeName : String
         final def shortString = nodeName + "(" + hyperCube + ")"

         override def toString = shortString +
            Seq.tabulate( numOrthants )( i => child( i ).shortString )
               .mkString( " : children = [", ", ", "]" )

         final def prevOption: Option[ QNode ] = Option( prev )
         final def nextOption: Option[ QNode ] = Option( next )
      }

      /**
       * An inner non empty tree element has a mutable parent node.
       */
      private sealed trait InnerNonEmpty extends NonEmpty {
         def parent: Node
      }

      /**
       * Utility trait which elements the rightward search `findPN`.
       */
      private sealed trait InnerNode extends Node with InnerNonEmpty {
         final def findPN : RightNode = {
            val n = next
            if( n == null ) parent.findPN else n
         }
      }

      /**
       * A right tree node implementation provides more specialized child nodes
       * of type `RightChild`. It furthermore defines the node in Qi-1 via the
       * `prev` method.
       */
      private sealed trait RightNode extends Node with NonEmpty {
         final val children = Array.fill[ RightChild ]( numOrthants )( Empty )
         final var next : RightNode = null

         def prev : Node
         final def child( idx: Int ) : RightChild = children( idx )

         final def findP0( point: D#Point ) : LeftNode = {
            val qidx = hyperCube.indexOf( point )
            children( qidx ) match {
               case n: Node if( n.hyperCube.contains( point )) => n.findP0( point )
               case _ => prev.findP0( point )
            }
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
         final def insert( leaf: Leaf ) {
            val point   = pointView( leaf.value )
            val qidx    = hyperCube.indexOf( point )
            children( qidx ) match {
               case Empty =>
                  children( qidx )  = leaf
                  leaf.parent       = this

               case old: RightInnerNonEmpty =>
                  // determine the greatest interesting square for the new
                  // intermediate node to create
                  val qn2     = old.union( hyperCube.orthant( qidx ), point )
                  // find the corresponding node in the lower tree
                  var pPrev   = prev
                  while( pPrev.hyperCube != qn2 ) pPrev = pPrev.child( leaf.orthantIndexIn( pPrev.hyperCube )) match {
                     case n: Node => n
                     case _ => sys.error( "Internal error -- structural problem. No node leading to leaf" )
                  }
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
         @inline private def newNode( qidx: Int, prev: Node, iq: D#HyperCube ) : InnerRightNode = {
            val n = new InnerRightNode( this, prev, iq )
            children( qidx ) = n
            n
         }

         final def removeImmediateLeaf( leaf: Leaf ) {
            var qidx = 0; while( qidx < numOrthants ) {
               if( children( qidx ) == leaf ) {
                  children( qidx ) = Empty
                  var newParent  = prev
                  var pidx       = qidx
                  while( true ) {
                     newParent.child( pidx ) match {
                        case sn: Node =>
                           newParent   = sn
                           pidx        = leaf.orthantIndexIn( sn.hyperCube )
                        case sl: Leaf =>
                           assert( sl == leaf, "Internal error - diverging leaves : " + leaf + " versus " + sl )
                           leafRemoved()
                           leaf.parent = newParent
                           return
                        case _ =>
                           assert( false, "Internal error - could not find parent of leaf in previous level : " + leaf )
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
      private sealed trait LeftNode extends Node with LeftNonEmpty {
         /**
          * For a `LeftBranch`, all its children are more specific
          * -- they are instances of `LeftChild` and thus support
          * order intervals.
          */
         final val children = Array.fill[ LeftChild ]( numOrthants )( Empty )
         final var next : RightNode = null

         /**
          * The stop-order of a left node is now always implicitly defined.
          * It is not a real entry in the total-order. Instead it is either
          * the start-order, if the node is empty, otherwise the stop-order
          * of the right-most non-empty child of the node. Since only `append`
          * is used on the order entries, this totally suffices for maintaining
          * the tree's binarization.
          */
         final def stopOrder : InOrder = {
            var res = startOrder
            var i = 0; while( i < numOrthants ) {
               children( i ) match {
                  case n: LeftNonEmpty => res = n.stopOrder
                  case _ =>
               }
            i +=1 }
            res
         }

         /**
          * Note that `prev` will not be called as part of this octree implementation
          * which smartly distinguishes between left and right nodes. It is merely here
          * to satisfy the `QNode` interface of `SkipOctree`.
          */
         final def prev : QNode = null

         final def child( idx: Int ) : Child = children( idx )

         final def findP0( point: D#Point ) : LeftNode = {
            val qidx = hyperCube.indexOf( point )
            child( qidx ) match {
               case n: Node if( n.hyperCube.contains( point )) => n.findP0( point )
               case _ => this
            }
         }

         /**
          * After arriving at this node from a `findP0` call, this resolves
          * the given point to an actual leaf.
          *
          * @return  the `Leaf` child in this node associated with the given
          *          `point`, or `null` if no such leaf exists.
          */
         final def findImmediateLeaf( point: D#Point ) : Leaf = {
            val qidx = hyperCube.indexOf( point )
            child( qidx ) match {
               case l: Leaf if( pointView( l.value ) == point ) => l
               case _ => null
            }
         }

         final def removeImmediateLeaf( leaf: Leaf ) {
            var qidx = 0; while( qidx < numOrthants ) {
               if( children( qidx ) == leaf ) {
                  children( qidx ) = Empty
                  leafRemoved()
                  leaf.dispose()
                  return
               }
            qidx += 1 }
         }

         final def insert( point: D#Point, value: A ) : Leaf = {
            val qidx = hyperCube.indexOf( point )
            children( qidx ) match {
               case Empty =>
                  newLeaf( qidx, point, value ) // (this adds it to the children!)

               case old: LeftInnerNonEmpty =>
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
         private def newLeaf( qidx: Int, point: D#Point, value: A ) : Leaf = {
            val l = new LeafImpl( point, value, newChildOrder( qidx ))
            l.parent = this
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
         private def newChildOrder( qidx: Int ) : InOrder = {
            var pre = startOrder
            var i = 0; while( i < qidx ) {
               children( i ) match {
                  case n: LeftNonEmpty => pre = n.stopOrder
                  case _ =>
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
         @inline private def newNode( qidx: Int, iq: D#HyperCube ) : InnerLeftNode = {
            val n = new InnerLeftNode( this, iq, newChildOrder( qidx ))
            children( qidx ) = n
            n
         }
      }

      private sealed trait TopNode extends Node {
         final def hyperCube : D#HyperCube = tree.hyperCube
         final def findPN : RightNode = next
      }

      private object TopLeftNode extends LeftNode with TopNode {
         val startOrder = totalOrder.root

         // that's alright, we don't need to do anything special here
         protected def leafRemoved() {}

         def nodeName = "top-left"
      }

      private final class InnerLeftNode( var parent: LeftNode, val hyperCube: D#HyperCube, val startOrder: InOrder )
      extends LeftNode with InnerNode with LeftInnerNonEmpty {
         def nodeName = "inner-left"

         def parentLeft_=( p: LeftNode ) { parent = p }

         // might become important with persistent implementation
         def dispose() {
            assert( next == null )
            startOrder.remove()
         }

         // make sure the node is not becoming uninteresting, in which case
         // we need to merge upwards
         protected def leafRemoved() {
            var lonely: LeftInnerNonEmpty = null
            var numNonEmpty = 0
            var i = 0; while( i < numOrthants ) {
               children( i ) match {
                  case n: LeftInnerNonEmpty =>
                     numNonEmpty += 1
                     lonely = n
                  case _ =>
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
      private final class TopRightNode( val prev: TopNode ) extends RightNode with TopNode {
         prev.next = this

         def nodeName = "top-right"

         // remove this node if it empty now and right-node tree
         protected def leafRemoved() {
            if( next != null ) return

            var i = 0; while( i < numOrthants ) {
               children( i ) match {
                  case _: RightInnerNonEmpty => return   // node not empty, abort the check
                  case _ =>
               }
            i += 1 }

            // ok, we are the right most tree and the node is empty...
            dispose()
         }

         def dispose() {
            assert( next == null )
            assert( tailVar == this )
            tailVar     = prev
            prev.next   = null
//            prev        = null
         }
      }

      /**
       * Note that this instantiation sets the `prev`'s `next` field to this new node.
       */
      private final class InnerRightNode( var parent: RightNode, val prev: Node, val hyperCube: D#HyperCube )
      extends RightNode with InnerNode with RightInnerNonEmpty {
         prev.next = this
//assert( prev.hyperCube == hyperCube )

         def nodeName = "inner-right"

         def parentRight_=( p: RightNode ) { parent = p }

         def dispose() {
            assert( next == null )
            prev.next   = null
//            prev        = null
         }

         // make sure the node is not becoming uninteresting, in which case
         // we need to merge upwards
         protected def leafRemoved() {
            var lonely: RightInnerNonEmpty = null
            var numNonEmpty = 0
            var i = 0; while( i < numOrthants ) {
               children( i ) match {
                  case n: RightInnerNonEmpty =>
                     numNonEmpty += 1
                     lonely = n
                  case _ =>
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

      private object MaxLeaf extends Leaf {
         def point                        = space.maxPoint
         val order                        = totalOrder.max

         def value : A                       = unsupportedOp
         def value_=( v: A ) : Unit          = unsupportedOp
         def parent : Node                   = unsupportedOp
         def parent_=( n: Node ) : Unit      = unsupportedOp
         def dispose() : Unit                = unsupportedOp

         private def unsupportedOp : Nothing = sys.error( "Internal error -- Operation not supported" )

         def shortString = "max-leaf"
         override def toString = shortString
      }

      // it would be better to replace the leaf instead of updating value; however
      // the problem is there can be several pointers to a leaf, so at least for now,
      // let's not make life more complicated than necessary. also skip list would
      // need to be made 'replace-aware'.
      private final class LeafImpl( val point: D#Point, var value: A, val order: InOrder )
      extends Leaf {
         private var parentVar: Node = null
         def parent: Node = parentVar
         def parent_=( p: Node ) { parentVar = p }

         def shortString = "leaf(" + point + ")"
         override def toString = "Leaf(" + point + ", " + value + ")"

         def dispose() {
            parentVar   = null
//            value       = null.asInstanceOf[ A ]
            order.remove()
         }
      }
   }
}