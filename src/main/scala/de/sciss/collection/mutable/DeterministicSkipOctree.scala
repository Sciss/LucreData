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

import annotation.{switch, tailrec}
import geom.{Dim, Point2D}

object DeterministicSkipOctree {
   def empty[ D <: Dim[ D ], A ]( quad: D#QuadType, skipGap: Int = 2 )( implicit view: A => D#PointType ) : SkipOctree[ D, A ] =
      new TreeImpl[ D, A ]( quad, skipGap, view )

   def apply[ D <: Dim[ D ], A <% D#PointType ]( quad: D#QuadType, skipGap: Int = 2 )( xs: A* ) : SkipOctree[ D, A ] = {
      val t = empty[ D, A ]( quad, skipGap )
      xs.foreach( t.+=( _ ))
      t
   }

   private final class TreeImpl[ D <: Dim[ D ], A ]( val quad: D#QuadType, _skipGap: Int, val pointView: A => D#PointType )
   extends impl.SkipOctreeImpl[ D, A ] {
      tree =>

      val totalOrder = TotalOrder()
      private var tailVar: TopNode = TopLeftNode
      private val skipList: SkipList[ Leaf ] = {
         implicit def maxKey = MaxKey( MaxLeaf )
         if( _skipGap < 2 ) {
            require( _skipGap == 1, "Illegal skipGap value (" + _skipGap + ")" )
            LLSkipList.empty[ Leaf ]( KeyObserver ) // ( Ordering.ordered[ Leaf ], )
         } else {
//            val mf = implicitly[ Manifest[ Leaf ]]
            HASkipList.empty[ Leaf ]( _skipGap, KeyObserver ) // ( Ordering.ordered[ Leaf ], maxKey, mf )
         }
      } // 2-5 DSL

      private val numChildren = 4

      def headTree : QNode = TopLeftNode
      def lastTree : QNode = tailVar

      type InOrder = totalOrder.Entry

      protected def findLeaf( point: D#PointType ) : Leaf = {
         val p0 = tailVar.findP0( point )
         p0.findImmediateLeaf( point )
      }

      protected def insertLeaf( elem: A ) : Leaf = {
         val point   = pointView( elem )
         require( quad.contains( point ), point.toString + " lies out of root square " + quad )

         val p0      = tailVar.findP0( point )
         val oldLeaf = p0.findImmediateLeaf( point )
         if( oldLeaf == null ) {
            val leaf = p0.insert( point, elem )
            skipList.add( leaf )
         } else {
//            require( oldLeaf == null, "UPDATES NOT YET SUPPORTED" )
            oldLeaf.value = elem
         }
         oldLeaf
      }

      protected def removeLeaf( point: D#PointType ) : Leaf = {
         if( !quad.contains( point )) return null

         // "To insert or delete a point y into or from S, we first search the
         // quadtree structure to locate y in each Qi ..."
         val p0 = tailVar.findP0( point )

         // "... Then we insert or delete y
         // in the binary Q0 and update our total order."
//         p0.removeImmediateLeaf( point )

         val l = p0.findImmediateLeaf( point )
         if( l != null ) {
            // this will trigger removals from upper levels
            skipList.remove( l )
            // be careful: p0 at this point may be invalid
            // as the skiplist removal might have merged
            // it with its parent(s). we thus need to find
            // the parent of l in Q0 again!
//            p0.removeImmediateLeaf( l )

            val p = l.parent
            p.removeImmediateLeaf( l )
//            assert( p.isInstanceOf[ LeftNode ], "Internal error - final leaf parent should be left : " + l )
            assert( l.parent == null, "Internal error - leaf should be removed by now : " + l )
         }

         l
      }

//      protected def findLeaf( point: D#PointType ) : Leaf = {
//         tailVar.findP0( point ).findLeaf( point )
//      }

      def iterator = new Iterator[ A ] {
         val underlying = skipList.iterator
         def next() : A = {
            val leaf = underlying.next()
            leaf.value
         }
         def hasNext : Boolean = underlying.hasNext
      }

      object KeyObserver extends SkipList.KeyObserver[ Leaf ] {
         def keyUp( l: Leaf ) {
//println( "up : " + l )
            // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
            //  then traverse upwards in Qi until we find the lowest
            //  ancestor q of x which is also interesting in Qi+1.
            //  (This is the reversed process of searching x in Qi
            //  with q = pi,start = pi+1,end so it takes at most 6
            //  steps by Lemma 5.) Then we go to the same square q
            //  in Qi+1 and insert x."

            // hmmm... XXX This is super tricky. the ancestor test suite
            // takes up to 8 elements. how can be prove the maximum required size?
            val path = new Array[ Node ]( 9 )
            val q0o  = l.parent.findPN( path, 0 )
            val q0   = if( q0o == null ) { // create new level
               val res = new TopRightNode( tailVar )
               tailVar = res
               res
            } else q0o
            q0.insert( l, path )
//          path.find( _.quad == p1.quad ) ...
         }

         def keyDown( l: Leaf ) {
//println( "down : " + l )
            // "To delete x from Qi we go from xi to the smallest interesting
            //  square pi(x) containing x in Qi following the pointers. Then
            //  the deletion given pi(x) is as described in Section 2.3."

            val p = l.parent
            p.removeImmediateLeaf( l )
//            p match {
//               case rn: RightNode => l.parent = rn.prev.findParent( l )
//               case ln: LeftNode => sys.error( "Internal error - leaf unexpectedly removed : " + l )
//            }
         }
      }

      /**
       * A child is an object that can be
       * stored in a quadrant of a node.
       */
      sealed trait Child extends Q {
         def shortString : String
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
      sealed trait LeftChild extends Child

      /**
       * A right child is one which is stored in Qi, i > 0.
       * This is either empty or an instance of `RightInnerNonEmpty`.
       * That is, `RightChild` is either of
       *
       * - `Empty`
       * - `Leaf`
       * - `InnerRightNode`
       */
      sealed trait RightChild extends Child

      /**
       * A dummy object indicating a vacant quadrant in a node.
       */
      case object Empty extends LeftChild with RightChild with QEmpty {
         def shortString = "empty"
         override def toString = shortString
      }

      /**
       * An object denoting a filled quadrant of a node.
       * This is either a leaf or another node. This trait
       * supports the operations of calculating the greatest
       * interesting square with regard to another point,
       * as well as determining its position within an
       * encompassing quad.
       */
      sealed trait NonEmpty extends Child {
         /**
          * Computes the greatest interesting square within
          * a given quadrant `mq` so that this (leaf's or node's)
          * square and the given point will be placed in
          * separated quadrants of this resulting square.
          */
         def union( mq: D#QuadType, point: D#PointType ) : D#QuadType

         /**
          * Queries the quadrant index for this (leaf's or node's) square
          * with respect to a given outer square `iq`.
          */
         def quadIdxIn( iq: D#QuadType ) : Int
      }

      /**
       * A tree element in Q0 has markers for the
       * in-order traversal.
       */
      sealed trait LeftNonEmpty extends NonEmpty {
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
       * A common trait used in pattern matching, comprised of `Leaf` and `InnerLeftNode`.
       */
      sealed trait LeftInnerNonEmpty extends LeftNonEmpty with InnerNonEmpty with LeftChild {
         def parentLeft_=( p: LeftNode ) : Unit
      }

      /**
       * A common trait used in pattern matching, comprised of `Leaf` and `InnerRightNode`.
       */
      sealed trait RightInnerNonEmpty extends InnerNonEmpty with RightChild {
         def parentRight_=( p: RightNode ) : Unit
      }

      /**
       * A leaf in the quadtree, carrying a map entry
       * in the form of a point and associated value.
       * Note that a single instance of a leaf is used
       * across the levels of the quadtree! That means
       * that multiple child pointers may go to the
       * same leaf, while the parent of a leaf always
       * points into the highest level quadtree that
       * the leaf resides in, according to the skiplist.
       */
      sealed trait Leaf extends LeftInnerNonEmpty with RightInnerNonEmpty with Ordered[ Leaf ] with QLeaf {
         def value : A
         def value_=( v: A ) : Unit  // XXX hmmm, not so nice

         final def parentLeft_=( p: LeftNode )   { parent_=( p )}
         final def parentRight_=( p: RightNode ) { parent_=( p )}
         def parent_=( p: Node ) : Unit
//         def parent_=( p: Node ) : Unit

         /**
          * The position of this leaf in the in-order list.
          */
         def order : InOrder // TotalOrder

         /**
          * Leafs are ordered by the tree's in-order traversal,
          * where the quadrants I+II and III+IV can be thought
          * of as dummy nodes to binarize the quadtree. That is
          * to say, in a node, the child order corresponds to
          * their quadrant indices (I < II < III < IV).
          */
         final def compare( that: Leaf ) : Int = order.compare( that.order )

         final def union( mq: D#QuadType, point2: D#PointType ) = {
            val point   = pointView( value )
//            mq.greatestInteresting( point.x, point.y, 1, point2 )
sys.error( "TODO" )
         }

         final def quadIdxIn( iq: D#QuadType ) : Int = iq.indexOf( pointView( value ))

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
       * Nodes are defined by a quad area as well as a list of children,
       * as well as a pointer `next` to the corresponding node in the
       * next highest tree. A `Node` also provides various search methods.
       */
      sealed trait Node extends NonEmpty with QNode {
         /**
          * Returns the child for a given
          * quadrant index
          */
         def child( idx: Int ) : Child

         /**
          * Finds to smallest interesting square
          * in Q0, containing a given point. This method
          * traverses downwards into its children, or,
          * if the "bottom" has been reached, tries to
          * continue in Qi-1.
          *
          * @return  the node defined by the given search `point`, or `null`
          *          if no such node exists.
          */
         def findP0( point: D#PointType ) : LeftNode

         /**
          * Assuming that the given `leaf` is a child of this node,
          * removes the child from this node's children. This method
          * will perform further clean-up such as merging this node
          * with its parent if it becomes uninteresting as part of the
          * removal.
          */
         def removeImmediateLeaf( leaf: Leaf ) : Unit

//         def findLeaf( point: D#PointType ) : Leaf

         /**
          * Returns the square covered by this node
          */
         def quad: D#QuadType

         /**
          * Returns the corresponding interesting
          * square in Qi+1, or `null` if no such
          * square exists.
          */
         def next: RightNode

         /**
          * Sets the corresponding interesting
          * square in Qi+1.
          */
         def next_=( n: RightNode ) : Unit

         final def union( mq: D#QuadType, point2: D#PointType ) = {
            val q = quad
//            mq.greatestInteresting( q.left, q.top, q.side, point2 )
sys.error( "TODO" )
         }

         final def quadIdxIn( iq: D#QuadType ) : Int = iq.indexOf( quad )

         /**
          * The reverse process of `findP0`: Finds the lowest
          * common ancestor interesting square of this node
          * which is also contained in Qi+1. Returns this node
          * in Qi+1, or null if no such node exists.
          */
         def findPN( path: Array[ Node ], pathSize: Int ) : RightNode

         /**
          * Called when a leaf has been removed from the node.
          * The node may need to cleanup after this, e.g. promote
          * an underfull node upwards.
          */
         protected def leafRemoved() : Unit

         def nodeName : String
         final def shortString = nodeName + "(" + quad + ")"

         override def toString = shortString + " : children = [" +
            child(0).shortString + ", " + child(1).shortString + ", " + child(2).shortString + ", " +
            child(3).shortString + "]"

         final def prevOption: Option[ QNode ] = Option( prev )
         final def nextOption: Option[ QNode ] = Option( next )
      }

      /**
       * An inner non empty tree element has a mutable parent node.
       */
      sealed trait InnerNonEmpty extends NonEmpty {
         def parent: Node
//         def parent_=( p: Node ) : Unit
      }

      /**
       * Utility trait which elements the rightward search `findPN`.
       */
      sealed trait InnerNode extends Node with InnerNonEmpty {
         final def findPN( path: Array[ Node ], pathSize: Int ) : RightNode = {
            val n = next
            if( n != null ) n else {
               path( pathSize ) = this
               parent.findPN( path, pathSize + 1 )
            }
         }
      }

      /**
       * A right tree node implementation provides more specialized child nodes
       * of type `RightChild`. It furthermore defines the node in Qi-1 via the
       * `prev` method.
       */
      sealed trait RightNode extends Node with NonEmpty {
         final val children = Array.fill[ RightChild ]( numChildren )( Empty ) // XXX is apply faster?
         final var next : RightNode = null

//         def parent : RightNode
//         def parent_=( p: RightNode ) : Unit

         // Child support
//         final def prevOption = Some( prev: QNode )

         def prev : Node
         final def child( idx: Int ) : RightChild = children( idx )

         final def findP0( point: D#PointType ) : LeftNode = {
            val qidx = quad.indexOf( point )
            children( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => prev.findP0( point )
            }
         }

//         final def findLeaf( point: D#PointType ) : Leaf = {
//            val qidx = quad.indexOf( point )
//            children( qidx ) match {
//               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
//               case l: Leaf if( pointView( l.value ) == point ) => l
//               case _ => prev.findLeaf( point )
//            }
//         }

         /**
          * Promotes a leaf that exists in Qi-1 to this
          * tree, by inserting it into this node which
          * is its interesting node in Qi (XXX are we
          * sure there cannot be any intermediate
          * descendants from here?).
          *
          * If the result of insertion is a new child node
          * below this node, this intermediate node will
          * be connected to Qi by looking for the corresponding
          * quad in the given search path that led here
          * (i.e. that was constructed in `findPN`).
          *
          * This method also sets the parent of the leaf
          * accordingly.
          */
         final def insert( leaf: Leaf, path: Array[ Node ]) {
            val point   = pointView( leaf.value )
            val qidx    = quad.indexOf( point )
            val c       = children
            c( qidx ) match {
               case Empty =>
                  leaf.parent = this
                  c( qidx )   = leaf

               case old: RightInnerNonEmpty =>
                  val qn2     = old.union( quad.quadrant( qidx ), point )
                  // find the corresponding node in the lower tree
                  var pathIdx = 0; while( path( pathIdx ).quad != qn2 ) pathIdx += 1
                  val n2      = newNode( path( pathIdx ), qn2 )
                  val c2      = n2.children
                  val oidx    = old.quadIdxIn( qn2 )
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
                  val lidx    = leaf.quadIdxIn( qn2 )
                  c2( lidx )  = leaf
                  leaf.parent = n2
                  c( qidx )   = n2
                  n2
            }
         }

         /*
          * Instantiates an appropriate
          * sub-node whose parent is this node, and whose predecessor
          * in the lower quadtree is given.
          */
         @inline private def newNode( prev: Node, iq: D#QuadType ) : InnerRightNode = new InnerRightNode( this, prev, iq )

         final def removeImmediateLeaf( leaf: Leaf ) {
            var qidx = 0; while( qidx < numChildren ) {
               if( children( qidx ) == leaf ) {
                  children( qidx ) = Empty
                  var newParent  = prev
                  var pidx       = qidx
                  while( true ) {
                     newParent.child( pidx ) match {
                        case sn: Node =>
                           newParent   = sn
                           pidx        = leaf.quadIdxIn( sn.quad )
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
      sealed trait LeftNode extends Node with LeftNonEmpty {
         /**
          * For a `LeftNode`, all its children are more specific
          * -- they are instances of `LeftChild` and thus support
          * order intervals.
          */
         final val children = Array.fill[ LeftChild ]( numChildren )( Empty ) // XXX is apply faster?
         final var next : RightNode = null

         // Child support
//         final def prevOption = Option.empty[ QNode ]

         /**
          * Note that `prev` will not be called as part of this quadtree implementation
          * which smartly distinguishes between left and right nodes. It is merely here
          * to satisfy the `QNode` interface of `SkipOctree`.
          */
         final def prev : QNode = null

         final def child( idx: Int ) : Child = children( idx )

//         /**
//          * Creates a new leaf based on a given leaf,
//          * but with a new value. The caller is responsible for
//          * replacing the leaf in the children array, however
//          * the method must ensure the old leaf is removed from
//          * the order and the new one is inserted accordingly.
//          */
//         def newValue( leaf: Leaf, value: A ) : Leaf

         final def findP0( point: D#PointType ) : LeftNode = {
            val qidx = quad.indexOf( point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
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
         final def findImmediateLeaf( point: D#PointType ) : Leaf = {
            val qidx = quad.indexOf( point )
            child( qidx ) match {
               case l: Leaf if( pointView( l.value ) == point ) => l
               case _ => null
            }
         }

//         final def findLeaf( point: D#PointType ) : Leaf = {
//            val qidx = quad.indexOf( point )
//            children( qidx ) match {
//               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
//               case l: Leaf if( pointView( l.value ) == point ) => l
//               case _ => null
//            }
//         }

         final def removeImmediateLeaf( leaf: Leaf ) {
            var qidx = 0; while( qidx < numChildren ) {
               if( children( qidx ) == leaf ) {
                  children( qidx ) = Empty
                  leafRemoved()
                  leaf.dispose()
//                  leaf.parent = null
                  return
               }
            qidx += 1 }
         }

//         final def removeImmediateLeaf( point: D#PointType ) : Leaf = {
//            var qidx = 0; while( qidx < numChildren ) {
//               children( qidx ) match {
//                  case l: Leaf if( pointView( l.value ) == point ) =>
//                     // Important: We remove l from the total order first,
//                     // because that way we get orderly demotions
//                     // of leaves, and then when we remove l
//                     // in the quadtree, we are certain that this
//                     // happens in Q0 (i.e. l.parent == this!)
//                     skipList.remove( l )
//                     children( qidx ) = Empty
//                     leafRemoved()
//                     return l
//
//                  case _ =>
//               }
//            qidx += 1 }
//            null
//         }

//         final def removeImmediateLeaf( point: D#PointType ) : Leaf = {
//            var qidx = 0; while( qidx < numChildren ) {
//               children( qidx ) match {
//                  case l: Leaf if( pointView( l.value ) == point ) =>
//                     children( qidx ) = Empty
//                     var lonely: NonEmpty = null
//                     var numNonEmpty = 0
//                     var i = 0; while( i < numChildren ) {
//                        children( i ) match {
//                           case n: NonEmpty =>
//                              numNonEmpty += 1
//                              lonely = n
//                           case _ =>
//                        }
//                     i += 1 }
//                     if( numNonEmpty == 1 && parent != null ) {   // gotta remove this node and put remaining non empty element in parent
//                        // note: there is no prev by definition (we are in a left-node)!
////                        if( prev != null ) prev.next = null       // note: since remove is called from Qn to Q0, there is no this.next !
//                        val myIdx = parent.quad.indexOf( quad )
//                        parent.children( myIdx ) = lonely
//                        lonely match {
//                           case n: Node => n.parent = parent
//                           case _ =>
//                        }
//                     }
//                        // note: there is no prev by definition (we are in a left-node)!
////                     // meaning that this is a root node (but not headTree)
////                     else if( numNonEmpty == 0 && prev != null ) {
////                        prev.next   = null
////                        tailVar     = prev
////                     }
//                     return l
//
//                  case _ =>
//               }
//            qidx += 1 }
//            null
//         }

         final def insert( point: D#PointType, value: A ) : Leaf = {
            val qidx = quad.indexOf( point )
            val c    = children
            c( qidx ) match {
               case Empty =>
                  val leaf    = newLeaf( point, value )
                  c( qidx )   = leaf
                  leaf
//               case l: Leaf if( l.point == point ) => // replace value
//                  l.value     = value
//                  l
               case old: LeftInnerNonEmpty =>
                  val qn2     = old.union( quad.quadrant( qidx ), point )
//val tmp = quadInQuad( quad, qn2 )
//if( tmp == -1 ) {
//   println( "Ouch! " + qn2 + " not in " + quad )
//   old.union( quad.quadrant( qidx ), point )
//}
                  val n2      = newNode( qn2 )
                  val c2      = n2.children
                  val oidx    = old.quadIdxIn( qn2 )
                  c2( oidx )  = old
                  // This is a tricky bit! And a reason
                  // why should eventually try to do without
                  // parent pointers at all. Since `old`
                  // may be a leaf whose parent points
                  // to a higher level tree, we need to
                  // check first if the parent is `this`,
                  // and if so, adjust the parent to point
                  // to the new intermediate node `ne`!
                  if( old.parent == this ) old.parentLeft_=( n2 )
                  val leaf    = n2.newLeaf( point, value )
                  val lidx    = leaf.quadIdxIn( qn2 )
                  c2( lidx )  = leaf
                  c( qidx )   = n2
                  leaf
            }
         }

         /**
          * Instantiates an appropriate
          * leaf whose parent is this node, and which should be
          * ordered according to its position in this node.
          */
         private def newLeaf( point: D#PointType, value: A ) : Leaf = {
            val l = new LeafImpl( point, value, { l =>
               val lne: LeftNonEmpty = l
               ((lne.quadIdxIn( quad ): @switch) match {
                  case 0 => startOrder.append() // startOrder.insertAfter( lne )
                  case 1 => children( 0 ) match {
                     case n2: LeftNonEmpty => n2.stopOrder.append() // n2.stopOrder.insertAfter( l )
                     case _ => startOrder.append() // startOrder.insertAfter( lne )
                  }
                  case 2 => children( 3 ) match {
                     case n2: LeftNonEmpty => n2.startOrder.prepend() // n2.startOrder.insertBefore( l )
                     case _ => stopOrder.prepend() // stopOrder.insertBefore( lne )
                  }
                  case 3 => stopOrder.prepend() // stopOrder.insertBefore( lne )
               }) : InOrder // to satisfy idea's presentation compiler
            })
            l.parent = this
            l
         }

//         def newValue( old: Leaf, value: A ) : Leaf = LeafImpl( this, leaf.point, value ) { l =>
//            val ord = old.order
//            ord.elem = l
//            ord
//         }

         @tailrec private def insetStart( n: LeftNode, idx: Int ) : InOrder = {
            if( idx == -1 ) {
               startOrder.append() // startOrder.insertAfter( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.startOrder.prepend() // n2.startOrder.insertBefore( n )
               case _ => insetStart( n, idx  - 1 )
            }
         }
         @tailrec private def insetStop( n: LeftNode, idx: Int ) : InOrder = {
            if( idx == numChildren ) {
               stopOrder.prepend() // stopOrder.insertBefore( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.stopOrder.append() // n2.stopOrder.insertAfter( n )
               case _ => insetStop( n, idx + 1 )
            }
         }

         @inline private def insets( n: LeftNode, nidx: Int ) : (InOrder, InOrder) = {
            (insetStart( n, nidx ), insetStop( n, nidx ))
         }

         /*
          * Instantiates an appropriate
          * sub-node whose parent is this node, and which should be
          * ordered according to its position in this node.
          */
         @inline private def newNode( iq: D#QuadType ) : InnerLeftNode = new InnerLeftNode( this, iq, { n =>
            insets( n, quad.indexOf( n.quad ))  // n.quadIdxIn( quad )
         })
      }

      sealed trait TopNode extends Node {
//         final def parent : Node                = null
         final def quad : D#QuadType = tree.quad
//         final def parent_=( n: Node ) : Unit   = unsupportedOp

         final def findPN( path: Array[ Node ], pathSize: Int ) : RightNode = {
            val n = next
            if( n != null ) n else {
               path( pathSize ) = this
               null
            }
         }
      }

      object TopLeftNode extends LeftNode with TopNode {
         val startOrder                   = totalOrder.root // TotalOrder() // TotalOrder[ LeftNonEmpty ]( this )
         val stopOrder                    = startOrder.append() // startOrder.insertAfter( this )

         // that's alright, we don't need to do anything special here
         protected def leafRemoved() {}

         def nodeName = "top-left"
      }

      final class InnerLeftNode( var parent: LeftNode, val quad: D#QuadType, _ins: LeftNode => (InOrder, InOrder) )
      extends LeftNode with InnerNode with LeftInnerNonEmpty {
         val (startOrder, stopOrder) = _ins( this )

         def nodeName = "inner-left"

         def parentLeft_=( p: LeftNode ) { parent = p }

         // might become important with persistent implementation
         def dispose() {
            assert( next == null )
            startOrder.remove()
            stopOrder.remove()
         }

         // make sure the node is not becoming uninteresting, in which case
         // we need to merge upwards
         protected def leafRemoved() {
            var lonely: LeftInnerNonEmpty = null
            var numNonEmpty = 0
            var i = 0; while( i < numChildren ) {
               children( i ) match {
                  case n: LeftInnerNonEmpty =>
                     numNonEmpty += 1
                     lonely = n
                  case _ =>
               }
            i += 1 }
            if( numNonEmpty == 1 ) {   // gotta remove this node and put remaining non empty element in parent
               val myIdx = parent.quad.indexOf( quad )
//
//               @tailrec def findLeftParent( n: Node ) : LeftNode = n match {
//                  case ln: LeftNode    => ln
//                  case rn: RightNode   =>
//println( "Woopa : findLeftParent needs to iterate for " + rn )
//                     findLeftParent( rn.prev )
//               }
//               val p = findLeftParent( parent )

               parent.children( myIdx ) = lonely
               if( lonely.parent == this ) lonely.parentLeft_=( parent )
//               lonely match {
//                  // be aware that n.parent may point to other skiplist level!
//                  case n: Node if( n.parent == this ) => n.parent = p
//                  case _ =>
//               }
               dispose()
            }
         }
      }

      /**
       * Note that this instantiation sets the `prev`'s `next` field to this new node.
       */
      final class TopRightNode( val prev: TopNode ) extends RightNode with TopNode {
         prev.next = this

         def nodeName = "top-right"

         // remove this node if it empty now and right-node tree
         protected def leafRemoved() {
            if( next != null ) return

            var i = 0; while( i < numChildren ) {
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
      final class InnerRightNode( var parent: RightNode, val prev: Node, val quad: D#QuadType )
      extends RightNode with InnerNode with RightInnerNonEmpty {
         prev.next = this
//assert( prev.quad == quad )

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
            var i = 0; while( i < numChildren ) {
               children( i ) match {
                  case n: RightInnerNonEmpty =>
                     numNonEmpty += 1
                     lonely = n
                  case _ =>
               }
            i += 1 }
            if( numNonEmpty == 1 ) {   // gotta remove this node and put remaining non empty element in parent
               val myIdx = parent.quad.indexOf( quad )
               parent.children( myIdx ) = lonely
               if( lonely.parent == this ) lonely.parentRight_=( parent )
               dispose()
            }
         }
      }

      object MaxLeaf extends Leaf {
         val point                        = Point2D( Int.MaxValue, Int.MaxValue )  // YYY
         val order                        = totalOrder.max // TotalOrder.max // TotalOrder.max[ LeftNonEmpty ]

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
      final class LeafImpl( val point: D#PointType, var value: A, _ins: Leaf => InOrder )
      extends Leaf {
         private var parentVar: Node = null
         def parent: Node = parentVar
         def parent_=( p: Node ) { parentVar = p }
         val order = _ins( this )

         def shortString = "leaf(" + point + ")"
         override def toString = "Leaf(" + point + ", " + value + ")"

         def dispose() {
            parentVar   = null
//            value       = null.asInstanceOf[ A ]
            order.remove()
         }
      }
   }

//   private def notYetImplemented : Nothing   = sys.error( "Not yet implemented" )
}
//trait DeterministicSkipOctree[ V ] extends SkipOctree[ V ]