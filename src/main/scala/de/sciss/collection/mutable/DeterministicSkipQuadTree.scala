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

package de.sciss.collection
package mutable

import annotation.{switch, tailrec}
import geom.{Point, DistanceMeasure, PointLike, Quad, QueryShape}

/**
 * XXX TODO:
 * - delete is missing
 * - find nearest neighbour is missing
 * - detect insertion of existing points (this causes a corruption currently!)
 */
object DeterministicSkipQuadTree {
//   def apply[ V ]( quad: Quad ) : DeterministicSkipQuadTree[ V ] = new TreeImpl[ V ]( quad )

   def empty[ A ]( quad: Quad, skipGap: Int = 2 )( implicit view: A => PointLike ) : SkipQuadTree[ A ] =
      new TreeImpl[ A ]( quad, skipGap, view )

   def apply[ A <% PointLike ]( quad: Quad, skipGap: Int = 2 )( xs: A* ) : SkipQuadTree[ A ] = {
      val t = empty[ A ]( quad, skipGap )
      xs.foreach( t.+=( _ ))
      t
   }

   private final class TreeImpl[ A ]( quad: Quad, _skipGap: Int, val pointView: A => PointLike )
   extends impl.SkipQuadTreeImpl[ A ] {
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

      val numChildren = 4

      def headTree : QNode = TopLeftNode
      def lastTree : QNode = tailVar

      type InOrder = totalOrder.Entry

      protected def findLeaf( point: PointLike ) : Leaf = tailVar.findLeaf( point )

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

      protected def removeLeaf( point: PointLike ) : Leaf = {
         notYetImplemented
      }

//      protected def findLeaf( point: PointLike ) : Leaf = {
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

//      def rangeQuery( qs: QueryShape ) : Iterator[ A ] = notYetImplemented

      protected def nn( point: PointLike, metric: DistanceMeasure ) : Leaf = {
         notYetImplemented
      }

//      def isomorphicQuery( orient: A => Int ) : RectangleLike = {
//         error( "TODO" )
//      }

      object KeyObserver extends SkipList.KeyObserver[ Leaf ] {
         def keyUp( l: Leaf ) {
            // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
            //  then traverse upwards in Qi until we find the lowest
            //  ancestor q of x which is also interesting in Qi+1.
            //  (This is the reversed process of searching x in Qi
            //  with q = pi,start = pi+1,end so it takes at most 6
            //  steps by Lemma 5.) Then we go to the same square q
            //  in Qi+1 and insert x."
            val path = new Array[ Node ]( 6 ) // hmmm... according to what i've seen, the maximum necessary size is 4 ?!
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
            println( "down : " + l )
            notYetImplemented
         }
      }

      /**
       * A child is an object that can be
       * stored in a quadrant of a node.
       */
      sealed trait Child extends Q
      /**
       * A left child is one which is stored in Q0.
       * This is either empty or an object (`LeftNonEmpty`)
       * which provides start and stop markers for its
       * position in the in-order list.
       */
      sealed trait LeftChild extends Child

      /**
       * A dummy object indicating a vacant quadrant in a node.
       */
      case object Empty extends LeftChild with QEmpty

      /**
       * An object denoting a filled quadrant of a node.
       * This is either a leaf or another node.
       */
      sealed trait NonEmpty extends Child {
         /**
          * Computes the greatest interesting square within
          * a given quadrant `mq` so that this (leaf's or node's)
          * square and the given point will be placed in
          * separated quadrants of this resulting square.
          */
         def union( mq: Quad, point: PointLike ) : Quad

         /**
          * Queries the quadrant index for this (leaf's or node's) square
          * with respect to a given outer square `iq`.
          */
         def quadIdxIn( iq: Quad ) : Int

         /**
          * Returns the parent node, or null if this is a tree's top.
          */
         def parent : Node

         /**
          * Sets the parent node.
          */
         def parent_=( n: Node ) : Unit
      }
      sealed trait LeftNonEmpty extends NonEmpty with LeftChild {
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

//      type TotalOrder = totalOrder.Entry // [ LeftNonEmpty ]

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
//      object Leaf {
//         implicit val mx = MaxKey( MaxLeaf )
//      }
      sealed trait Leaf extends LeftNonEmpty with Ordered[ Leaf ] with QLeaf {
//         def point : PointLike
         def value : A
         def value_=( v: A ) : Unit  // XXX hmmm, not so nice

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

         final def union( mq: Quad, point2: PointLike ) = {
            val point   = pointView( value )
            mq.greatestInteresting( point.x, point.y, 1, point2 )
         }

         final def quadIdxIn( iq: Quad ) : Int = iq.indexOf( pointView( value ))

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
      }

      sealed trait Node extends NonEmpty with QNode {
         /**
          * Returns the child for a given
          * quadrant index
          */
         def child( idx: Int ) : Child

         /**
          * Finds to smallest interesting square
          * in Q0, containing a given point.
          */
         def findP0( point: PointLike ) : LeftNode

         final def findImmediateLeaf( point: PointLike ) : Leaf = {
            var i = 0; while( i < numChildren ) {
               child( i ) match {
                  case l: Leaf if( pointView( l.value ) == point ) => return l
                  case _ =>
               }
            i += 1 }
            null
         }

         def findLeaf( point: PointLike ) : Leaf

         /**
          * Returns the square covered by this node
          */
         def quad: Quad

         /**
          * Returns the corresponding interesting
          * square in Qi+1, or null if no such
          * square exists.
          */
         def next: RightNode

         final def nextOption: Option[ QNode ] = Option( next )

         /**
          * Sets the corresponding interesting
          * square in Qi+1.
          */
         def next_=( n: RightNode ) : Unit

         final def union( mq: Quad, point2: PointLike ) = {
            val q = quad
            mq.greatestInteresting( q.left, q.top, q.side, point2 )
         }

         final def quadIdxIn( iq: Quad ) : Int = iq.indexOf( quad )

         /**
          * The reverse process of `findP0`: Finds the lowest
          * common ancestor interesting square of this node
          * which is also contained in Qi+1. Returns this node
          * in Qi+1, or null if no such node exists.
          */
         final def findPN( path: Array[ Node ], pathSize: Int ) : RightNode = {
            val n = next
            if( n != null ) n else {
               path( pathSize ) = this
               val p = parent
               if( p == null ) null else {
                  p.findPN( path, pathSize + 1 )
               }
            }
         }
      }

      sealed trait RightNode extends Node {
         final val children = Array.fill[ Child ]( 4 )( Empty ) // XXX is apply faster?
         final var next : RightNode = null

         // Child support
         final def prevOption = Some( prev: QNode )

         def prev : Node
         final def child( idx: Int ) : Child = children( idx )

         final def findP0( point: PointLike ) : LeftNode = {
            val qidx = quad.indexOf( point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => prev.findP0( point )
            }
         }

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

               case old: NonEmpty =>
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
                  if( old.parent eq this ) old.parent = n2
                  val lidx    = leaf.quadIdxIn( qn2 )
                  c2( lidx )  = leaf
                  leaf.parent = n2
                  c( qidx )   = n2
                  n2
            }
         }

         /**
          * Instantiates an appropriate
          * sub-node whose parent is this node, and whose predecessor
          * in the lower quadtree is given.
          */
         final def newNode( prev: Node, iq: Quad ) : RightNode = new InnerRightNode( this, prev, iq )

         final def findLeaf( point: PointLike ) : Leaf = {
            val qidx = quad.indexOf( point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
               case l: Leaf if( pointView( l.value ) == point ) => l
               case _ => prev.findLeaf( point )
            }
         }
      }

      sealed trait LeftNode extends Node with LeftNonEmpty {
         /**
          * For a `LeftNode`, all its children are more specific
          * -- they are instances of `LeftChild` and thus support
          * order intervals.
          */
         final val children = Array.fill[ LeftChild ]( 4 )( Empty ) // XXX is apply faster?
         final var next : RightNode = null

         // Child support
         final def prevOption = Option.empty[ QNode ]

         final def child( idx: Int ) : Child = children( idx )

//         /**
//          * Creates a new leaf based on a given leaf,
//          * but with a new value. The caller is responsible for
//          * replacing the leaf in the children array, however
//          * the method must ensure the old leaf is removed from
//          * the order and the new one is inserted accordingly.
//          */
//         def newValue( leaf: Leaf, value: A ) : Leaf

         final def findP0( point: PointLike ) : LeftNode = {
            val qidx = quad.indexOf( point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => this
            }
         }

         final def insert( point: PointLike, value: A ) : Leaf = {
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
               case old: LeftNonEmpty =>
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
                  if( old.parent eq this ) old.parent = n2
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
         final def newLeaf( point: PointLike, value: A ) : Leaf = new LeafImpl( this, point, value, { l =>
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

//         def newValue( old: Leaf, value: A ) : Leaf = LeafImpl( this, leaf.point, value ) { l =>
//            val ord = old.order
//            ord.elem = l
//            ord
//         }

         final def findLeaf( point: PointLike ) : Leaf = {
            val qidx = quad.indexOf( point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findLeaf( point )
               case l: Leaf if( pointView( l.value ) == point ) => l
               case _ => null
            }
         }

         @tailrec final def insetStart( n: LeftNonEmpty, idx: Int ) : InOrder = {
            if( idx == -1 ) {
               startOrder.append() // startOrder.insertAfter( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.startOrder.prepend() // n2.startOrder.insertBefore( n )
               case _ => insetStart( n, idx  - 1 )
            }
         }
         @tailrec final def insetStop( n: LeftNonEmpty, idx: Int ) : InOrder = {
            if( idx == 4 ) {
               stopOrder.prepend() // stopOrder.insertBefore( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.stopOrder.append() // n2.stopOrder.insertAfter( n )
               case _ => insetStop( n, idx + 1 )
            }
         }

         final def insets( n: LeftNode, nidx: Int ) : (InOrder, InOrder) = {
            (insetStart( n, nidx ), insetStop( n, nidx ))
         }

         /**
          * Instantiates an appropriate
          * sub-node whose parent is this node, and which should be
          * ordered according to its position in this node.
          */
         final def newNode( iq: Quad ) : LeftNode = new InnerLeftNode( this, iq, { n =>
            insets( n, quad.indexOf( n.quad ))  // n.quadIdxIn( quad )
         })
      }

      sealed trait TopNode extends Node {
         final def parent : Node                = null
         final def quad : Quad                  = tree.quad
         final def parent_=( n: Node ) : Unit   = unsupportedOp
      }

      object TopLeftNode extends LeftNode with TopNode {
         val startOrder                   = totalOrder.root // TotalOrder() // TotalOrder[ LeftNonEmpty ]( this )
         val stopOrder                    = startOrder.append() // startOrder.insertAfter( this )
      }

      final class InnerLeftNode( var parent: Node, val quad: Quad, _ins: LeftNode => (InOrder, InOrder) )
      extends LeftNode {
         val (startOrder, stopOrder) = _ins( this )
      }

      /**
       * Note that this instantiation sets the `prev`'s `next` field to this new node.
       */
      final class TopRightNode( val prev: Node ) extends RightNode with TopNode {
         prev.next = this
//assert( prev.quad == quad )
      }

      /**
       * Note that this instantiation sets the `prev`'s `next` field to this new node.
       */
      final class InnerRightNode( var parent: Node, val prev: Node, val quad: Quad ) extends RightNode {
         prev.next = this
//assert( prev.quad == quad )
      }

      object MaxLeaf extends Leaf {
         val point                        = Point( Int.MaxValue, Int.MaxValue )
         val order                        = totalOrder.max // TotalOrder.max // TotalOrder.max[ LeftNonEmpty ]

         def value : A                    = unsupportedOp
         def value_=( v: A ) : Unit       = unsupportedOp
         def parent : Node                = unsupportedOp
         def parent_=( n: Node ) : Unit   = unsupportedOp
      }

      // it would be better to replace the leaf instead of updating value; however
      // the problem is there can be several pointers to a leaf, so at least for now,
      // let's not make life more complicated than necessary. also skip list would
      // need to be made 'replace-aware'.
      final class LeafImpl( var parent: Node, val point: PointLike, var value: A, _ins: Leaf => InOrder )
      extends Leaf {
         val order = _ins( this )
      }
   }

   private def unsupportedOp : Nothing       = sys.error( "Operation not supported" )
   private def notYetImplemented : Nothing   = sys.error( "Not yet implemented" )
}
//trait DeterministicSkipQuadTree[ V ] extends SkipQuadTree[ V ]