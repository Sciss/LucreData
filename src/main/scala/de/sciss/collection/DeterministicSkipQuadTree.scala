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

import sys.error
import annotation.{switch, tailrec}

/**
 * XXX TODO:
 * - TotalOrder[ LeftNonEmpty ] can now be probably
 *   TotalOrder[ Unit ], which in turn could be an optimized
 *   TotalOrder that doesn't carry an `elem` field any more.
 * - delete is missing
 * - find nearest neighbour is missing
 * - detect insertion of existing points (this causes a corruption currently!)
 */
object DeterministicSkipQuadTree {
//   def apply[ V ]( quad: Quad ) : DeterministicSkipQuadTree[ V ] = new TreeImpl[ V ]( quad )

   def apply[ V ]( quad: Quad, skipGap: Int = 2 )( xs: (Point, V)* ) : DeterministicSkipQuadTree[ V ] = {
      val t = new TreeImpl[ V ]( quad, skipGap )
      xs.foreach( t.+=( _ ))
      t
   }

   private class TreeImpl[ V ]( _quad: Quad, _skipGap: Int ) extends DeterministicSkipQuadTree[ V ] {
      private var tl: TopNode = TopLeftNode
      val list: SkipList[ Leaf ] = {
         implicit def maxKey = MaxKey( MaxLeaf )
         if( _skipGap < 2 ) {
            require( _skipGap == 1, "Illegal skipGap value (" + _skipGap + ")" )
            LLSkipList.empty[ Leaf ]( KeyObserver ) // ( Ordering.ordered[ Leaf ], )
         } else {
//            val mf = implicitly[ Manifest[ Leaf ]]
            HASkipList.empty[ Leaf ]( _skipGap, KeyObserver ) // ( Ordering.ordered[ Leaf ], maxKey, mf )
         }
      } // 2-5 DSL
      def skipList = list

      val numChildren = 4

      def headTree : QNode = TopLeftNode
      def lastTree : QNode = tl

      // ---- map support ----

      def +=( kv: (Point, V) ) : this.type = {
         val point   = kv._1
         val value   = kv._2
         val p0      = tl.findP0( point )
         val leaf    = p0.insert( point, value )
         list.add( leaf )
         this
      }

      def get( point: Point ) : Option[ V ] = {
         val p0 = tl.findP0( point )
         var i = 0; while( i < numChildren ) {
            p0.child( i ) match {
               case l: Leaf if( l.point == point ) => return Some( l.value )
               case _ =>
            }
         i += 1 }
         None
      }

      def -=( point: Point ) : this.type = {
         error( "TODO" )
      }

      def iterator = new Iterator[ (Point, V) ] {
         val underlying = list.iterator
         def next : (Point, V) = {
            val leaf = underlying.next
            (leaf.point, leaf.value)
         }
         def hasNext : Boolean = underlying.hasNext
      }

      def rangeQuery( qs: QueryShape ) : Iterator[ V ] = notYetImplemented

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
               val res = TopRightNode( tl )
               tl = res
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
         def union( mq: Quad, point: Point ) : Quad

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

      type InOrder = TotalOrder[ LeftNonEmpty ]

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
         def point : Point
         def value : V
//         def value_=( v: V ) : Unit  // XXX hmmm, not so nice

         /**
          * The position of this leaf in the in-order list.
          */
         def order : InOrder

         /**
          * Leafs are ordered by the tree's in-order traversal,
          * where the quadrants I+II and III+IV can be thought
          * of as dummy nodes to binarize the quadtree. That is
          * to say, in a node, the child order corresponds to
          * their quadrant indices (I < II < III < IV).
          */
         def compare( that: Leaf ) : Int = order.compare( that.order )

         def union( mq: Quad, point2: Point ) = {
            val p = point
            interestingSquare( mq, p.x, p.y, 1, point2 )
         }
         def quadIdxIn( iq: Quad ) : Int = pointInQuad( iq, point )

         /**
          * For a leaf (which does not have a subtree),
          * the `startOrder` is identical to its `order`.
          */
         def startOrder : InOrder   = order

         /**
          * For a leaf (which does not have a subtree),
          * the `stopOrder` is identical to its `order`.
          */
         def stopOrder : InOrder    = order
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
         def findP0( point: Point ) : LeftNode

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

         /**
          * Sets the corresponding interesting
          * square in Qi+1.
          */
         def next_=( n: RightNode ) : Unit

         def union( mq: Quad, point2: Point ) = {
            val q = quad
            interestingSquare( mq, q.left, q.top, q.side, point2 )
         }

         def quadIdxIn( iq: Quad ) : Int = quadInQuad( iq, quad )

         /**
          * The reverse process of `findP0`: Finds the lowest
          * common ancestor interesting square of this node
          * which is also contained in Qi+1. Returns this node
          * in Qi+1, or null if no such node exists.
          */
         def findPN( path: Array[ Node ], pathSize: Int ) : RightNode = {
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
         // Child support
         def prevOption = Some( prev: QNode )

         def prev : Node
         def children : Array[ Child ]
         def child( idx: Int ) : Child = children( idx )

         def findP0( point: Point ) : LeftNode = {
            val qidx = pointInQuad( quad, point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => prev.findP0( point )
            }
         }

         /**
          * Abstract method which should instantiate an appropriate
          * sub-node whose parent is this node, and whose predecessor
          * in the lower quadtree is given.
          */
         def newNode( prev: Node, iq: Quad ) : RightNode

         /**
          * Promotes a leaf that exists in Qi-1 to this
          * tree, by inserting it into this note which
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
         def insert( leaf: Leaf, path: Array[ Node ]) {
            val point         = leaf.point
            val qidx          = pointInQuad( quad, point )
            val c             = children
            c( qidx ) match {
               case Empty =>
//                  val leaf    = newLeaf( point, value )
                  leaf.parent = this
                  c( qidx )   = leaf
//println( "promoted " + point + " to " + leaf.parent.quad )

//               case l: Leaf if( l.point == leaf.point )

               case old: NonEmpty =>
                  val qn2     = old.union( quad.quadrant( qidx ), point )
                  // find the corresponding node in the lower tree
//val gaga = old match {
//   case l: Leaf => Quad( l.point.x, l.point.y, 1 )
//   case n: Node => n.quad
//}
//print( "promoting " + point + " to union(" + gaga + ", " + point + ") = " + qn2 + " (parent = " + quad + "; path = " +
//   path.toList.takeWhile(_ != null ).map(_.quad) + ") ... " )
                  var pathIdx = 0; while( path( pathIdx ).quad != qn2 ) pathIdx += 1
                  val n2      = newNode( path( pathIdx ), qn2 )
//println( "(pathIdx = " + pathIdx + " of " + {
//   var i = pathIdx + 1; while( i < 6 && path( i ) != null ) i += 1
//   i
//} + ")" )
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
//                  val leaf    = n2.newLeaf( point, value )
                  val lidx    = leaf.quadIdxIn( qn2 )
//assert( oidx != lidx )
                  c2( lidx )  = leaf
                  leaf.parent = n2
                  c( qidx )   = n2
                  n2
            }
         }
      }

      sealed trait LeftNode extends Node with LeftNonEmpty {
         // Child support
         def prevOption = Option.empty[ QNode ]

         /**
          * For a `LeftNode`, all its children are more specific
          * -- they are instances of `LeftChild` and thus support
          * order intervals.
          */
         def children : Array[ LeftChild ]
         def child( idx: Int ) : Child = children( idx )

         /**
          * Abstract method which should instantiate an appropriate
          * leaf whose parent is this node, and which should be
          * ordered according to its position in this node.
          */
         def newLeaf( point: Point, value: V ) : Leaf

//         /**
//          * Creates a new leaf based on a given leaf,
//          * but with a new value. The caller is responsible for
//          * replacing the leaf in the children array, however
//          * the method must ensure the old leaf is removed from
//          * the order and the new one is inserted accordingly.
//          */
//         def newValue( leaf: Leaf, value: V ) : Leaf

         /**
          * Abstract method which should instantiate an appropriate
          * sub-node whose parent is this node, and which should be
          * ordered according to its position in this node.
          */
         def newNode( iq: Quad ) : LeftNode

         def findP0( point: Point ) : LeftNode = {
            val qidx = pointInQuad( quad, point )
            child( qidx ) match {
               case n: Node if( n.quad.contains( point )) => n.findP0( point )
               case _ => this
            }
         }

         def insert( point: Point, value: V ) : Leaf = {
            val qidx = pointInQuad( quad, point )
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
      }

      sealed trait LeftNodeImpl extends LeftNode {
         val children = Array.fill[ LeftChild ]( 4 )( Empty ) // XXX is apply faster?
         var next : RightNode = null

         def newLeaf( point: Point, value: V ) : Leaf = LeafImpl( this, point, value ) { l =>
            val lne: LeftNonEmpty = l
            ((lne.quadIdxIn( quad ): @switch) match {
               case 0 => startOrder.insertAfter( lne )
               case 1 => children( 0 ) match {
                  case n2: LeftNonEmpty => n2.stopOrder.insertAfter( l )
                  case _ => startOrder.insertAfter( lne )
               }
               case 2 => children( 3 ) match {
                  case n2: LeftNonEmpty => n2.startOrder.insertBefore( l )
                  case _ => stopOrder.insertBefore( lne )
               }
               case 3 => stopOrder.insertBefore( lne )
            }) : InOrder // to satisfy idea's presentation compiler
         }

//         def newValue( old: Leaf, value: V ) : Leaf = LeafImpl( this, leaf.point, value ) { l =>
//            val ord = old.order
//            ord.elem = l
//            ord
//         }

         @tailrec final def insetStart( n: LeftNonEmpty, idx: Int ) : InOrder = {
            if( idx == -1 ) {
               startOrder.insertAfter( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.startOrder.insertBefore( n )
               case _ => insetStart( n, idx  - 1 )
            }
         }
         @tailrec final def insetStop( n: LeftNonEmpty, idx: Int ) : InOrder = {
            if( idx == 4 ) {
               stopOrder.insertBefore( n )
            } else children( idx ) match {
               case n2: LeftNonEmpty => n2.stopOrder.insertAfter( n )
               case _ => insetStop( n, idx + 1 )
            }
         }

         def insets( n: LeftNode, nidx: Int ) : (InOrder, InOrder) = {
            (insetStart( n, nidx ), insetStop( n, nidx ))
         }

         def newNode( iq: Quad ) : LeftNode = InnerLeftNode( this, iq ) { n =>
            insets( n, n.quadIdxIn( quad ))
         }
      }

      sealed trait RightNodeImpl extends RightNode {
         val children = Array.fill[ Child ]( 4 )( Empty ) // XXX is apply faster?
         var next : RightNode = null

         def newNode( prev: Node, iq: Quad ) : RightNode = InnerRightNode( this, prev, iq )
      }

      sealed trait TopNode extends Node {
         val parent : Node                = null
         def quad : Quad                  = _quad
         def parent_=( n: Node ) : Unit   = unsupportedOp
      }

      object TopLeftNode extends LeftNodeImpl with TopNode {
         val startOrder                   = TotalOrder[ LeftNonEmpty ]( this )
         val stopOrder                    = startOrder.insertAfter( this )
      }

      final case class InnerLeftNode( var parent: Node, quad: Quad )( _ins: LeftNode => (InOrder, InOrder) ) extends LeftNodeImpl {
         val (startOrder, stopOrder) = _ins( this )
      }

      /**
       * Note that this instantiation sets the `prev`'s `next` field to this new node.
       */
      final case class TopRightNode( prev: Node ) extends RightNodeImpl with TopNode {
         prev.next = this
//assert( prev.quad == quad )
      }

      /**
       * Note that this instantiation sets the `prev`'s `next` field to this new node.
       */
      final case class InnerRightNode( var parent: Node, prev: Node, quad: Quad ) extends RightNodeImpl {
         prev.next = this
//assert( prev.quad == quad )
      }

      object MaxLeaf extends Leaf {
         val point                        = Point( Int.MaxValue, Int.MaxValue )
         val order                        = TotalOrder.max[ LeftNonEmpty ]

         def value : V                    = unsupportedOp
//         def value_=( v: V ) : Unit       = unsupportedOp
         def parent : Node                = unsupportedOp
         def parent_=( n: Node ) : Unit   = unsupportedOp
      }

      // it would be better to replace the leaf instead of updating value; however
      // the problem is there can be several pointers to a leaf, so at least for now,
      // let's not make life more complicated than necessary. also skip list would
      // need to be made 'replace-aware'.
      final case class LeafImpl( var parent: Node, point: Point, /* var */ value: V )( _ins: Leaf => InOrder ) extends Leaf {
         val order = _ins( this )
      }
   }

  /**
    * Determines the quadrant index of a point `a` in a square `p` defined
    * by its center `pc` and extent `pe`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `a` lies
    *          outside of `p`.
    */
   private def pointInQuad( pq: Quad, a: Point ) : Int = {
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

   private def quadInQuad( pq: Quad, aq: Quad ) : Int = {
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

   private def unsupportedOp : Nothing       = error( "Operation not supported" )
   private def notYetImplemented : Nothing   = error( "Not yet implemented" )

   // http://stackoverflow.com/questions/6156502/integer-in-an-interval-with-maximized-number-of-trailing-zero-bits
   @tailrec private def binSplit( a: Int, b: Int, mask: Int = 0xFFFF0000, shift: Int = 8 ): Int = {
      val gt = a > (b & mask)
      if( shift == 0 ) {
         if( gt ) mask >> 1 else mask
      } else {
        binSplit( a, b, if( gt ) mask >> shift else mask << shift, shift >> 1 )
      }
   }

//   private def interestingSquare( pq: Quad, aleft: Int, atop: Int, asize: Int,  b: Point ) : Quad = {
//      val tlx           = pq.cx - pq.extent
//      val tly           = pq.cy - pq.extent
//      val akx           = aleft - tlx
//      val aky           = atop  - tly
//      val bkx           = b.x - tlx
//      val bky           = b.y - tly
//      val (x0, x1, x2)  = if( akx <= bkx ) (akx, akx + asize, bkx) else (bkx, bkx + 1, akx ) // XXX Tuple3 not specialized
//      val (y0, y1, y2)  = if( aky <= bky ) (aky, aky + asize, bky) else (bky, bky + 1, aky ) // XXX Tuple3 not specialized
//      val mx            = binSplit( x1, x2 )
//      val my            = binSplit( y1, y2 )
//      // that means the x extent is greater (x grid more coarse).
//      if( mx <= my ) {
//         Quad( tlx + (x2 & mx), tly + (y0 & (mx << 1)) - mx, -mx )   // XXX check for Int.MaxValue issues
//      } else {
//         Quad( tlx + (x0 & (my << 1)) - my, tly + (y2 & my), -my )   // XXX check for Int.MaxValue issues
//      }
//   }

   private def interestingSquare( pq: Quad, aleft: Int, atop: Int, asize: Int,  b: Point ) : Quad = {
      val tlx  = pq.left
      val tly  = pq.top
      val akx  = aleft - tlx
      val aky  = atop  - tly
      val bkx  = b.x - tlx
      val bky  = b.y - tly
      var x0, x1, x2, y0, y1, y2 = 0

      if( akx <= bkx ) {
         x0 = akx
         x1 = akx + asize
         x2 = bkx
      } else {
         x0 = bkx
         x1 = bkx + 1
         x2 = akx
      }
      if( aky <= bky ) {
         y0 = aky
         y1 = aky + asize
         y2 = bky
      } else {
         y0 = bky
         y1 = bky + 1
         y2 = aky
      }
      val mask = math.min( binSplit( x1, x2 ), binSplit( y1, y2 ))
      val ext  = -mask
      val m2   = mask << 1
      Quad( tlx + (x0 & m2) + ext, tly + (y0 & m2) + ext, ext )
   }

   // visible interface
}
trait DeterministicSkipQuadTree[ V ] extends SkipQuadTree[ V ]