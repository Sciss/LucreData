/*
 *  SkipList.scala
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

package de.sciss.tree

import sys.error
import annotation.tailrec

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 *
 * XXX todo: verify the structure doesn't get damaged when we try to insert an existing key
 *
 * XXX todo: there is currently a bug with minGap = 1, resulting in keys occasionally being
 * stored twice in successive bottom bins.
 */
object HASkipList {
//   def empty[ @specialized( Int, Long ) B : Manifest, A ]( minGap: Int = 1, key: A => B, maxKey: B ) : HASkipList[ A ] =
//      new Impl[ B, A ]( minGap, key maxKey )

   def empty[ @specialized( Int, Long ) A : Ordering : Manifest ]
      ( maxKey: A, minGap: Int = 2, keyObserver: SkipList.KeyObserver[ A ] = new SkipList.NoKeyObserver[ A ]) : HASkipList[ A ] = {

      require( minGap >= 1 )
      if( minGap == 1 ) println( "WARNING: HASkipList implementation currently broken for minGap = 1" )
      new Impl( maxKey, minGap, keyObserver )
   }

   sealed trait Node[ @specialized( Int, Long ) A ] {
      def size : Int
      def key( i: Int ) : A // Int
      def down( i: Int ) : Node[ A ]
      def isBottom : Boolean // = this eq Bottom
   }

   private class Impl[ @specialized( Int, Long ) A ]( maxKey: A, val minGap: Int, keyObserver: SkipList.KeyObserver[ A ])
                                                    ( implicit mf: Manifest[ A ], ord: Ordering[ A ])
   extends HASkipList[ A ] {
      val maxGap  = (minGap << 1) + 1
      val arrSize = maxGap + 1
      var arrMid  = maxGap >> 1

      override def size : Int = leafSizeSum( Head ) - 1

      private def leafSizeSum( n: Node[ _ ]) : Int = {
         var res = 0
         var i = 0; while( i < n.size ) {
            val dn = n.down( i )
            if( dn.isBottom ) return n.size
            res += leafSizeSum( dn )
            i += 1
         }
         res
      }

      def height : Int = {
         val x = new Nav
         var i = -1; while( !x.isBottom ) { x.moveDown; i += 1 }
         i
      }

      def top : Node[ A ] = Head.downNode

      // a kind of finger to navigate through
      // the data structure
      final class Nav {
         var node: NodeImpl = Head
         var idx: Int = 0
         def isBottom : Boolean = node.isBottom
//         def isHead : Boolean = node eq head
         def key : A = node.key( idx )
         def moveRight( key: A ) : Boolean = {
            while( ord.gt( key, node.key( idx ))) idx += 1
            ord.equiv( key, node.key( idx ))
         }
         def moveDown {
            node  = node.down( idx )
            idx   = 0
         }

         def copy( target: Nav ) {
            target.node = node
            target.idx  = idx
         }

         def isFull : Boolean = node.size == arrSize

//         /**
//          * Splits the current node, and
//          * returns the right hand side
//          * as a new node. This nav will
//          * remain positioned at the
//          * old node which is shrunk to
//          * the left hand side. We assume
//          * that the index is 0, so make
//          * sure this condition is met!
//          */
//         def split : Branch = node.split

         def splitChild( key: A, ch: Nav ) {
            val left       = ch.node
            val right      = left.split
            val splitKey   = left.key( arrMid )
            if( node eq Head ) {
               val n             = new Branch
               n.keyArr( 0 )     = splitKey
               n.keyArr( 1 )     = maxKey // MAX_KEY // aka right.key( right.size - 1 )
               n.downArr( 0 )    = left
               n.downArr( 1 )    = right
               n.size            = 2
               Head.downNode     = n
//println( "splitting below head; new node has size " + n.size + " ; ch.node.size (left) " + ch.node.size + " ; ch.idx " + ch.idx )
               node              = n
            } else {
               val n             = node.asNode
               val i1            = idx + 1
//println( "splitting into parent; new node has old size " + n.size )
               System.arraycopy( n.keyArr, idx, n.keyArr, i1, n.size - idx )
               val num           = n.size - i1
               if( num > 0 ) System.arraycopy( n.downArr, i1, n.downArr, i1 + 1, num )
               n.keyArr( idx )   = splitKey
               // this is already the case:
//               n.downArr( idx )  = left
               n.downArr( i1 )   = right
               n.size           += 1
            }
            // important: if the current key of the parent
            // is greater or equal than the splitKey,
            // we must update the child navigation accordingly,
            // beause it means we are now traversing the right
            // half!
            if( ord.gteq( key, splitKey )) ch.node = right

            // notify observer
            keyObserver.keyUp( splitKey )
         }

         /**
          * Inserts the key and value at
          * the current index into the
          * current node which is assumed
          * to be a leaf
          */
         def insert( key: A ) {
            if( node eq Head ) {
               val n             = new Leaf
               n.keyArr( 0 )     = key
               n.keyArr( 1 )     = maxKey // MAX_KEY // aka right.key( right.size - 1 )
//               n.valArr( 0 )     = v
               n.size            = 2
               Head.downNode     = n
//println( "inserting new leaf below head of new size " + n.size )
//               node              = n
            } else {
               val n             = node.asLeaf
//               val i1            = idx + 1
//               val sz            = n.size - idx
//println( "inserting in node of old size " + n.size + " where idx = " + idx )
               System.arraycopy( n.keyArr, idx, n.keyArr, idx + 1, n.size - idx )
//               System.arraycopy( n.valArr, idx, n.valArr, i1, sz )
               n.keyArr( idx )   = key
//               n.valArr( idx )   = v
               n.size           += 1
            }
         }
      }

      // ---- set support ----

      def contains( key: A ) : Boolean = {
//         val key  = keyFun( v )
         val x    = new Nav
         while( !x.isBottom ) {
            if( x.moveRight( key )) return true
            x.moveDown
         }
         false
      }

      override def add( key: A ) : Boolean = {
//         val key  = keyFun( v )
         val x    = new Nav
         val x0   = new Nav
         do {
            if( x.moveRight( key )) return false // key was already present
            x.copy( x0 )
            x.moveDown
            if( x.isFull ) x0.splitChild( key, x )
         } while( !x.isBottom )
         x0.insert( key )
         true
      }

      def +=( elem: A ) : this.type = { add( elem ); this }
      def -=( elem: A ) : this.type = error( "Not yet implemented" )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var x: Node[ A ]  = _
         var idx: Int      = _
         val stack         = collection.mutable.Stack.empty[ (Int, Node[ A ])]
         pushDown( 0, Head )

         def pushDown( idx0: Int, n: Node[ A ] ) {
            var pred = n
            var pidx = idx0
            var dn   = pred.down( pidx )
            while( !dn.isBottom ) {
               stack.push( (pidx + 1, pred) )
               pred  = dn
               pidx  = 0
               dn    = pred.down( pidx )
            }
            x     = pred
            idx   = pidx
         }

         def hasNext : Boolean = !ord.equiv( x.key( idx ), maxKey )
         def next : A = {
            val res = x.key( idx )
            idx += 1
            if( idx == x.size ) {
               @tailrec def findPush {
                  if( stack.nonEmpty ) {
                     val (i, n) = stack.pop
                     if( i < n.size ) pushDown( i, n ) else findPush
                  }
               }
               findPush
            }
            res
         }
      }

      sealed trait NodeImpl extends Node[ A ] {
         override def down( i: Int ) : NodeImpl

         /**
          * Splits the node, and
          * returns the right hand side
          * as a new node. This old node
          * is shrunk to the left hand side
          */
         def split : NodeImpl

         def asNode : Branch
         def asLeaf : Leaf
      }

      sealed trait BranchOrLeaf extends NodeImpl {
         var keyArr     = new Array[ A ]( arrSize )
         var size       = 0
         def key( i: Int ) : A = keyArr( i )
         def isBottom   = false
//         def isHead     = false
      }

      class Leaf extends BranchOrLeaf {
//         var valArr  = new Array[ A ]( arrSize )
         def down( i: Int ) : NodeImpl = Bottom
         def split : NodeImpl = {
            val res     = new Leaf
            val roff    = arrMid + 1
            val rsz     = size - roff
//println( "Splitting a leaf of size " + size + " so that left will have " + roff + " and right " + rsz )
            System.arraycopy( keyArr, roff, res.keyArr, 0, rsz )
//            System.arraycopy( valArr, roff, res.valArr, 0, rsz )
            res.size    = rsz
            size        = roff
            res
         }
         def asNode : Branch = notSupported
         def asLeaf : Leaf = this
      }

      class Branch extends BranchOrLeaf {
         var downArr = new Array[ NodeImpl ]( arrSize )
         def down( i: Int ) : NodeImpl = downArr( i )
         def split : NodeImpl = {
            val res     = new Branch
            val roff    = arrMid + 1
            val rsz     = size - roff
//println( "Splitting a branch of size " + size + " so that left will have " + roff + " and right " + rsz )
            System.arraycopy( keyArr, roff, res.keyArr, 0, rsz )
            System.arraycopy( downArr, roff, res.downArr, 0, rsz )
            res.size    = rsz
            size        = roff
            res
         }
         def asNode : Branch = this
         def asLeaf : Leaf = notSupported
      }

      private def notSupported = throw new IllegalArgumentException()

      sealed trait HeadOrBottom extends NodeImpl {
         def split : NodeImpl  = notSupported
         def asNode : Branch  = notSupported
         def asLeaf : Leaf    = notSupported
      }

      object Head extends HeadOrBottom {
         var downNode : NodeImpl = Bottom
         def key( i: Int ) : A = maxKey // MAX_KEY
         def down( i: Int ) : NodeImpl = downNode
         val size = 1
         val isBottom   = false
//         val isHead     = true
      }

      object Bottom extends HeadOrBottom {
         def key( i: Int ) : A         = notSupported
         def down( i: Int ) : NodeImpl  = notSupported
         val size = 0
         val isBottom   = true
//         val isHead     = false
      }
   }
}
trait HASkipList[ A ] extends SkipList[ A ] {
   def top : HASkipList.Node[ A ]
   def minGap : Int
   def maxGap : Int
}