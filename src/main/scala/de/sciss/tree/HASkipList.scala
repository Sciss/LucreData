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
import java.lang.IllegalArgumentException

// suckers

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 *
 * XXX todo: verify the structure doesn't get damaged when we try to insert an existing key
 */
object HASkipList {
//   def empty[ @specialized( Int, Long ) B : Manifest, A ]( minGap: Int = 1, key: A => B, maxKey: B ) : HASkipList[ A ] =
//      new Impl[ B, A ]( minGap, key maxKey )

   def withIntKey[ @specialized( Int, Long ) A : Manifest ]( keyFun: A => Int, minGap: Int = 1 ) : HASkipList[ A ] =
      new IntImpl( keyFun, minGap )

   sealed trait Node {
      def size : Int
      def key( i: Int ) : Int
      def down( i: Int ) : Node
      def isBottom : Boolean // = this eq Bottom
   }

   private object IntImpl {
      val MAX_KEY = 0x7FFFFFFF
   }
   private class IntImpl[ @specialized( Int, Long ) A : Manifest ]( keyFun: A => Int, val minGap: Int )
   extends HASkipList[ A ] { // extends Impl[ Int, A ]
      import IntImpl._

      val maxGap  = (minGap << 1) + 1
      val arrSize = maxGap + 1
      var arrMid  = maxGap >> 1

      def size : Int = leafSizeSum( Head ) - 1

      private def leafSizeSum( n: Node ) : Int = {
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

      def top : Node = Head.downNode

      // a kind of finger to navigate through
      // the data structure
      final class Nav {
         var node: IntNode = Head
         var idx: Int = 0
         def isBottom : Boolean = node.isBottom
//         def isHead : Boolean = node eq head
         def key : Int = node.key( idx )
         def moveRight( key: Int ) : Boolean = {
            while( key > node.key( idx )) idx += 1
            key == node.key( idx )
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

         def splitChild( key: Int, ch: Nav ) {
            val left       = ch.node
            val right      = left.split
            val splitKey   = left.key( arrMid )
            if( node eq Head ) {
               val n             = new Branch
               n.keyArr( 0 )     = splitKey
               n.keyArr( 1 )     = MAX_KEY // aka right.key( right.size - 1 )
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
               val i2            = i1 + 1
               val num           = n.size - i2
               if( num > 0 ) System.arraycopy( n.downArr, i1, n.keyArr, i2, num )
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
            if( key >= splitKey ) ch.node = right
         }

         /**
          * Inserts the key and value at
          * the current index into the
          * current node which is assumed
          * to be a leaf
          */
         def insert( key: Int, v: A ) {
            if( node eq Head ) {
               val n             = new Leaf
               n.keyArr( 0 )     = key
               n.keyArr( 1 )     = MAX_KEY // aka right.key( right.size - 1 )
               n.valArr( 0 )     = v
               n.size            = 2
               Head.downNode     = n
//println( "inserting new leaf below head of new size " + n.size )
//               node              = n
            } else {
               val n             = node.asLeaf
               val i1            = idx + 1
               val sz            = n.size - idx
//println( "inserting in node of old size " + n.size + " where idx = " + idx )
               System.arraycopy( n.keyArr, idx, n.keyArr, i1, sz )
               System.arraycopy( n.valArr, idx, n.valArr, i1, sz )
               n.keyArr( idx )   = key
               n.valArr( idx )   = v
               n.size           += 1
            }
         }
      }

      def contains( v: A ) : Boolean = {
         val key  = keyFun( v )
         val x    = new Nav
         while( !x.isBottom ) {
            if( x.moveRight( key )) return true
            x.moveDown
         }
         false
      }

      def add( v: A ) : Boolean = {
         val key  = keyFun( v )
         val x    = new Nav
         val x0   = new Nav
         do {
            if( x.moveRight( key )) return false // key was already present
            x.copy( x0 )
            x.moveDown
            if( x.isFull ) x0.splitChild( key, x )
         } while( !x.isBottom )
         x0.insert( key, v )
         true
      }

      sealed trait IntNode extends Node {
         override def down( i: Int ) : IntNode

         /**
          * Splits the node, and
          * returns the right hand side
          * as a new node. This old node
          * is shrunk to the left hand side
          */
         def split : IntNode

         def asNode : Branch
         def asLeaf : Leaf
      }

      sealed trait BranchOrLeaf extends IntNode {
         var keyArr     = new Array[ Int ]( arrSize )
         var size       = 0
         def key( i: Int ) : Int = keyArr( i )
         def isBottom   = false
//         def isHead     = false
      }

      class Leaf extends BranchOrLeaf {
         var valArr  = new Array[ A ]( arrSize )
         def down( i: Int ) : IntNode = Bottom
         def split : IntNode = {
            val res     = new Leaf
            val roff    = arrMid + 1
            val rsz     = size - roff
//println( "Splitting a leaf of size " + size + " so that left will have " + roff + " and right " + rsz )
            System.arraycopy( keyArr, roff, res.keyArr, 0, rsz )
            System.arraycopy( valArr, roff, res.valArr, 0, rsz )
            res.size    = rsz
            size        = roff
            res
         }
         def asNode : Branch = notSupported
         def asLeaf : Leaf = this
      }

      class Branch extends BranchOrLeaf {
         var downArr = new Array[ IntNode ]( arrSize )
         def down( i: Int ) : IntNode = downArr( i )
         def split : IntNode = {
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

      sealed trait HeadOrBottom extends IntNode {
         def split : IntNode  = notSupported
         def asNode : Branch  = notSupported
         def asLeaf : Leaf    = notSupported
      }

      object Head extends HeadOrBottom {
         var downNode : IntNode = Bottom
         def key( i: Int ) : Int = MAX_KEY
         def down( i: Int ) : IntNode = downNode
         val size = 1
         val isBottom   = false
//         val isHead     = true
      }

      object Bottom extends HeadOrBottom {
         def key( i: Int ) : Int       = notSupported
         def down( i: Int ) : IntNode  = notSupported
         val size = 0
         val isBottom   = true
//         val isHead     = false
      }
   }
}
trait HASkipList[ A ] extends SkipList[ A ] {
   def top : HASkipList.Node
   def minGap : Int
   def maxGap : Int
}