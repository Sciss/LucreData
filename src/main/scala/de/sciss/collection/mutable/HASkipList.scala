/*
 *  HASkipList.scala
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

import sys.error
import annotation.tailrec

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 *
 * XXX todo: removal is not yet implemented
 */
object HASkipList {
//   def empty[ @specialized( Int, Long ) B : Manifest, A ]( minGap: Int = 1, key: A => B, maxKey: B ) : HASkipList[ A ] =
//      new Impl[ B, A ]( minGap, key maxKey )

   def empty[ A : Ordering : MaxKey : Manifest ] : HASkipList[ A ] = empty()
   def empty[ A ]( minGap: Int = 2, keyObserver: SkipList.KeyObserver[ A ] = SkipList.NoKeyObserver )
                 ( implicit ord: Ordering[ A ], maxKey: MaxKey[ A ], mf: Manifest[ A ]) : HASkipList[ A ] = {
      require( minGap >= 1, "Minimum gap (" + minGap + ") cannot be less than 1" )
      new Impl( maxKey.value, minGap, keyObserver )
   }

   sealed trait Node[ /* @specialized( Int, Long ) */ A ] {
      def size : Int
      def key( i: Int ) : A // Int
      def down( i: Int ) : Node[ A ]
      def isBottom : Boolean // = this eq Bottom
   }

   private final class Impl[ /* @specialized( Int, Long ) */ A ]
      ( val maxKey: A, val minGap: Int, keyObserver: SkipList.KeyObserver[ A ])
      ( implicit mf: Manifest[ A ], val ordering: Ordering[ A ])
   extends HASkipList[ A ] {
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
         var x: NodeImpl = Head.downNode
         var i = 0; while( !x.isBottom ) { x = x.down( 0 ); i += 1 }
         i
      }

      def top : Node[ A ] = Head.downNode

      def isomorphicQuery( compare: A => Int ) : A = error( "not yet implemented" )

      // ---- set support ----

      def contains( v: A ) : Boolean = {
         if( ordering.gteq( v, maxKey )) return false
         var x: NodeImpl = Head.downNode
         while( !x.isBottom ) {
            var idx = 0
            var cmp = ordering.compare( v, x.key( idx ))
            while( cmp > 0 ) {
               idx += 1
               cmp  = ordering.compare( v, x.key( idx ))
            }
            if( cmp == 0 ) return true
            x = x.down( idx )
         }
         false
      }

      override def add( v: A ) : Boolean = {
         require( ordering.lt( v, maxKey ), "Cannot add key (" + v + ") greater or equal to maxKey" )
//         val key  = keyFun( v )
         var pn: NodeImpl = Head
         var sn   = Head.downNode
         var pidx = 0
         while( !sn.isBottom ) {
            var idx = 0
            var cmp = ordering.compare( v, sn.key( idx ))
            while( cmp > 0 ) {
               idx += 1
               cmp = ordering.compare( v, sn.key( idx ))
            }
            if( cmp == 0 ) return false

            if( sn.isFull ) {
               // ---- BEGIN SPLIT ----
               val left       = sn
               val right      = left.split()
               val splitKey   = left.key( arrMid )
               if( pn eq Head ) {
                  val n             = new Branch
                  n.keyArr( 0 )     = splitKey
                  n.keyArr( 1 )     = maxKey // MAX_KEY // aka right.key( right.size - 1 )
                  n.downArr( 0 )    = left
                  n.downArr( 1 )    = right
                  n.size            = 2
                  Head.downNode     = n
               } else {
                  val n             = pn.asBranch
                  val i1            = pidx + 1
                  System.arraycopy( n.keyArr, pidx, n.keyArr, i1, n.size - pidx )
                  val num           = n.size - i1
                  if( num > 0 ) System.arraycopy( n.downArr, i1, n.downArr, i1 + 1, num )
                  n.keyArr( pidx )  = splitKey
                  // this is already the case:
//               n.downArr( idx )  = left
                  n.downArr( i1 )   = right
                  n.size           += 1
               }

               // notify observer
               keyObserver.keyUp( splitKey )

               // important: if the current key of the parent
               // is greater or equal than the splitKey,
               // we must update the child navigation accordingly,
               // beause it means we are now traversing the right
               // half!
               if( idx >= left.size ) {
                  sn    = right
                  idx  -= left.size
               }
               // ---- END SPLIT ----
            }
            pn    = sn
            sn    = sn.down( idx )
            pidx  = idx
         }

         // ---- BEGIN INSERT ----
         if( pn eq Head ) {
            val n             = new Leaf
            n.keyArr( 0 )     = v
            n.keyArr( 1 )     = maxKey // MAX_KEY // aka right.key( right.size - 1 )
            n.size            = 2
            Head.downNode     = n
         } else {
            val n             = pn.asLeaf
            System.arraycopy( n.keyArr, pidx, n.keyArr, pidx + 1, n.size - pidx )
            n.keyArr( pidx )  = v
            n.size           += 1
         }
         // ---- END INSERT ----
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

         def hasNext : Boolean = !ordering.equiv( x.key( idx ), maxKey )
         def next() : A = {
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
         def split() : NodeImpl

         def asBranch : Branch
         def asLeaf : Leaf

         def isFull = size == arrSize
      }

      sealed trait BranchOrLeaf extends NodeImpl {
         var keyArr     = new Array[ A ]( arrSize )
         var size       = 0
         def key( i: Int ) : A = keyArr( i )
         def isBottom   = false
//         def isHead     = false

         protected final def toString( name: String ) : String =
            keyArr.toSeq.take( size ).map( k => if( k == maxKey ) "M" else k.toString ).mkString( name + "(", ", ", ")" )
      }

      final class Leaf extends BranchOrLeaf {
//         var valArr  = new Array[ A ]( arrSize )
         def down( i: Int ) : NodeImpl = Bottom
         def split() : NodeImpl = {
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
         def asBranch : Branch = notSupported
         def asLeaf : Leaf = this

         override def toString = toString( "Leaf" )
      }

      final class Branch extends BranchOrLeaf {
         var downArr = new Array[ NodeImpl ]( arrSize )
         def down( i: Int ) : NodeImpl = downArr( i )
         def split() : NodeImpl = {
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
         def asBranch : Branch = this
         def asLeaf : Leaf = notSupported

         override def toString = toString( "Branch" )
      }

      private def notSupported = throw new IllegalArgumentException()

      sealed trait HeadOrBottom extends NodeImpl {
         def split() : NodeImpl  = notSupported
         def asBranch : Branch  = notSupported
         def asLeaf : Leaf    = notSupported
      }

      object Head extends HeadOrBottom {
         var downNode : NodeImpl = Bottom
         def key( i: Int ) : A = maxKey // MAX_KEY
         def down( i: Int ) : NodeImpl = downNode
         val size = 1
         val isBottom   = false
//         val isHead     = true

         override def toString = "Head"
      }

      object Bottom extends HeadOrBottom {
         def key( i: Int ) : A         = notSupported
         def down( i: Int ) : NodeImpl  = notSupported
         val size = 0
         val isBottom   = true
//         val isHead     = false

         override def toString = "Bottom"
      }
   }
}
trait HASkipList[ A ] extends SkipList[ A ] {
   def top : HASkipList.Node[ A ]
}