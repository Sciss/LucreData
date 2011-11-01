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
 */

package de.sciss.collection
package txn

import annotation.tailrec
import concurrent.stm.{TArray, InTxn}

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 */
object HASkipList {
   def empty[ A : Ordering : MaxKey : Manifest ] : HASkipList[ A ] = empty()
   def empty[ A ]( minGap: Int = 2, keyObserver: SkipList.KeyObserver[ A ] = SkipList.NoKeyObserver )
                 ( implicit ord: Ordering[ A ], maxKey: MaxKey[ A ], mf: Manifest[ A ]) : HASkipList[ A ] = {
      require( minGap >= 1, "Minimum gap (" + minGap + ") cannot be less than 1" )
      new Impl( maxKey.value, minGap, keyObserver )
   }

   sealed trait Node[ /* @specialized( Int, Long ) */ A ] {
      def size : Int
      def key( i: Int )( implicit tx: InTxn ) : A // Int
      def down( i: Int ) : Node[ A ]
      def isBottom : Boolean // = this eq Bottom
   }

   private final class Impl[ /* @specialized( Int, Long ) */ A ]
      ( val maxKey: A, val minGap: Int, keyObserver: SkipList.KeyObserver[ A ])
      ( implicit mf: Manifest[ A ], val ordering: Ordering[ A ])
   extends HASkipList[ A ] {
      private val arrMaxSz = maxGap + 1
      private val arrMid   = maxGap >> 1
      private val arrMinSz = minGap + 1

      /* override */ def size( implicit tx: InTxn ) : Int = leafSizeSum( Head ) - 1
      def maxKeyHolder : MaxKey[ A ] = MaxKey( maxKey )

      def maxGap : Int = (minGap << 1) + 1

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

      private def keyCopy( a: BranchOrLeaf, aOff: Int, b: BranchOrLeaf, bOff: Int, num: Int ) {
         sys.error( "TODO" ) // System.arraycopy( a.keyArr, aOff, b.keyArr, bOff, num )
      }

      private def downCopy( a: Branch, aOff: Int, b: Branch, bOff: Int, num: Int ) {
         sys.error( "TODO" ) // System.arraycopy( a.downArr, aOff, b.downArr, bOff, num )
      }

      def height( implicit tx: InTxn ) : Int = {
         var x: NodeImpl = Head.downNode
         var i = 0; while( !x.isBottom ) { x = x.down( 0 ); i += 1 }
         i
      }

      def top : Node[ A ] = Head.downNode

//      def isomorphicQuery( compare: A => Int ) : A = sys.error( "not yet implemented" )

      // ---- set support ----

      def contains( v: A )( implicit tx: InTxn ) : Boolean = {
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

      override def add( v: A )( implicit tx: InTxn ) : Boolean = {
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

            if( sn.hasMaxSize ) {
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
                  keyCopy( n, pidx, n, i1, n.size - pidx )
                  val num           = n.size - i1
                  if( num > 0 ) downCopy( n, i1, n, i1 + 1, num )
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
            keyCopy( n, pidx, n, pidx + 1, n.size - pidx )
            n.keyArr( pidx )  = v
            n.size           += 1
         }
         // ---- END INSERT ----
         true
      }

      def +=( elem: A )( implicit tx: InTxn ) : this.type = { add( elem ); this }
      def -=( elem: A )( implicit tx: InTxn ) : this.type = { remove( elem ); this }

      override def remove( v: A )( implicit tx: InTxn ) : Boolean = {
         var x             = Head.downNode
         // prevents infinite loop if user provided a surmountable maxKey, or if v == maxKey
         if( ordering.gteq( v, maxKey ) || x.isBottom ) return false

         var lastAbove     = maxKey    // last key at level above, needed to determine if we drop into the last gap

         while( true ) {
            var idx = 0
            var cmp = ordering.compare( v, x.key( idx ))
            while( cmp > 0 ) {   // find where you drop
               idx  += 1
               cmp   = ordering.compare( v, x.key( idx ))
            }
            var d       = x.down( idx )
            val dIsBot  = d.isBottom
            var xKey    = x.key( idx )

            if( dIsBot ) {
               val success = if( cmp == 0 ) {
                  val idx1 = idx + 1
                  val l    = x.asLeaf
                  val szl  = l.size
                  l.size   = szl - 1
                  if( idx1 < szl ) { // replace x by its right neighbour
                     keyCopy( l, idx1, l, idx, szl - idx1 )
                  } else { // this was the last element.
                     // therefore we just need to have the size decremented.
                     // but also, we need a second pass to remove the key
                     // from previous levels:
                     val prevKey = x.key( idx - 1 )
                     x = Head.downNode
                     while( !x.isBottom ) {
                        idx = 0
                        cmp = ordering.compare( v, x.key( idx ))
                        while( cmp > 0 ) {
                           idx += 1
                           cmp  = ordering.compare( v, x.key( idx ))
                        }
                        val d = x.down( idx )
                        if( cmp == 0 ) {
                           if( d.isBottom ) {
                              val lx            = x.asLeaf
                              lx.keyArr( idx )  = prevKey
                           } else {
                              val bx            = x.asBranch
                              bx.keyArr( idx )  = prevKey
                              keyObserver.keyDown( v )
                              keyObserver.keyUp( prevKey )
                           }
                        }
                        x = d
                     }
                  }
                  true

               } else { // which means the key was not found
                  false
               }

               // lower header of DSL, if necessary
               x = Head.downNode
               if( x.isEmpty ) {
                  Head.downNode = x.down( 0 )
               }

               return success

            } else if( d.hasMinSize ) {   // we drop into gap G with size minGap
               if( !ordering.equiv( xKey, lastAbove )) { // if does NOT drop in last gap -> merge or borrow to the right
                  // if minGap elems in next gap G' (aka xSucc.down.right),
                  // or at bottom level --> merge G and G', by lowering the element
                  // between G and G', that is xSucc
                  val idx1 = idx + 1
                  keyObserver.keyDown( xKey )
                  val b             = x.asBranch
                  xKey              = x.key( idx1 )
                  val rightSibling  = x.down( idx1 ) // .asBranch
                  if( rightSibling.hasMinSize ) {    // i.e. G' has size minGap -- merge
                     val idx2 = idx1 + 1
                     // overwrite x.key, but keep x.down
                     keyCopy(  b, idx1, b, idx,  b.size - idx1 )
                     downCopy( b, idx2, b, idx1, b.size - idx2 )
                     b.size -= 1
                     if( d.isLeaf ) {
                        val ld   = d.asLeaf
                        val lrs  = rightSibling.asLeaf
                        keyCopy( lrs, 0, ld, arrMinSz, arrMinSz )
                        ld.size  = arrMinSz + arrMinSz
                     } else {
                        val bd   = d.asBranch
                        val brs  = rightSibling.asBranch
                        keyCopy(  brs, 0, bd, arrMinSz, arrMinSz )
                        downCopy( brs, 0, bd, arrMinSz, arrMinSz )
                        bd.size  = arrMinSz + arrMinSz
                     }
                  } else {	   // if >minGap elems in next gap G' -- borrow
                     val upKey         = rightSibling.key( 0 ) // raise 1st elem in next gap & lower...
                     b.keyArr( idx )   = upKey
                     // ... separator of current+next gap
                     if( d.isLeaf ) {
                        val ld   = d.asLeaf
                        val lrs  = rightSibling.asLeaf
                        ld.keyArr( arrMinSz ) = upKey
                        ld.size  = arrMinSz + 1
                        val szm1 = lrs.size - 1
                        keyCopy( lrs, 1, lrs, 0, szm1 )
                        lrs.size = szm1
                     } else {
                        val bd   = d.asBranch
                        val brs  = rightSibling.asBranch
                        bd.keyArr( arrMinSz )   = upKey
                        bd.downArr( arrMinSz )  = brs.downArr( 0 )
                        bd.size  = arrMinSz + 1
                        val szm1 = brs.size - 1
                        keyCopy(  brs, 1, brs, 0, szm1 )
                        downCopy( brs, 1, brs, 0, szm1 )
                        brs.size = szm1
                     }
                     keyObserver.keyUp( upKey )
                  }
               } else {    // if DOES drop in last gap --> merge or borrow to the left
                  val idx1          = idx - 1
                  val leftSibling   = x.down( idx1 )
                  val dnKey         = x.key( idx1 ) // xPred.key
                  if( leftSibling.hasMinSize ) { // if only minGap elems in previous gap --> merge
                     keyObserver.keyDown( dnKey )
                     val b = x.asBranch   // XXX this could be factored out and go up one level
                     b.keyArr( idx1 ) = xKey
                     b.size -= 1
                     if( leftSibling.isLeaf ) {
                        val lls  = leftSibling.asLeaf
                        val ld   = d.asLeaf
                        val szld = ld.size
                        keyCopy( ld, 0, lls, arrMinSz, szld )
                        lls.size  = arrMinSz + szld
                     } else {
                        val bls = leftSibling.asBranch
                        val bd   = d.asBranch
                        val szbd = bd.size
                        keyCopy(  bd, 0, bls, arrMinSz, szbd )
                        downCopy( bd, 0, bls, arrMinSz, szbd )
                        bls.size  = arrMinSz + szbd
                     }
                     d = leftSibling

                  } else {    // if >minGap elems in previous gap --> borrow
                     val lssz1   = leftSibling.size - 1
                     val upKey   = leftSibling.key( lssz1 - 1 )
                     val b       = x.asBranch
                     b.keyArr( idx1 ) = upKey   // raise last elem in previous gap & lower...
                     if( d.isLeaf ) {
                        val ld            = d.asLeaf
                        val lls           = leftSibling.asLeaf
                        keyCopy( ld, 0, ld, 1, arrMinSz )
                        ld.keyArr( 0 )    = dnKey
                        ld.size           = arrMinSz + 1
                        lls.size          = lssz1
                     } else {
                        val bd            = d.asBranch
                        val bls           = leftSibling.asBranch
                        keyCopy(  bd, 0, bd, 1, arrMinSz )
                        downCopy( bd, 0, bd, 1, arrMinSz )
                        bd.keyArr( 0 )    = dnKey
                        bd.downArr( 0 )   = bls.downArr( lssz1 )
                        bd.size           = arrMinSz + 1
                        bls.size          = lssz1
                     }
                     keyObserver.keyDown( dnKey )
                     keyObserver.keyUp( upKey )
                  }
               }
            }

            lastAbove   = xKey // x.key( idx )
            x           = d
         }

         sys.error( "Never gets here" )
      }

//      def iterator : Iterator[ A ] = new Iterator[ A ] {
//         var x: Node[ A ]  = _
//         var idx: Int      = _
//         val stack         = collection.mutable.Stack.empty[ (Int, Node[ A ])]
//         pushDown( 0, Head )
//
//         def pushDown( idx0: Int, n: Node[ A ] ) {
//            var pred = n
//            var pidx = idx0
//            var dn   = pred.down( pidx )
//            while( !dn.isBottom ) {
//               stack.push( (pidx + 1, pred) )
//               pred  = dn
//               pidx  = 0
//               dn    = pred.down( pidx )
//            }
//            x     = pred
//            idx   = pidx
//         }
//
//         def hasNext : Boolean = !ordering.equiv( x.key( idx ), maxKey )
//         def next() : A = {
//            val res = x.key( idx )
//            idx += 1
//            if( idx == x.size ) {
//               @tailrec def findPush {
//                  if( stack.nonEmpty ) {
//                     val (i, n) = stack.pop
//                     if( i < n.size ) pushDown( i, n ) else findPush
//                  }
//               }
//               findPush
//            }
//            res
//         }
//      }

      private sealed trait NodeImpl extends Node[ A ] {
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
         def isLeaf : Boolean

         final def hasMaxSize = size == arrMaxSz
         final def hasMinSize = size == arrMinSz

         def isEmpty( implicit tx: InTxn ) : Boolean
      }

      private sealed trait BranchOrLeaf extends NodeImpl {
         final val keyArr  = TArray.ofDim[ A ]( arrMaxSz )
         final var size    = 0
         final def key( i: Int )( implicit tx: InTxn ) : A = keyArr( i )
         final def isBottom   = false

         final def isEmpty( implicit tx: InTxn ) = ordering.equiv( keyArr( 0 ), maxKey )

//         protected final def toString( name: String ) : String =
//            keyArr.toSeq.take( size ).map( k => if( k == maxKey ) "M" else k.toString ).mkString( name + "(", ", ", ")" )
      }

      private final class Leaf extends BranchOrLeaf {
         def down( i: Int ) : NodeImpl = Bottom
         def split() : NodeImpl = {
            val res     = new Leaf
            val roff    = arrMid + 1
            val rsz     = size - roff
            keyCopy( this, roff, res, 0, rsz )
            res.size    = rsz
            size        = roff
            res
         }
         def asBranch : Branch = notSupported
         def asLeaf : Leaf = this
         def isLeaf : Boolean = true

         override def toString = "Leaf" // toString( "Leaf" )
      }

      private final class Branch extends BranchOrLeaf {
         var downArr = new Array[ NodeImpl ]( arrMaxSz )
         def down( i: Int ) : NodeImpl = downArr( i )
         def split() : NodeImpl = {
            val res     = new Branch
            val roff    = arrMid + 1
            val rsz     = size - roff
            keyCopy(  this, roff, res, 0, rsz )
            downCopy( this, roff, res, 0, rsz )
            res.size    = rsz
            size        = roff
            res
         }
         def asBranch : Branch = this
         def asLeaf : Leaf = notSupported
         def isLeaf : Boolean = false

         override def toString = "Branch" // toString( "Branch" )
      }

      private def notSupported = throw new IllegalArgumentException()

      private sealed trait HeadOrBottom extends NodeImpl {
         final def split() : NodeImpl  = notSupported
         final def asBranch : Branch   = notSupported
         final def asLeaf : Leaf       = notSupported
         final def isLeaf : Boolean    = false
         final def isEmpty( implicit tx: InTxn ) = false
      }

      private object Head extends HeadOrBottom {
         var downNode : NodeImpl = Bottom
         def key( i: Int )( implicit tx: InTxn ) : A = maxKey // MAX_KEY
         def down( i: Int ) : NodeImpl = downNode
         val size = 1
         val isBottom   = false

         override def toString = "Head"
      }

      private object Bottom extends HeadOrBottom {
         def key( i: Int )( implicit tx: InTxn ) : A  = notSupported
         def down( i: Int ) : NodeImpl                = notSupported
         val size = 0
         val isBottom   = true

         override def toString = "Bottom"
      }
   }
}
sealed trait HASkipList[ A ] extends SkipList[ A ] {
   def top : HASkipList.Node[ A ]
}