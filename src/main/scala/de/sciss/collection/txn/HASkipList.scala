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
import collection.mutable.Builder
import concurrent.stm.{Ref, InTxn}

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 */
object HASkipList {
   def empty[ A : de.sciss.collection.Ordering : MaxKey : Manifest ] : HASkipList[ A ] = empty()
   def empty[ A ]( minGap: Int = 2, keyObserver: txn.SkipList.KeyObserver[ A ] = txn.SkipList.NoKeyObserver )
                 ( implicit ord: de.sciss.collection.Ordering[ A ], maxKey: MaxKey[ A ], mf: Manifest[ A ]) : HASkipList[ A ] = {
      require( minGap >= 1, "Minimum gap (" + minGap + ") cannot be less than 1" )
      new Impl( maxKey.value, minGap, keyObserver )
   }

   private final class Impl[ /* @specialized( Int, Long ) */ A ]
      ( val maxKey: A, val minGap: Int, keyObserver: txn.SkipList.KeyObserver[ A ])
      ( implicit mf: Manifest[ A ], val ordering: de.sciss.collection.Ordering[ A ])
   extends HASkipList[ A ] {
      private val arrMinSz = minGap + 1
      private val arrMaxSz = arrMinSz << 1   // aka maxGap + 1

      override def size( implicit tx: InTxn ) : Int = Head.downNode() match {
         case BottomImpl  => 0
         case n: NodeImpl => leafSizeSum( n )
      }

      def maxGap : Int = arrMaxSz - 1  // aka (minGap << 1) + 1
      def maxKeyHolder : MaxKey[ A ] = MaxKey( maxKey )

      def isEmpty( implicit tx: InTxn )   = Head.downNode() eq BottomImpl
      def notEmpty( implicit tx: InTxn )  = !isEmpty

      def toIndexedSeq( implicit tx: InTxn ) : collection.immutable.IndexedSeq[ A ] = {
         val b = collection.immutable.IndexedSeq.newBuilder[ A ]
         fillBuilder( b )
         b.result()
      }

      def toList( implicit tx: InTxn ) : List[ A ] = {
         val b = List.newBuilder[ A ]
         fillBuilder( b )
         b.result()
      }

      def toSeq( implicit tx: InTxn ) : Seq[ A ] = {
         val b = Seq.newBuilder[ A ]
         fillBuilder( b )
         b.result()
      }

      def toSet( implicit tx: InTxn ) : Set[ A ] = {
         val b = Set.newBuilder[ A ]
         fillBuilder( b )
         b.result()
      }

      private def fillBuilder( b: Builder[ A, _ ])( implicit tx: InTxn ) {
         val iter = iterator
         while( iter.hasNext ) b += iter.next()
      }

      private def leafSizeSum( n: NodeImpl )( implicit tx: InTxn ) : Int = {
         val sz = n.size
         n match {
            case l: LeafImpl   => sz
            case b: BranchImpl =>
               var res = 0
               var i = 0; while( i < sz ) {
                  res += leafSizeSum( b.down( i ))
               i += 1 }
               res
         }
      }

      @inline private def keyCopy( a: NodeImpl, aOff: Int, b: Array[ A ], bOff: Int, num: Int ) {
         System.arraycopy( a.keys, aOff, b, bOff, num )
      }

      @inline private def downCopy( a: BranchImpl, aOff: Int,
                                    b: Array[ Ref[ NodeImpl ]], bOff: Int, num: Int )( implicit tx: InTxn ) {
         var i = 0; while( i < num ) {
            b( i + bOff ) = Ref( a.down( i + aOff ))
         i += 1 }
      }

      def height( implicit tx: InTxn ) : Int = {
         @tailrec def step( num: Int, n: NodeImpl ) : Int = n match {
            case l: LeafImpl   => num
            case b: BranchImpl => step( num + 1, b )
         }

         Head.downNode() match {
            case BottomImpl   => 0
            case n: NodeImpl  => step( 1, n )
         }
      }

      def top( implicit tx: InTxn ) : Child = Head.downNode()

//      def isomorphicQuery( compare: A => Int ) : A = sys.error( "not yet implemented" )

      // ---- set support ----

      def contains( v: A )( implicit tx: InTxn ) : Boolean = {
         sys.error( "TODO" )
//         if( ordering.gteq( v, maxKey )) return false
//         var x: NodeImpl = Head.downNode()
//         while( !x.isBottom ) {
//            var idx = 0
//            var cmp = ordering.compare( v, x.key( idx ))
//            while( cmp > 0 ) {
//               idx += 1
//               cmp  = ordering.compare( v, x.key( idx ))
//            }
//            if( cmp == 0 ) return true
//            x = x.down( idx )
//         }
//         false
      }

      override def add( v: A )( implicit tx: InTxn ) : Boolean = {
         Head.downNode() match {
            case BottomImpl =>
               val lkeys         = new Array[ A ]( 2 )
               lkeys( 0 )        = v
               lkeys( 1 )        = maxKey
               val l             = new LeafImpl( lkeys )
               Head.downNode()   = l
               true

            case l: LeafImpl   => addLeaf(   v, Head, 0, Head, 0, l )
            case b: BranchImpl => addBranch( v, Head, 0, Head, 0, b )
         }
      }

      /*
       * Finds the right-most key which
       * is greater than or equal to the query key.
       *
       * @param   v  the key to search for
       * @param   sn the branch or leaf from which to go down
       *
       * @return  the index to go down (a node whose key is greater than `v`),
        *         or `-1` if `v` was found
       */
      @inline private def addFindDown( v: A, n: NodeImpl ) : Int = {
         @tailrec def step( idx: Int ) : Int = {
            val cmp = ordering.compare( v, n.key( idx ))
            if( cmp == 0 ) -1 else if( cmp < 0 ) idx else step( idx + 1 )
         }
         step( 0 )
      }

      private def addLeaf( v: A, pp: HeadOrBranch, ppidx: Int, p: HeadOrBranch, pidx: Int, l: LeafImpl )
                         ( implicit tx: InTxn ) : Boolean = {
         val idx = addFindDown( v, l )
         if( idx == -1 ) return false

         if( l.hasMaxSize ) {
            val tup        = l.splitAndInsert( v, idx )
            val left       = tup._1
            val right      = tup._2
            val splitKey   = left.key( minGap )
            val pNew       = p.insertAfterSplit( pidx, splitKey, left, right )
            pp.updateDown( ppidx, pNew )
            keyObserver.keyUp( splitKey )
         } else {
            val lNew       = l.insert( v, idx )
            // and overwrite down entry in pn's parent
            p.updateDown( pidx, lNew )
         }
         true
      }

      @tailrec private def addBranch( v: A, pp: HeadOrBranch, ppidx: Int, p: HeadOrBranch, pidx: Int, b: BranchImpl )
                                    ( implicit tx: InTxn ) : Boolean = {
         val idx = addFindDown( v, b )
         if( idx == -1 ) return false

         var bNew    = b
         var idxNew  = idx
         var pNew    = p
         var pidxNew = pidx

         if( b.hasMaxSize ) {
            val tup        = b.split
            val left       = tup._1
            val right      = tup._2
            val splitKey   = left.key( minGap )
            val pbNew      = p.insertAfterSplit( pidx, splitKey, left, right )
            pNew           = pbNew
            pp.updateDown( ppidx, pbNew )
            if( idx < arrMinSz ) {
               bNew     = left
            } else {
               bNew     = right
               pidxNew += 1
               idxNew  -= arrMinSz
            }
         }

         bNew.down( idxNew ) match {
            case l: LeafImpl    => addLeaf(   v, pNew, pidxNew, bNew, idxNew, l  )
            case bc: BranchImpl => addBranch( v, pNew, pidxNew, bNew, idxNew, bc )
         }
      }

//      def addXX( v: A )( implicit tx: InTxn ) : Boolean = {
//         require( ordering.lt( v, maxKey ), "Cannot add key (" + v + ") greater or equal to maxKey" )
//         var sn               = Head.downNode() // the current node we are comparing with
//         var pn: NodeImpl     = Head            // it's parent
//         var ppn: NodeImpl    = null            // and the parent of the parent
//         var ppidx            = 0
//         var pidx             = 0
//         while( !sn.isBottom ) {
//            // find the right-most key which
//            // is greater than or equal to the query key
//            var idx = 0
//            var cmp = ordering.compare( v, sn.key( idx ))
//            while( cmp > 0 ) {
//               idx += 1
//               cmp = ordering.compare( v, sn.key( idx ))
//            }
//            // if we found the key, we
//            // can abort immediately and
//            // return `false` - key already present
//            if( cmp == 0 ) return false
//
//            // if we would go down a full node, we need to split it first
//            if( sn.hasMaxSize ) {
//               // ---- BEGIN SPLIT ----
//               val tup        = sn.split
//               val left       = tup._1
//               val right      = tup._2
//               val splitKey   = left.key( minGap )
//               // if the parent is `Head`, we insert
//               // a new parent with the new split nodes
//               // `left` and `right` as children
//               if( pn eq Head ) {
//                  val bkeys         = new Array[ A ]( 2 )
//                  bkeys( 0 )        = splitKey  // left node ends in the split key
//                  bkeys( 1 )        = maxKey    // right node ends in max key (remember parent is `Head`!)
//                  val bdowns        = new Array[ Ref[ NodeImpl ]]( 2 )
//                  bdowns( 0 )       = Ref( left )
//                  bdowns( 1 )       = Ref( right )
//                  pn                = new Branch( bkeys, bdowns ) // new parent branch
//                  Head.downNode()   = pn
//
//               } else {
//                  // parent is not `Head`, but an internal branch.
//                  // we must make a copy of this branch with the
//                  // size increased by one. the new key is `splitKey`
//                  // which gets inserted at the index where we went
//                  // down, `pidx`.
//                  val pbOld         = pn.asBranch
//                  val pbszOld       = pbOld.size
//                  val bsz           = pbszOld + 1
//                  val bkeys         = new Array[ A ]( bsz )
//                  val bdowns        = new Array[ Ref[ NodeImpl ]]( bsz )
//                  // copy entries left to split index
//                  if( pidx > 0 ) {
//                     keyCopy(  pbOld, 0, bkeys,  0, pidx )
//                     downCopy( pbOld, 0, bdowns, 0, pidx )
//                  }
//                  // insert the left split entry
//                  bkeys( pidx )     = splitKey
//                  bdowns( pidx )    = Ref( left )
//                  // copy entries right to split index
//                  val rightOff      = pidx + 1
//                  val num           = pbszOld - pidx
//                  keyCopy( pbOld, pidx, bkeys, rightOff, num )
//                  // while we could copy the right split entry's key,
//                  // the split operation has yielded a new right node
//                  bdowns( rightOff ) = Ref( right )
//                  if( num > 1 ) {
//                     downCopy( pbOld, rightOff, bdowns, rightOff + 1, num - 1 )
//                  }
//
//                  pn = new Branch( bkeys, bdowns )
//                  // make sure to rewrite the down entry
//                  // for the parent's parent
//                  ppn.down_=( ppidx, pn )
//               }
//
//               // notify observer
//               keyObserver.keyUp( splitKey )
//
//               // important: if the current key of the parent
//               // is greater or equal than the splitKey,
//               // we must update the child navigation accordingly,
//               // because it means we are now traversing the right
//               // half! in any case, we need to assign one of
//               // the new split nodes to `sn`
//               if( idx < arrMinSz ) {
//                  sn    = left
//               } else {
//                  sn    = right
//                  pidx += 1
//                  idx  -= arrMinSz
//               }
//               // ---- END SPLIT ----
//            }
//            ppn   = pn
//            ppidx = pidx
//            pn    = sn
//            sn    = sn.down( idx )
//            pidx  = idx
//         }
//
//         // now we have reached the leaf level,
//         // and final insertion takes place.
//         // note that actually `sn` is `Bottom`,
//         // and `pn` is the `Leaf` into which
//         // we would like to insert. `pidx` is
//         // the index into which the new entry
//         // is written.
//
//         // ---- BEGIN INSERT ----
//         // we never insert into `Head`, so in
//         // that case, create a new leaf with
//         // our entry and the max entry.
//         if( pn eq Head ) {
//            val lkeys         = new Array[ A ]( 2 )
//            lkeys( 0 )        = v
//            lkeys( 1 )        = maxKey
//            val l             = new Leaf( lkeys )
//            Head.downNode()   = l
//         } else {
//            val lOld          = pn.asLeaf
//            val lszOld        = lOld.size
//            val lsz           = lszOld + 1
//            val lkeys         = new Array[ A ]( lsz )
//            // copy keys left to the insertion index
//            if( pidx > 0 ) keyCopy( lOld, 0, lkeys, 0, pidx )
//            // put the new value
//            lkeys( pidx )    = v
//            // copy the keys right to the insertion index
//            val num           = lszOld - pidx
//            if( num > 0 ) keyCopy( lOld, pidx, lkeys, pidx + 1, num )
//            val l             = new Leaf( lkeys )
//            // and overwrite down entry in pn's parent
//            ppn.down_=( ppidx, l )
//         }
//         // ---- END INSERT ----
//         true
//      }

      def +=( elem: A )( implicit tx: InTxn ) : this.type = { add( elem ); this }
      def -=( elem: A )( implicit tx: InTxn ) : this.type = { remove( elem ); this }

      override def remove( v: A )( implicit tx: InTxn ) : Boolean = {
//         var pn: NodeImpl  = Head
//         var x             = Head.downNode()
//         // prevents infinite loop if user provided a surmountable maxKey, or if v == maxKey
//         if( ordering.gteq( v, maxKey ) || x.isBottom ) return false
//
//         var lastAbove     = maxKey    // last key at level above, needed to determine if we drop into the last gap
//
//         while( true ) {
//            var idx = 0
//            var cmp = ordering.compare( v, x.key( idx ))
//            while( cmp > 0 ) {   // find where you drop
//               idx  += 1
//               cmp   = ordering.compare( v, x.key( idx ))
//            }
//            var d       = x.down( idx )
//            val dIsBot  = d.isBottom
//            var xKey    = x.key( idx )
//
//            if( dIsBot ) {
//               val success = if( cmp == 0 ) {
//                  val idx1 = idx + 1
//                  val l    = x.asLeaf
//                  val szl  = l.size
//                  l.size   = szl - 1
//                  if( idx1 < szl ) { // replace x by its right neighbour
//                     keyCopy( l, idx1, l, idx, szl - idx1 )
//                  } else { // this was the last element.
//                     // therefore we just need to have the size decremented.
//                     // but also, we need a second pass to remove the key
//                     // from previous levels:
//                     val prevKey = x.key( idx - 1 )
//                     x = Head.downNode()
//                     while( !x.isBottom ) {
//                        idx = 0
//                        cmp = ordering.compare( v, x.key( idx ))
//                        while( cmp > 0 ) {
//                           idx += 1
//                           cmp  = ordering.compare( v, x.key( idx ))
//                        }
//                        val d = x.down( idx )
//                        if( cmp == 0 ) {
//                           if( d.isBottom ) {
//                              val lx            = x.asLeaf
//                              lx.keyArr( idx )  = prevKey
//                           } else {
//                              val bx            = x.asBranch
//                              bx.keyArr( idx )  = prevKey
//                              keyObserver.keyDown( v )
//                              keyObserver.keyUp( prevKey )
//                           }
//                        }
//                        x = d
//                     }
//                  }
//                  true
//
//               } else { // which means the key was not found
//                  false
//               }
//
//               // lower header of DSL, if necessary
//               x = Head.downNode()
//               if( x.isEmpty ) {
//                  Head.downNode() = x.down( 0 )
//               }
//
//               return success
//
//            } else if( d.hasMinSize ) {   // we drop into gap G with size minGap
//               if( !ordering.equiv( xKey, lastAbove )) { // if does NOT drop in last gap -> merge or borrow to the right
//                  // if minGap elems in next gap G' (aka xSucc.down.right),
//                  // or at bottom level --> merge G and G', by lowering the element
//                  // between G and G', that is xSucc
//                  val idx1 = idx + 1
//                  keyObserver.keyDown( xKey )
//                  val b             = x.asBranch
//                  xKey              = x.key( idx1 )
//                  val rightSibling  = x.down( idx1 ) // .asBranch
//                  if( rightSibling.hasMinSize ) {    // i.e. G' has size minGap -- merge
//                     val idx2 = idx1 + 1
//                     // overwrite x.key, but keep x.down
//                     val bsz = b.size
//                     keyCopy(  b, idx1, b, idx,  bsz - idx1 )
//                     downCopy( b, idx2, b, idx1, bsz - idx2 )
//                     b.size  = bsz - 1
//                     if( d.isLeaf ) {
//                        val ld   = d.asLeaf
//                        val lrs  = rightSibling.asLeaf
//                        keyCopy( lrs, 0, ld, arrMinSz, arrMinSz )
//                        ld.size  = arrMinSz + arrMinSz
//                     } else {
//                        val bd   = d.asBranch
//                        val brs  = rightSibling.asBranch
//                        keyCopy(  brs, 0, bd, arrMinSz, arrMinSz )
//                        downCopy( brs, 0, bd, arrMinSz, arrMinSz )
//                        bd.size  = arrMinSz + arrMinSz
//                     }
//                  } else {	   // if >minGap elems in next gap G' -- borrow
//                     val upKey         = rightSibling.key( 0 ) // raise 1st elem in next gap & lower...
//                     b.keyArr( idx )   = upKey
//                     // ... separator of current+next gap
//                     if( d.isLeaf ) {
//                        val ld   = d.asLeaf
//                        val lrs  = rightSibling.asLeaf
//                        ld.keyArr( arrMinSz ) = upKey
//                        ld.size  = arrMinSz + 1
//                        val szm1 = lrs.size - 1
//                        keyCopy( lrs, 1, lrs, 0, szm1 )
//                        lrs.size = szm1
//                     } else {
//                        val bd   = d.asBranch
//                        val brs  = rightSibling.asBranch
//                        bd.keyArr( arrMinSz )   = upKey
//                        bd.downArr( arrMinSz )  = brs.downArr( 0 )
//                        bd.size  = arrMinSz + 1
//                        val szm1 = brs.size - 1
//                        keyCopy(  brs, 1, brs, 0, szm1 )
//                        downCopy( brs, 1, brs, 0, szm1 )
//                        brs.size = szm1
//                     }
//                     keyObserver.keyUp( upKey )
//                  }
//               } else {    // if DOES drop in last gap --> merge or borrow to the left
//                  val idx1          = idx - 1
//                  val leftSibling   = x.down( idx1 )
//                  val dnKey         = x.key( idx1 ) // xPred.key
//                  if( leftSibling.hasMinSize ) { // if only minGap elems in previous gap --> merge
//                     keyObserver.keyDown( dnKey )
//                     val b = x.asBranch   // XXX this could be factored out and go up one level
//                     b.keyArr( idx1 ) = xKey
//                     b.size -= 1
//                     if( leftSibling.isLeaf ) {
//                        val lls  = leftSibling.asLeaf
//                        val ld   = d.asLeaf
//                        val szld = ld.size
//                        keyCopy( ld, 0, lls, arrMinSz, szld )
//                        lls.size  = arrMinSz + szld
//                     } else {
//                        val bls = leftSibling.asBranch
//                        val bd   = d.asBranch
//                        val szbd = bd.size
//                        keyCopy(  bd, 0, bls, arrMinSz, szbd )
//                        downCopy( bd, 0, bls, arrMinSz, szbd )
//                        bls.size  = arrMinSz + szbd
//                     }
//                     d = leftSibling
//
//
//                  } else {    // if >minGap elems in previous gap --> borrow
//                     val lssz1   = leftSibling.size - 1
//                     val upKey   = leftSibling.key( lssz1 - 1 )
//                     val b       = x.asBranch
//                     b.keyArr( idx1 ) = upKey   // raise last elem in previous gap & lower...
//                     if( d.isLeaf ) {
//                        val ld            = d.asLeaf
//                        val lls           = leftSibling.asLeaf
//                        keyCopy( ld, 0, ld, 1, arrMinSz )
//                        ld.keyArr( 0 )    = dnKey
//                        ld.size           = arrMinSz + 1
//                        lls.size          = lssz1
//                     } else {
//                        val bd            = d.asBranch
//                        val bls           = leftSibling.asBranch
//                        keyCopy(  bd, 0, bd, 1, arrMinSz )
//                        downCopy( bd, 0, bd, 1, arrMinSz )
//                        bd.keyArr( 0 )    = dnKey
//                        bd.downArr( 0 )   = bls.downArr( lssz1 )
//                        bd.size           = arrMinSz + 1
//                        bls.size          = lssz1
//                     }
//                     keyObserver.keyDown( dnKey )
//                     keyObserver.keyUp( upKey )
//                  }
//               }
//            }
//
//            lastAbove   = xKey // x.key( idx )
//            x           = d
//         }
//
         sys.error( "Never gets here" )
      }

      def iterator( implicit tx: InTxn ) : Iterator[ A ] = {
         sys.error( "TODO" )
//         val i = new IteratorImpl
//         i.pushDown( 0, Head )
//         i
      }

//      private final class IteratorImpl extends Iterator[ A ] {
//         private var x: Node       = _
//         private var idx: Int      = _
//         private val stack         = collection.mutable.Stack.empty[ (Int, Node[ A ])]
////         pushDown( 0, Head )
//
//         def pushDown( idx0: Int, n: Node )( implicit tx: InTxn ) {
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
//         def hasNext( implicit tx: InTxn ) : Boolean = !ordering.equiv( x.key( idx ), maxKey )
//         def next()( implicit tx: InTxn ) : A = {
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

      private sealed trait NodeOrBottom extends Child

      private sealed trait NodeImpl extends Node with NodeOrBottom {
//         override def down( i: Int )( implicit tx: InTxn ) : NodeImpl

         final def hasMaxSize = size == arrMaxSz
         final def hasMinSize = size == arrMinSz

         final def size : Int = keys.size
         def keys: Array[ A ]
         final def key( i: Int ) : A = keys( i )

         final def isEmpty = ordering.equiv( key( 0 ), maxKey )

         protected final def toString( name: String ) : String =
            keys.toSeq.take( size ).map( k => if( k == maxKey ) "M" else k.toString ).mkString( name + "(", ", ", ")" )
      }

      private final class LeafImpl( val keys: Array[ A ])
      extends NodeImpl with Leaf {
//         def down( i: Int )( implicit tx: InTxn )  : NodeImpl = Bottom
//         def down_=( i: Int, n: NodeImpl )( implicit tx: InTxn ) { notSupported }

         def insert( v: A, idx: Int ) : LeafImpl = {
            val lsz     = size + 1
            val lkeys   = new Array[ A ]( lsz )
            // copy keys left to the insertion index
            if( idx > 0 ) keyCopy( this, 0, lkeys, 0, idx )
            // put the new value
            lkeys( idx ) = v
            // copy the keys right to the insertion index
            val idxp1   = idx + 1
            val numr    = lsz - idxp1
            if( numr > 0 ) keyCopy( this, idx, lkeys, idxp1, numr )
            new LeafImpl( lkeys )
         }

         def splitAndInsert( v: A, idx: Int ) : (LeafImpl, LeafImpl) = {
            assert( size == arrMaxSz )

            if( idx < arrMinSz ) {  // split and add `v` to left leaf
               val lsz     = arrMinSz + 1
               val lkeys   = new Array[ A ]( lsz )
               if( idx > 0 ) keyCopy( this, 0, lkeys, 0, idx )
               lkeys( idx ) = v
               val numr    = arrMinSz - idx
               if( numr > 0 ) keyCopy( this, idx, lkeys, idx + 1, numr )
               val left    = new LeafImpl( lkeys )

               val rsz     = arrMinSz
               val rkeys   = new Array[ A ]( rsz )
               keyCopy( this, arrMinSz, rkeys, 0, rsz )
               val right   = new LeafImpl( rkeys )

               (left, right)

            } else {               // split and add `v` to right leaf
               val lsz     = arrMinSz
               val lkeys   = new Array[ A ]( lsz )
               keyCopy( this, 0, lkeys, 0, lsz )
               val left    = new LeafImpl( lkeys )

               val rsz     = arrMinSz + 1
               val rkeys   = new Array[ A ]( rsz )
               val numl    = idx - arrMinSz
               if( numl > 0 ) keyCopy( this, arrMinSz, rkeys, 0, numl )
               rkeys( numl ) = v
               val numr    = arrMinSz - numl
               if( numr > 0 ) keyCopy( this, idx, rkeys, numl + 1, numr )
               val right   = new LeafImpl( rkeys )

               (left, right)
            }
         }

         override def toString = toString( "Leaf" )
      }

      private sealed trait HeadOrBranch {
         def updateDown( i: Int, n: NodeImpl )( implicit tx: InTxn ) : Unit

         def insertAfterSplit( pidx: Int, splitKey: A, left: NodeImpl, right: NodeImpl )
                             ( implicit tx: InTxn ) : BranchImpl
      }

      private final class BranchImpl( val keys: Array[ A ], downs: Array[ Ref[ NodeImpl ]])
      extends NodeImpl with HeadOrBranch with Branch {
         assert( keys.size == downs.size )

         def down( i: Int )( implicit tx: InTxn ) : NodeImpl = downs( i )()
         def split( implicit tx: InTxn ) : (BranchImpl, BranchImpl) = {
            val lsz     = arrMinSz
            val lkeys   = new Array[ A ]( lsz )
            val ldowns  = new Array[ Ref[ NodeImpl ]]( lsz )
            keyCopy( this, 0, lkeys, 0, lsz )
            downCopy( this, 0, ldowns, 0, lsz )
            val left    = new BranchImpl( lkeys, ldowns )

            val rsz     = size - lsz
            val rkeys   = new Array[ A ]( rsz )
            val rdowns  = new Array[ Ref[ NodeImpl ]]( rsz )
            keyCopy( this, lsz, rkeys, 0, rsz )
            downCopy( this, lsz, rdowns, 0, rsz )
            val right   = new BranchImpl( rkeys, rdowns )

            (left, right)
         }

         def updateDown( i: Int, n: NodeImpl )( implicit tx: InTxn ) {
            downs( i ).set( n )
         }

         def insertAfterSplit( idx: Int, splitKey: A, left: NodeImpl, right: NodeImpl )
                             ( implicit tx: InTxn ) : BranchImpl = {
            // we must make a copy of this branch with the
            // size increased by one. the new key is `splitKey`
            // which gets inserted at the index where we went
            // down, `idx`.
            val bsz           = size + 1
            val bkeys         = new Array[ A ]( bsz )
            val bdowns        = new Array[ Ref[ NodeImpl ]]( bsz )
            // copy entries left to split index
            if( idx > 0 ) {
               keyCopy(  this, 0, bkeys,  0, idx )
               downCopy( this, 0, bdowns, 0, idx )
            }
            // insert the left split entry
            bkeys( idx )     = splitKey
            bdowns( idx )    = Ref( left )
            // copy entries right to split index
            val rightOff      = idx + 1
            val numr          = bsz - rightOff
            keyCopy( this, idx, bkeys, rightOff, numr )
            // while we could copy the right split entry's key,
            // the split operation has yielded a new right node
            bdowns( rightOff ) = Ref( right )
            if( numr > 1 ) {
               downCopy( this, rightOff, bdowns, rightOff + 1, numr - 1 )
            }

            new BranchImpl( bkeys, bdowns )
         }

         override def toString = toString( "Branch" )
      }

//      private def notSupported = throw new IllegalArgumentException()

//      private sealed trait HeadOrBottom extends NodeImpl {
//         final def asLeaf : Leaf                   = notSupported
//         final def isLeaf : Boolean                = false
//         final def isEmpty                         = false
//         final def asBranch : Branch               = notSupported
//      }

      private object Head extends HeadOrBranch {
         val downNode = Ref[ NodeOrBottom ]( BottomImpl )
//         def key( i: Int ) : A = {
//            assert( i == 0, "Accessing head with index > 0" )
//            maxKey
//         }
//         def down( i: Int )( implicit tx: InTxn ) : NodeOrBottom = {
//            assert( i == 0, "Accessing head with index > 0" )
//            downNode()
//         }
         def updateDown( i: Int, n: NodeImpl )( implicit tx: InTxn ) {
            assert( i == 0, "Accessing head with index > 0" )
            downNode.set( n )
         }
//         val size       = 1
//         val isBottom   = false

         def insertAfterSplit( pidx: Int, splitKey: A, left: NodeImpl, right: NodeImpl )
                             ( implicit tx: InTxn ) : BranchImpl = {
            assert( pidx == 0 )

            val bkeys         = new Array[ A ]( 2 )
            bkeys( 0 )        = splitKey  // left node ends in the split key
            bkeys( 1 )        = maxKey    // right node ends in max key (remember parent is `Head`!)
            val bdowns        = new Array[ Ref[ NodeImpl ]]( 2 )
            bdowns( 0 )       = Ref( left )
            bdowns( 1 )       = Ref( right )
            new BranchImpl( bkeys, bdowns ) // new parent branch
//            downNode.set( b )
//            b
         }

         override def toString = "Head"
      }

//      private object Bottom extends HeadOrBottom {
//         def key( i: Int ) : A = notSupported
//         def down( i: Int )( implicit tx: InTxn ) : NodeImpl = notSupported
//         def down_=( i: Int, n: NodeImpl )( implicit tx: InTxn ) { notSupported }
//         val size = 0
//         val isBottom   = true
////         val isHead     = false
//
//      }

      private object BottomImpl extends Bottom with NodeOrBottom {
         override def toString = "Bottom"
      }
   }
}
sealed trait HASkipList[ @specialized( Int, Long) A ] extends txn.SkipList[ A ] {
   def top( implicit tx: InTxn ) : Child // HASkipList.Node[ A ]

   sealed trait Child   // Either Node or Bottom

   sealed trait Node extends Child {
      def size : Int
      def key( i: Int ): A
   }

   sealed trait Branch extends Node {
      def down( i: Int )( implicit tx: InTxn ) : Node
   }

   sealed trait Leaf extends Child

   sealed trait Bottom extends Child

//   object Bottom extends Child {
//      override def toString = "Bottom"
//   }
}