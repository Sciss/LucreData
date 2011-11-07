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
import concurrent.stm.{Sink, TxnExecutor, Ref, InTxn}

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
         case n: LeafOrBranch => leafSizeSum( n ) - 1
      }

      def maxGap : Int = arrMaxSz - 1  // aka (minGap << 1) + 1
      def maxKeyHolder : MaxKey[ A ] = MaxKey( maxKey )

      def isEmpty( implicit tx: InTxn )   = Head.downNode() eq BottomImpl
      def notEmpty( implicit tx: InTxn )  = !isEmpty

      def toIndexedSeq( implicit tx: InTxn ) : collection.immutable.IndexedSeq[ A ] = iterator.toIndexedSeq
      def toList( implicit tx: InTxn ) : List[ A ] = iterator.toList
      def toSeq( implicit tx: InTxn ) : Seq[ A ] = iterator.toSeq
      def toSet( implicit tx: InTxn ) : Set[ A ] = iterator.toSet

//      private def fillBuilder( b: Builder[ A, _ ])( implicit tx: InTxn ) {
//         val iter = iterator
//         while( iter.hasNext ) b += iter.next()
//      }

      private def leafSizeSum( n: LeafOrBranch )( implicit tx: InTxn ) : Int = {
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

      @inline private def keyCopy( a: LeafOrBranch, aOff: Int, b: Array[ A ], bOff: Int, num: Int ) {
sys.error( "TODO" )
//         System.arraycopy( a.keys, aOff, b, bOff, num )
      }

      @inline private def downCopy( a: BranchImpl, aOff: Int,
                                    b: Array[ Ref[ LeafOrBranch ]], bOff: Int, num: Int )( implicit tx: InTxn ) {
sys.error( "TODO" )
//         var i = 0; while( i < num ) {
//            b( i + bOff ) = Ref( a.down( i + aOff ))
//         i += 1 }
      }

      def height( implicit tx: InTxn ) : Int = {
         @tailrec def step( num: Int, n: LeafOrBranch ) : Int = n match {
            case l: LeafImpl   => num
            case b: BranchImpl => step( num + 1, b.down( 0 ))
         }

         Head.downNode() match {
            case BottomImpl         => 0
            case n: LeafOrBranch    => step( 1, n )
         }
      }

      def top( implicit tx: InTxn ) : Child = Head.downNode()

//      def isomorphicQuery( compare: A => Int ) : A = sys.error( "not yet implemented" )

      // ---- set support ----

      def contains( v: A )( implicit tx: InTxn ) : Boolean = {
         if( ordering.gteq( v, maxKey )) return false

         @tailrec def step( n: LeafOrBranch ) : Boolean = {
            val idx = indexInNode( v, n )
            if( idx < 0 ) true else n match {
               case _: LeafImpl   => false
               case b: BranchImpl => step( b.down( idx ))
            }
         }

         Head.downNode() match {
            case BottomImpl      => false
            case n: LeafOrBranch => step( n )
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
        *         or `-(index+1)` if `v` was found at `index`
       */
      @inline private def indexInNode( v: A, n: NodeLike ) : Int = {
         @tailrec def step( idx: Int ) : Int = {
            val cmp = ordering.compare( v, n.key( idx ))
            if( cmp == 0 ) -(idx + 1) else if( cmp < 0 ) idx else step( idx + 1 )
         }
         step( 0 )
      }

//      /*
//       * Same as `indexInNode`, but doesn't provide hint as to whether the
//       * key was found or not.
//       */
//      @inline private def indexInNode2( v: A, n: NodeLike ) : Int = {
//         @tailrec def step( idx: Int ) : Int = {
//            val cmp = ordering.compare( v, n.key( idx ))
//            if( cmp < 0 ) idx else step( idx + 1 )
//         }
//         step( 0 )
//      }

      override def add( v: A )( implicit tx: InTxn ) : Boolean = {
         require( ordering.lt( v, maxKey ))
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

      private def addLeaf( v: A, pp: HeadOrBranch, ppidx: Int, p: HeadOrBranch, pidx: Int, l: LeafImpl )
                         ( implicit tx: InTxn ) : Boolean = {
         val idx = indexInNode( v, l )
         if( idx < 0 ) return false

         if( l.size == arrMaxSz ) {
            val splitKey   = l.key( minGap )
            val tup        = l.splitAndInsert( v, idx )
            val left       = tup._1
            val right      = tup._2
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
         val idx = indexInNode( v, b )
         if( idx == -1 ) return false

         var bNew    = b
         var idxNew  = idx
         var pNew    = p
         var pidxNew = pidx

         if( b.size == arrMaxSz ) {
            val splitKey   = b.key( minGap )
            val tup        = b.split
            val left       = tup._1
            val right      = tup._2
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
            keyObserver.keyUp( splitKey )
         }

         bNew.down( idxNew ) match {
            case l: LeafImpl    => addLeaf(   v, pNew, pidxNew, bNew, idxNew, l  )
            case bc: BranchImpl => addBranch( v, pNew, pidxNew, bNew, idxNew, bc )
         }
      }

      def +=( elem: A )( implicit tx: InTxn ) : this.type = { add( elem ); this }
      def -=( elem: A )( implicit tx: InTxn ) : this.type = { remove( elem ); this }

      override def remove( v: A )( implicit tx: InTxn ) : Boolean = {
         if( ordering.gteq( v, maxKey )) return false
         Head.downNode() match {
            case l: LeafImpl   => removeLeaf(   v, Head.downNode, l )
            case b: BranchImpl => removeBranch( v, Head.downNode, b )
            case BottomImpl    => false
         }
      }

//      private def removeLeaf( v: A, pp: HeadOrBranch, ppidx: Int, p: HeadOrBranch, pidx: Int, pmod: Boolean,
//                              l: LeafImpl )( implicit tx: InTxn ) : Boolean = {

      private def removeLeaf( v: A, pDown: Sink[ BranchImpl ], l: LeafLike )
                            ( implicit tx: InTxn ) : Boolean = {

         val idx     = indexInNode( v, l )
         val found   = idx < 0
//         val idxP    = if( found ) -(idx + 1) else idx

//         val minSz   = if( found ) arrMinSz + 1 else arrMinSz
//
//         val lmod = l.size == minSz
//
//         if( pmod || lmod ) {
//            val tup  = p.mergeOrBorrow( pp, ppidx, pidx )
//            val pNew = tup._1
////            pidxNew  = tup._2
//            pp.updateDown( ppidx, pNew )
//         }
         if( found ) sys.error( "TODO" )

         found
      }

      private sealed trait ModMaybe
      private case object ModNone   extends ModMaybe
      private sealed trait ModSome  extends ModMaybe

      /*
       * In merge-with-right, the right sibling's
       * identifier is re-used for the merged node.
       * Thus after the merge, the originating sibling
       * should be disposed (when using an ephemeral
       * datastore). The parent needs to remove the
       * entry of the originating sibling.
       *
       * (thus the disposal corresponds with the ref
       * removed from the `downs` array)
       */
      private case object ModMergeRight extends ModSome

      private sealed trait ModBorrowRight extends ModSome
      /*
       * In borrow-from-right, both parents' downs need
       * update, but identifiers are kept.
       * the parent needs to update the key for the
       * originating sibling to match the first key in
       * the right sibling (or the new last key in the
       * originating sibling).
       */
      private case object ModBorrowFromRight extends ModBorrowRight

      /*
       * In merge-with-left, the originating sibling's
       * identifier is re-used for the merged node.
       * Thus after the merge, the left sibling
       * should be disposed (when using an ephemeral
       * datastore). The parent needs to remove the
       * entry of the left sibling.
       *
       * (thus the disposal corresponds with the ref
       * removed from the `downs` array)
       */
      private case object ModMergeLeft extends ModSome

      /*
       * In borrow-from-left, both parents' downs need
       * update, but identifiers are kept.
       * the parent needs to update the key for the
       * left sibling to match the before-last key in
       * the left sibling.
       */
      private case object ModBorrowFromLeft extends ModSome

      /**
       * Borrow-to-right is a special case encountered when
       * going down from a branch containing the search node,
       * where the child cannot be merged to the right.
       * Instead of merging to the left, or borrowing from
       * the left which would keep the query key in the
       * right-most position, causing successive update
       * problems, we hereby achieve that the query key
       * ends up in a position that is not the last in
       * any node: We remove the query key -- the right
       * most entry -- from the originating sibling and
       * prepend it to its right sibling.
       *
       * Like in a normal borrow, both parents' downs need
       * update, but identifiers are kept. The parent needs
       * to update the key for the originating sibling to
       * match the before-last key in the originating sibling
       * (or the new last key in the new originating sibling).
       */
      private case object ModBorrowToRight extends ModBorrowRight

      @tailrec private def removeBranch( v: A, pDown: Sink[ BranchImpl ], b: BranchLike )
                                       ( implicit tx: InTxn ) : Boolean = {
         val idx        = indexInNode( v, b )
         val idxP       = -(idx + 1)
         val c          = b.down( idxP )
         val cIdx       = indexInNode( v, c )
         val cFound     = cIdx < 0
         val cSz        = if( cFound ) c.size - 1 else c.size

         var bNew: BranchImpl = null
         var bDownIdx         = idxP
         var cNew: NodeLike   = c
//         var cMod: ModMaybe   = ModNone
//         var cSib: NodeLike   = null

//         var bDown: Ref[ BranchImpl ] =

         if( cFound || (cSz == arrMinSz) ) {
            val idxP1   = idxP + 1
            if( cFound || (idxP1 < b.size )) {   // merge with or borrow from/to the right
               val cSib       = b.down( idxP1 )
               val cSibSz     = cSib.size
               val mergedSz   = cSz + cSibSz
               if( mergedSz <= arrMaxSz ) {                    // merge with the right
                  // remove the entry at idxP from the branch,
                  // and actualise b with virtual sibling. the key
                  // at bNew's index idxP is now the one formerly at
                  // idxP1, hence the right-most key in csib.
                  bNew        = b.removeColumn( idxP )
//                  bDownIdx    = idxP
                  cNew        = c.virtualize( ModMergeRight, cSib )
               } else if( cSibSz > arrMinSz ) {                // borrow from the right
                  // update the key index idxP of the
                  // originating sibling to match the first key in
                  // the right sibling
                  val upKey   = cSib.key( 0 )
                  bNew        = b.updateKey( idxP, upKey )
//                  bDownIdx    = idxP
                  val bDown1  = b.downRef( idxP1 )
                  bDown1()    = cSib.removeColumn( 0 )
                  cNew        = c.virtualize( ModBorrowFromRight, cSib )
               } else {                                        // borrow to the right
                  assert( cFound )
                  // Like in a normal borrow, both parents' downs need
                  // update, but identifiers are kept. The parent needs
                  // to update the key for the originating sibling to
                  // match the before-last key in the originating sibling
                  // (or the new last key in the new originating sibling).
                  val upKey   = c.key( cSz - 1 )               // note that, since cFound == true, cSz := c.size - 1 !
                  bNew        = b.updateKey( idxP, upKey )
                  bDownIdx    = idxP1     // we borrowed _to_ the right, hence traverse this way!
                  val bDown1  = b.downRef( idxP )
                  bDown1()    = c.removeColumn( cSz )
                  cNew        = c.virtualize( ModBorrowToRight, cSib )
               }

            } else {                                           // merge with or borrow from the left
               val sib  = b.down( idxP - 1 )
               if( sib.size == arrMinSz ) {                    // merge with the left
                  /*
                   * In merge-with-left, the originating sibling's
                   * identifier is re-used for the merged node.
                   * Thus after the merge, the left sibling
                   * should be disposed (when using an ephemeral
                   * datastore). The parent needs to remove the
                   * entry of the left sibling.
                   *
                   * (thus the disposal corresponds with the ref
                   * removed from the `downs` array)
                   */
                  sys.error( "TODO" )
               } else {                                        // borrow from the left
                  /*
                   * In borrow-from-left, both parents' downs need
                   * update, but identifiers are kept.
                   * the parent needs to update the key for the
                   * left sibling to match the before-last key in
                   * the left sibling.
                   */
                  sys.error( "TODO" )
               }
            }
         } else {
            bNew  = b.devirtualize
         }

         if( bNew ne b ) {
            pDown() = bNew
         }

         val bDown = bNew.downRef( bDownIdx )
         cNew match {
            case cl: LeafLike   => removeLeaf(   v, bDown, cl )
            case cb: BranchLike => removeBranch( v, bDown, cb )
         }
      }

      def iterator( implicit tx: InTxn ) : Iterator[ A ] = {
         val i = new IteratorImpl
         i.init()
         i
      }

//      private object EmptyIterator extends Iterator[ A ] {
//         def hasNext( implicit tx: InTxn ) = false
//         override def toString() = "empty iterator"
//         def next()( implicit tx: InTxn ) : A = throw new java.util.NoSuchElementException( "next on " + this )
//      }

      private final class IteratorImpl extends Iterator[ A ] {
         private var l: LeafImpl       = null
         private var nextKey : A       = _
         private var idx: Int          = 0
         private val stack             = collection.mutable.Stack.empty[ (BranchImpl, Int) ]
//         pushDown( 0, Head )

         @tailrec private def pushDown( n: LeafOrBranch, idx0: Int )( implicit tx: InTxn ) {
            n match {
               case l2: LeafImpl =>
                  l        = l2
                  idx      = 0
                  nextKey  = l2.key( 0 )
               case b: BranchImpl =>
                  stack.push( (b, idx0 + 1) )
                  pushDown( b.down( idx0 ), 0 )
            }
         }

         def init()( implicit tx: InTxn ) {
            Head.downNode() match {
               case n: LeafOrBranch =>
                  pushDown( n, 0 )
               case _ =>
            }
         }

         def hasNext : Boolean = ordering.nequiv( nextKey, maxKey )
         def next() : A = {
            if( !hasNext ) throw new java.util.NoSuchElementException( "next on empty iterator" )
            val res  = nextKey
            idx     += 1
            if( idx == l.size || ordering.equiv( l.key( idx ), maxKey )) {
               @tailrec def popUp() {
                  if( stack.isEmpty ) {
                     l        = null
                     nextKey  = maxKey
                  } else {
                     val (b, i) = stack.pop()
                     if( i < b.size ) {
                        TxnExecutor.defaultAtomic( pushDown( b, i )( _ ))
                     } else {
                        popUp()
                     }
                  }
               }
               popUp()
            } else {
               nextKey = l.key( idx )
            }
            res
         }
      }

      private sealed trait NodeOrBottom extends Child

      private sealed trait NodeLike extends Node /* with NodeOrBottom */ {
//         override def down( i: Int )( implicit tx: InTxn ) : NodeLike

//         final def hasMaxSize = size == arrMaxSz
//         final def hasMinSize = size == arrMinSz

//         def size : Int // = keys.size
////         def keys: Array[ A ]
//         def key( i: Int ) : A // = keys( i )

//         final def isEmpty = ordering.equiv( key( 0 ), maxKey )

//         def virtualKey( idx: Int, mod: ModMaybe, sib: NodeLike ) : A = mod match {
//            case ModNone            => key( idx )
//            case ModMergeRight      => val ridx = idx - size; if( ridx < 0 ) key( idx ) else sib.key( ridx )
//            case ModMergeLeft       => val ridx = idx - sib.size; if( ridx < 0 ) sib.key( idx ) else key( ridx )
//            case ModBorrowFromLeft  => if( idx == 0 ) sib.key( sib.size - 1 ) else key( idx - 1 )
//            case _: ModBorrowRight  => if( idx == size ) sib.key( 0 ) else key( idx )
//         }

         def removeColumn( idx: Int ) : LeafOrBranch

//         def asBranch: BranchImpl
//         def asLeaf: LeafImpl

//         protected final def toString( name: String ) : String =
//            keys.toSeq.take( size ).map( k => if( k == maxKey ) "M" else k.toString ).mkString( name + "(", ", ", ")" )
      }

      private sealed trait LeafOrBranch extends NodeLike with NodeOrBottom {
         def virtualize( mod: ModSome, sib: LeafOrBranch ) : NodeLike with VirtualLike
      }

      private sealed trait LeafLike extends NodeLike with Leaf

//      private def virtualize( main: LeafOrBranch, mod: ModSome, sib: LeafOrBranch ) : NodeLike with VirtualLike = (main, sib) match {
//         case (b: BranchImpl, bSib: BranchImpl) => new VirtualBranch( b, mod, bSib )
//         case (l: BranchImpl, lSib: BranchImpl) => new VirtualLeaf(   l, mod, lSib )
//      }

      private sealed trait VirtualLike {
         protected def mod: ModSome
         protected def main: LeafOrBranch
         protected def sib: LeafOrBranch

         def size : Int = main.size + sib.size

         final def key( idx: Int ) : A = mod match {
            case ModMergeRight      => val ridx = idx - main.size; if( ridx < 0 ) main.key( idx ) else sib.key( ridx )
            case ModMergeLeft       => val ridx = idx - sib.size; if( ridx < 0 ) sib.key( idx ) else main.key( ridx )
            case ModBorrowFromLeft  => if( idx == 0 ) sib.key( sib.size - 1 ) else main.key( idx - 1 )
            case _: ModBorrowRight  => if( idx == main.size ) sib.key( 0 ) else main.key( idx )
         }
      }

      private final class VirtualLeaf( protected val main: LeafImpl, protected val mod: ModSome,
                                       protected val sib: LeafImpl ) extends LeafLike with VirtualLike {
         def removeColumn( idx: Int ) : LeafOrBranch = {
            sys.error( "TODO" )
         }
      }

      private final class LeafImpl( keys: Array[ A ])
      extends LeafLike with LeafOrBranch {
//         def down( i: Int )( implicit tx: InTxn )  : NodeLike = Bottom
//         def down_=( i: Int, n: NodeLike )( implicit tx: InTxn ) { notSupported }

         def key( idx: Int ) = keys( idx )
         def size : Int = keys.size

         // XXX we could avoid this crappy pattern match if the branch would have a child type parameter.
         // but then this gets all too pathetic...
         def virtualize( mod: ModSome, sib: LeafOrBranch ) : NodeLike with VirtualLike = sib match {
            case lSib: LeafImpl => new VirtualLeaf( this, mod, lSib )
            case _ => sys.error( "Internal structural error - sibling not a leaf: " + sib )
         }

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

         def removeColumn( idx: Int ) : LeafOrBranch = {
            sys.error( "TODO" )
         }

//         override def toString = toString( "Leaf" )
      }

      private sealed trait HeadOrBranch /* extends Branch */ {
         def updateDown( i: Int, n: LeafOrBranch )( implicit tx: InTxn ) : Unit

         def insertAfterSplit( pidx: Int, splitKey: A, left: LeafOrBranch, right: LeafOrBranch )
                             ( implicit tx: InTxn ) : BranchImpl

//         def mergeOrBorrow( p: HeadOrBranch, pidx: Int, idx: Int )( implicit tx: InTxn ) : (BranchImpl, Int)

//         override def down( i: Int )( implicit tx: InTxn ) : NodeLike
      }

      private sealed trait BranchLike extends NodeLike with /* HeadOrBranch with */ Branch {
         def downRef( idx: Int ) : Ref[ LeafOrBranch ]
         def down( idx: Int )( implicit tx: InTxn ) : LeafOrBranch
//         def removeColumn( idx: Int, mod: ModMaybe, sib: NodeLike ) : BranchImpl = removeColumn( idx, mod, sib.asInstanceOf[ BranchImpl ])
         def updateKey( idx: Int, key: A ) : BranchImpl // BranchLike
         def removeColumn( idx: Int ) : BranchImpl
         def devirtualize : BranchImpl
      }

      private final class VirtualBranch( protected val main: BranchImpl, protected val mod: ModSome,
                                         protected val sib: BranchImpl )
      extends BranchLike with VirtualLike {
         def downRef( idx: Int ) : Ref[ LeafOrBranch ] = mod match {
            case ModMergeRight      => val ridx = idx - main.size; if( ridx < 0 ) main.downRef( idx ) else sib.downRef( ridx )
            case ModMergeLeft       => val ridx = idx - sib.size; if( ridx < 0 ) sib.downRef( idx ) else main.downRef( ridx )
            case ModBorrowFromLeft  => if( idx == 0 ) sib.downRef( sib.size - 1 ) else main.downRef( idx - 1 )
            case _: ModBorrowRight  => if( idx == main.size ) sib.downRef( 0 ) else main.downRef( idx )
         }

         def down( idx: Int )( implicit tx: InTxn ) : LeafOrBranch = downRef( idx )()

         def devirtualize : BranchImpl = sys.error( "TODO" )

         def removeColumn( idx: Int ) : BranchImpl = {
            sys.error( "TODO" )
         }

         def updateKey( idx: Int, key: A ) : BranchImpl = {
            sys.error( "TODO" )
         }
      }

      private final class BranchImpl( keys: Array[ A ], downs: Array[ Ref[ LeafOrBranch ]])
      extends BranchLike with HeadOrBranch with LeafOrBranch {
         assert( keys.size == downs.size )

         def devirtualize : BranchImpl = this

         def virtualize( mod: ModSome, sib: LeafOrBranch ) : NodeLike with VirtualLike = sib match {
            case bSib: BranchImpl => new VirtualBranch( this, mod, bSib )
            case _ => sys.error( "Internal structural error - sibling not a branch: " + sib )
         }

         def key( idx: Int ) : A = keys( idx )
         def size : Int = keys.size

         def downRef( i: Int ) : Ref[ LeafOrBranch ] = downs( i )

         def down( i: Int )( implicit tx: InTxn ) : LeafOrBranch = downs( i )()

         def split( implicit tx: InTxn ) : (BranchImpl, BranchImpl) = {
            val lsz     = arrMinSz
            val lkeys   = new Array[ A ]( lsz )
            val ldowns  = new Array[ Ref[ LeafOrBranch ]]( lsz )
            keyCopy( this, 0, lkeys, 0, lsz )
            downCopy( this, 0, ldowns, 0, lsz )
            val left    = new BranchImpl( lkeys, ldowns )

            val rsz     = size - lsz
            val rkeys   = new Array[ A ]( rsz )
            val rdowns  = new Array[ Ref[ LeafOrBranch ]]( rsz )
            keyCopy( this, lsz, rkeys, 0, rsz )
            downCopy( this, lsz, rdowns, 0, rsz )
            val right   = new BranchImpl( rkeys, rdowns )

            (left, right)
         }

//         def virtualSize( mod: ModMaybe, sib: BranchImpl ) : Int = {
//            if( sib == null ) size else size + sib.size
//         }

         def updateDown( i: Int, n: LeafOrBranch )( implicit tx: InTxn ) {
            downs( i ).set( n )
         }

         def removeColumn( idx: Int ) : BranchImpl  = {
            sys.error( "TODO" )
         }

         def updateKey( idx: Int, key: A ) : BranchImpl = {
            sys.error( "TODO" )
         }

         def insertAfterSplit( idx: Int, splitKey: A, left: LeafOrBranch, right: LeafOrBranch )
                             ( implicit tx: InTxn ) : BranchImpl = {
            // we must make a copy of this branch with the
            // size increased by one. the new key is `splitKey`
            // which gets inserted at the index where we went
            // down, `idx`.
            val bsz           = size + 1
            val bkeys         = new Array[ A ]( bsz )
            val bdowns        = new Array[ Ref[ LeafOrBranch ]]( bsz )
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

//         override def toString = toString( "Branch" )
      }

//      private def notSupported = throw new IllegalArgumentException()

//      private sealed trait HeadOrBottom extends NodeLike {
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
         def updateDown( i: Int, n: LeafOrBranch )( implicit tx: InTxn ) {
            assert( i == 0, "Accessing head with index > 0" )
            downNode.set( n )
         }
//         val size       = 1
//         val isBottom   = false

         def insertAfterSplit( pidx: Int, splitKey: A, left: LeafOrBranch, right: LeafOrBranch )
                             ( implicit tx: InTxn ) : BranchImpl = {
            assert( pidx == 0 )

            val bkeys         = new Array[ A ]( 2 )
            bkeys( 0 )        = splitKey  // left node ends in the split key
            bkeys( 1 )        = maxKey    // right node ends in max key (remember parent is `Head`!)
            val bdowns        = new Array[ Ref[ LeafOrBranch ]]( 2 )
            bdowns( 0 )       = Ref( left )
            bdowns( 1 )       = Ref( right )
            new BranchImpl( bkeys, bdowns ) // new parent branch
//            downNode.set( b )
//            b
         }

//         def mergeOrBorrow( p: HeadOrBranch, pidx: Int, idx: Int )( implicit tx: InTxn ) : (BranchImpl, Int) = {
//            sys.error( "TODO" )
//         }

         override def toString = "Head"
      }

//      private object Bottom extends HeadOrBottom {
//         def key( i: Int ) : A = notSupported
//         def down( i: Int )( implicit tx: InTxn ) : NodeLike = notSupported
//         def down_=( i: Int, n: NodeLike )( implicit tx: InTxn ) { notSupported }
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

   sealed trait Leaf extends Node

   sealed trait Bottom extends Child

//   object Bottom extends Child {
//      override def toString = "Bottom"
//   }
}