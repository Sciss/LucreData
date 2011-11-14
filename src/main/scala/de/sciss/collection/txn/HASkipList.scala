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

import collection.mutable.Builder
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.io.{ObjectOutputStream, ObjectInputStream}
import annotation.{switch, tailrec}
import de.sciss.lucrestm.{DataOutput, DataInput, Sink, Serializer, Sys}

/**
 * A transactional version of the deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size).
 * It uses a modified top-down removal algorithm that avoids the need for a second
 * pass as in the original algorithm, and is careful about object creations, so that
 * it will be able to persist the data structure without any unnecessary reads or
 * writes to the store.
 */
object HASkipList {
   /**
    * Creates a new empty skip list with default minimum gap parameter of `2` and no key observer.
    * Type parameter `S` specifies the STM system to use. Type parameter `A`
    * specifies the type of the keys stored in the list.
    *
    * @param   ord         the ordering of the keys. This is an instance of `de.sciss.collection.Ordering` to allow
    *                      for specialized versions.
    * @param   maxKey      the maximum key for the list. No element added can be equal or greater to this key.
    * @param   mf          the manifest for key type `A`, necessary for internal array constructions.
    * @param   serKey      the serializer for the elements, in case a persistent STM is used.
    * @param   stm         the software transactional memory to use.
    */
   def empty[ S <: Sys[ S ], A ]( implicit ord: de.sciss.collection.Ordering[ A ], maxKey: MaxKey[ A ],
                                  mf: Manifest[ A ], serKey: Serializer[ A ], stm: S ) : HASkipList[ S, A ] = empty()

   /**
    * Creates a new empty skip list. Type parameter `S` specifies the STM system to use. Type parameter `A`
    * specifies the type of the keys stored in the list.
    *
    * @param   minGap      the minimum gap-size used for the skip list. This value must be between 1 and 126 inclusive.
    * @param   keyObserver an object which observes key promotions and demotions. Use `NoKeyObserver` (default) if
    *                      key motions do not need to be monitored. The monitoring allows the use of the skip list
    *                      for synchronized decimations of related data structures, such as the deterministic
    *                      skip quadtree.
    * @param   ord         the ordering of the keys. This is an instance of `de.sciss.collection.Ordering` to allow
    *                      for specialized versions.
    * @param   maxKey      the maximum key for the list. No element added can be equal or greater to this key.
    * @param   mf          the manifest for key type `A`, necessary for internal array constructions.
    * @param   serKey      the serializer for the elements, in case a persistent STM is used.
    * @param   stm         the software transactional memory to use.
    */
   def empty[ S <: Sys[ S ], A ]( minGap: Int = 2,
                                  keyObserver: txn.SkipList.KeyObserver[ S, A ] = txn.SkipList.NoKeyObserver[ S, A ])
                                ( implicit ord: de.sciss.collection.Ordering[ A ], maxKey: MaxKey[ A ],
                                  mf: Manifest[ A ], serKey: Serializer[ A ], stm: S ) : HASkipList[ S, A ] = {

      // 255 <= arrMaxSz = (minGap + 1) << 1
      // ; this is, so we can write a node's size as signed byte, and
      // no reasonable app would use a node size > 255
      require( minGap >= 1 && minGap <= 126, "Minimum gap (" + minGap + ") cannot be less than 1 or greater than 126" )

      new Impl[ S, A ]( maxKey.value, minGap, keyObserver )
   }

   private final class Impl[ S <: Sys[ S ], /* @specialized( Int, Long ) */ A ]
      ( val maxKey: A, val minGap: Int, keyObserver: txn.SkipList.KeyObserver[ S, A ])
      ( implicit mf: Manifest[ A ], val ordering: de.sciss.collection.Ordering[ A ], serKey: Serializer[ A ],
        val system: S )
   extends HASkipList[ S, A ] {
      private val arrMinSz = minGap + 1
      private val arrMaxSz = arrMinSz << 1   // aka maxGap + 1

      override def size( implicit tx: S#Tx ) : Int = topImpl match {
         case BottomImpl  => 0
         case n: LeafOrBranch => leafSizeSum( n ) - 1
      }

      def maxGap : Int = arrMaxSz - 1  // aka (minGap << 1) + 1
      def maxKeyHolder : MaxKey[ A ] = MaxKey( maxKey )

      def isEmpty( implicit tx: S#Tx )   = top eq BottomImpl
      def notEmpty( implicit tx: S#Tx )  = !isEmpty

      def toIndexedSeq( implicit tx: S#Tx ) : IIdxSeq[ A ] = fillBuilder( IIdxSeq.newBuilder[ A ])
      def toList( implicit tx: S#Tx ) : List[ A ] = fillBuilder( List.newBuilder[ A ])
      def toSeq(  implicit tx: S#Tx ) : Seq[  A ] = fillBuilder( Seq.newBuilder[  A ])
      def toSet(  implicit tx: S#Tx ) : Set[  A ] = fillBuilder( Set.newBuilder[  A ])

      private def leafSizeSum( n: LeafOrBranch )( implicit tx: S#Tx ) : Int = {
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

      def height( implicit tx: S#Tx ) : Int = {
         @tailrec def step( num: Int, n: LeafOrBranch ) : Int = n match {
            case l: LeafImpl   => num
            case b: BranchImpl => step( num + 1, b.down( 0 ))
         }

         topImpl match {
            case BottomImpl         => 0
            case n: LeafOrBranch    => step( 1, n )
         }
      }

      def top( implicit tx: S#Tx ) : Child = topImpl
      private def topImpl( implicit tx: S#Tx ) : NodeOrBottom = Head.downNode.get

      def debugPrint( implicit tx: S#Tx ) : String = {
         def printNode( n: LeafOrBranch ) : IndexedSeq[ String ] = {
            n match {
               case l: LeafImpl   =>
                  val keys = Seq.tabulate( n.size ) { idx =>
                     val k = n.key( idx )
                     if( k == maxKey ) "M" else k.toString
                  }
                  IndexedSeq( keys.mkString( "--" ))
               case b: BranchImpl =>
                  val columns = IndexedSeq.tabulate( b.size ) { idx =>
                     val child   = printNode( b.down( idx ))
                     val childSz = child.head.length()
                     val k       = b.key( idx )
                     val key     = if( k == maxKey ) "M" else k.toString
                     val keySz   = key.length()
                     val colSz   = math.max( keySz, childSz ) + 2
                     val keyAdd  = (if( idx == b.size - 1 ) " " else "-") * (colSz - keySz)
                     val bar     = "|" + (" " * (colSz - 1))
                     val childAdd= " " * (colSz - childSz)
                     IndexedSeq( key + keyAdd, bar ) ++ child.map( _ + childAdd )
                  }
//                  val red = columns.reduceLeft { (b, column) =>
//                     b.zip( column ).map { case (left, right) => left + right }
//                  }
//                  red // .map( _ + "  " )
                  IndexedSeq.tabulate( columns.map( _.size ).max ) { row =>
                     columns.map( _.apply( row )).mkString( "" )
                  }
            }
         }
         topImpl match {
            case BottomImpl      => "M"
            case n: LeafOrBranch => printNode( n ).mkString( "\n" )
         }
      }

      // ---- set support ----

      def contains( v: A )( implicit tx: S#Tx ) : Boolean = {
         if( ordering.gteq( v, maxKey )) return false

         @tailrec def step( n: LeafOrBranch ) : Boolean = {
            val idx = indexInNode( v, n )
            if( idx < 0 ) true else n match {
               case _: LeafImpl   => false
               case b: BranchImpl => step( b.down( idx ))
            }
         }

         topImpl match {
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

      override def add( v: A )( implicit tx: S#Tx ) : Boolean = {
         require( ordering.lt( v, maxKey ))
         topImpl match {
            case BottomImpl =>
               val lkeys         = new Array[ A ]( 2 )
               lkeys( 0 )        = v
               lkeys( 1 )        = maxKey
               val l             = new LeafImpl( lkeys )
               Head.downNode.set( l )
               true

            case l: LeafImpl   => addLeaf(   v, Head, 0, Head, 0, l )
            case b: BranchImpl => addBranch( v, Head, 0, Head, 0, b )
         }
      }

      private def addLeaf( v: A, pp: HeadOrBranch, ppidx: Int, p: HeadOrBranch, pidx: Int, l: LeafImpl )
                         ( implicit tx: S#Tx ) : Boolean = {
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
                                    ( implicit tx: S#Tx ) : Boolean = {
         val idx = indexInNode( v, b )
         if( idx < 0 ) return false

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

      def +=( elem: A )( implicit tx: S#Tx ) : this.type = { add( elem ); this }
      def -=( elem: A )( implicit tx: S#Tx ) : this.type = { remove( elem ); this }

      override def remove( v: A )( implicit tx: S#Tx ) : Boolean = {
         if( ordering.gteq( v, maxKey )) return false
         topImpl match {
            case l: LeafImpl =>
               removeLeaf(   v, Head.downNode, l )
            case b: BranchImpl =>
               removeBranch( v, Head.downNode, b )
            case BottomImpl =>
               false
         }
      }

      private sealed trait ModVirtual  // extends ModMaybe

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
      private case object ModMergeRight extends ModVirtual

      /*
       * In borrow-from-right, both parents' downs need
       * update, but identifiers are kept.
       * the parent needs to update the key for the
       * originating sibling to match the first key in
       * the right sibling (or the new last key in the
       * originating sibling).
       */
      private case object ModBorrowFromRight extends ModVirtual

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
      private case object ModMergeLeft extends ModVirtual

      /*
       * In borrow-from-left, both parents' downs need
       * update, but identifiers are kept.
       * the parent needs to update the key for the
       * left sibling to match the before-last key in
       * the left sibling.
       */
      private case object ModBorrowFromLeft extends ModVirtual

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
      private case object ModBorrowToRight extends ModVirtual

      private def removeLeaf( v: A, pDown: Sink[ S#Tx, LeafOrBranch ], l: LeafLike )
                            ( implicit tx: S#Tx ) : Boolean = {

         val idx     = indexInNode( v, l )
         val found   = idx < 0
         val lNew    = if( found ) {
            val idxP = -(idx + 1)
            l.removeColumn( idxP )
         } else {
            l.devirtualize
         }
         if( lNew ne l ) {
            pDown.set( lNew )
         }
         found
      }

      @tailrec private def removeBranch( v: A, pDown: Sink[ S#Tx, LeafOrBranch ], b: BranchLike )
                                       ( implicit tx: S#Tx ) : Boolean = {
         val idx        = indexInNode( v, b )
         val found      = idx < 0
         val idxP       = if( found ) -(idx + 1) else idx
         val c          = b.down( idxP )
         val cIdx       = indexInNode( v, c )
         val cFound     = cIdx < 0
         val cSz        = if( cFound ) c.size - 1 else c.size
         // if the key was found in the last element of the child,
         // (-(cIdx+1) == cSz), this implies that it was found in
         // the current branch, thus `found` will be true.
//         val cFoundLast = found || (-(cIdx+1) == cSz)

         var bNew: BranchImpl = null
         var bDownIdx         = idxP
         var cNew: NodeLike   = c

         // a merge or borrow is necesary either when we descend
         // to a minimally filled child (because that child might
         // need to shrink in the next step), or when the key was
         // found in the current branch, because then the key will
         // appear as the last element of the child -- and we
         // prevent further problems with an early borrow-to-right
         // if necessary.
         // ; note that `cSz` can be `< arrMinSz` due to a found key already being accounted for (`c.size - 1`)!
         if( found /* cFound */ || (cSz <= arrMinSz) ) {
            val idxP1      = idxP + 1
            val bHasRight  = idxP1 < b.size
            if( bHasRight ) {                                  // merge with or borrow from/to the right
               val cSib       = b.down( idxP1 )
               val cSibSz     = cSib.size
               val mergedSz   = cSz + cSibSz

               val downKey    = b.key( idxP )
               keyObserver.keyDown( downKey )

               if( mergedSz <= arrMaxSz ) {                    // merge with the right
                  // remove the entry at idxP from the branch,
                  // and actualise b with virtual sibling. the key
                  // at bNew's index idxP is now the one formerly at
                  // idxP1, hence the right-most key in csib.
                  bNew        = b.removeColumn( idxP )
                  system.disposeRef( b.downRef( idxP ))
//                  bDownIdx    = idxP
                  cNew        = c.virtualize( ModMergeRight, cSib )
               } else if( cSibSz > arrMinSz ) {                // borrow from the right
                  // update the key index idxP of the
                  // originating sibling to match the first key in
                  // the right sibling
                  val upKey   = cSib.key( 0 )
                  bNew        = b.updateKey( idxP, upKey )
                  keyObserver.keyUp( upKey )
//                  bDownIdx    = idxP
                  val bDown1  = b.downRef( idxP1 )
                  bDown1.set( cSib.removeColumn( 0 ))
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
                  keyObserver.keyUp( upKey )
                  bDownIdx    = idxP1     // we borrowed _to_ the right, hence traverse this way!
                  val bDown1  = b.downRef( idxP )
                  bDown1.set( c.removeColumn( cSz ))
                  cNew        = c.virtualize( ModBorrowToRight, cSib )
               }

            } else {                                           // merge with or borrow from the left
               // it implies that if cFound is true, cIdx is < c.size - 1
               // that is, the key is not in the last element of c
               // (because otherwise, b would have already in its
               // virtualization be merged to or have borrowed from its right sibling)

               val idxPM1  = idxP - 1
               val cSib    = b.down( idxPM1 )
               val cSibSz  = cSib.size

               val downKey    = b.key( idxPM1 )
               keyObserver.keyDown( downKey )

               if( cSibSz == arrMinSz ) {                      // merge with the left
                  // The parent needs to remove the
                  // entry of the left sibling.
                  bNew        = b.removeColumn( idxPM1 )
                  system.disposeRef( b.downRef( idxPM1 ))
                  bDownIdx    = idxPM1
                  cNew        = c.virtualize( ModMergeLeft, cSib )
               } else {                                        // borrow from the left
                  // the parent needs to update the key for the
                  // left sibling to match the before-last key in
                  // the left sibling.
                  val upKey   = cSib.key( cSibSz - 2 )
                  bNew        = b.updateKey( idxPM1, upKey )
                  keyObserver.keyUp( upKey )
//                  bDownIdx    = idxP
                  val bDown1  = b.downRef( idxPM1 )
                  bDown1.set( cSib.removeColumn( cSibSz - 1 ))
                  cNew        = c.virtualize( ModBorrowFromLeft, cSib )
               }
            }
         } else {
            bNew  = b.devirtualize
         }

         val bDown = if( bNew ne b ) { // branch changed
            if( bNew.size > 1 ) {
               pDown.set( bNew ) // update down ref from which it came
               bNew.downRef( bDownIdx )
            } else {
               // unfortunately we do not have `p`
//               assert( p == Head )
               pDown
            }
         } else {
            bNew.downRef( bDownIdx )
         }

//         val bDown = bNew.downRef( bDownIdx )
         cNew match {
            case cl: LeafLike   => removeLeaf(   v, bDown, cl )
            case cb: BranchLike => removeBranch( v, bDown, cb )
         }
      }

      def iterator( implicit tx: S#Tx ) : Iterator[ A ] = {
         val i = new IteratorImpl
         i.init()
         i
      }

      private def fillBuilder[ Res ]( b: Builder[ A, Res ])( implicit tx: S#Tx ) : Res = {
         val i = new IteratorImpl
         i.init()
         while( i.hasNext ) {
            b += i.nextTxn
         }
         b.result()
      }

//      private object EmptyIterator extends Iterator[ A ] {
//         def hasNext( implicit tx: S#Tx ) = false
//         override def toString() = "empty iterator"
//         def next()( implicit tx: S#Tx ) : A = throw new java.util.NoSuchElementException( "next on " + this )
//      }

      private final class IteratorImpl extends Iterator[ A ] {
         private var l: LeafImpl       = null
         private var nextKey : A       = _
         private var idx: Int          = 0
         private val stack             = collection.mutable.Stack.empty[ (BranchImpl, Int) ]
//         pushDown( 0, Head )

         @tailrec private def pushDown( n: LeafOrBranch, idx0: Int )( implicit tx: S#Tx ) {
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

         def init()( implicit tx: S#Tx ) {
            topImpl match {
               case n: LeafOrBranch =>
                  pushDown( n, 0 )
               case _ =>
            }
         }

         def hasNext : Boolean = ordering.nequiv( nextKey, maxKey )
         def next() : A = system.atomic( nextTxn( _ ))

         def nextTxn( implicit tx: S#Tx ) : A = {
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
                        system.atomic( pushDown( b, i )( _ ))
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

      private object NodeOrBottom {
         implicit object Serializer extends Serializer[ NodeOrBottom ] {
            def write( v: NodeOrBottom, out: DataOutput ) { v.write( out )}
            def read( in: DataInput ) : NodeOrBottom = {
               (in.readUnsignedByte(): @switch) match {
                  case 0 => BottomImpl
                  case 1 => BranchImpl.read( in )
                  case 2 => LeafImpl.read( in )
               }
            }
         }
      }
      private sealed trait NodeOrBottom extends Child {
         def write( out: DataOutput ) : Unit
      }

      private sealed trait NodeLike extends Node /* with NodeOrBottom */ {
         def removeColumn( idx: Int ) : LeafOrBranch
      }

      private object LeafOrBranch {
         implicit object Ser extends Serializer[ LeafOrBranch ] {
            def write( v: LeafOrBranch, out: DataOutput ) { v.write( out )}
            def read( in: DataInput ) : LeafOrBranch = {
               (in.readUnsignedByte(): @switch) match {
                  case 1 => BranchImpl.read( in )
                  case 2 => LeafImpl.read( in )
               }
            }
         }
      }
      private sealed trait LeafOrBranch extends NodeLike with NodeOrBottom {
         def virtualize( mod: ModVirtual, sib: LeafOrBranch ) : NodeLike with VirtualLike
      }

      private sealed trait LeafLike extends NodeLike with Leaf {
         def devirtualize : LeafImpl
         def removeColumn( idx: Int ) : LeafImpl
      }

      private sealed trait VirtualLike {
         protected def mod:  ModVirtual
         protected def main: LeafOrBranch
         protected def sib:  LeafOrBranch

         def size : Int = mod match {
            case ModMergeRight      => main.size + sib.size
            case ModBorrowFromRight => main.size + 1
            case ModBorrowToRight   => sib.size + 1
            case ModMergeLeft       => main.size + sib.size
            case ModBorrowFromLeft  => main.size + 1
         }

         final def key( idx: Int ) : A = mod match {
            case ModMergeRight      => val ridx = idx - main.size; if( ridx < 0 ) main.key( idx ) else sib.key( ridx )
            case ModBorrowFromRight => if( idx == main.size ) sib.key( 0 ) else main.key( idx )
            case ModBorrowToRight   => if( idx == 0 ) main.key( main.size - 1 ) else sib.key( idx - 1 )
            case ModMergeLeft       => val ridx = idx - sib.size; if( ridx < 0 ) sib.key( idx ) else main.key( ridx )
            case ModBorrowFromLeft  => if( idx == 0 ) sib.key( sib.size - 1 ) else main.key( idx - 1 )
         }
      }

      private final class VirtualLeaf( protected val main: LeafImpl, protected val mod: ModVirtual,
                                       protected val sib: LeafImpl ) extends LeafLike with VirtualLike {
         def removeColumn( idx: Int ) : LeafImpl = {
            val newSz   = size - 1
            val keys    = new Array[ A ]( newSz )
            var i = 0
            while( i < idx ) {
               keys( i ) = key( i )
               i += 1
            }
            while( i < newSz ) {
               val i1 = i + 1
               keys( i ) = key( i1 )
               i = i1
            }
            new LeafImpl( keys )
         }

         def devirtualize: LeafImpl = {
            val newSz   = size
            val keys    = new Array[ A ]( newSz )
            var i = 0
            while( i < newSz ) {
               keys( i ) = key( i )
               i += 1
            }
            new LeafImpl( keys )
         }
      }

      private object LeafImpl {
         def read( in: DataInput ) : LeafImpl = {
            val sz: Int = in.readUnsignedByte()
            val keys = new Array[ A ]( sz )
            var i = 0; while( i < sz ) {
               keys( i ) = serKey.read( in )
            i += 1 }
            new LeafImpl( keys )
         }
      }
      private final class LeafImpl( keys: Array[ A ])
      extends LeafLike with LeafOrBranch {
         def key( idx: Int ) = keys( idx )
         def size : Int = keys.size

         def devirtualize: LeafImpl = this

         // XXX we could avoid this crappy pattern match if the branch would have a child type parameter.
         // but then this gets all too pathetic...
         def virtualize( mod: ModVirtual, sib: LeafOrBranch ) : NodeLike with VirtualLike = sib match {
            case lSib: LeafImpl => new VirtualLeaf( this, mod, lSib )
            case _ => sys.error( "Internal structural error - sibling not a leaf: " + sib )
         }

         def insert( v: A, idx: Int ) : LeafImpl = {
            val lsz     = size + 1
            val lkeys   = new Array[ A ]( lsz )
            // copy keys left to the insertion index
            if( idx > 0 ) {
//               keyCopy( this, 0, lkeys, 0, idx )
               System.arraycopy( keys, 0, lkeys, 0, idx )
            }
            // put the new value
            lkeys( idx ) = v
            // copy the keys right to the insertion index
            val idxp1   = idx + 1
            val numr    = lsz - idxp1
            if( numr > 0 ) {
//               keyCopy( this, idx, lkeys, idxp1, numr )
               System.arraycopy( keys, idx, lkeys, idxp1, numr )
            }
            new LeafImpl( lkeys )
         }

         def splitAndInsert( v: A, idx: Int ) : (LeafImpl, LeafImpl) = {
            assert( size == arrMaxSz )

            if( idx < arrMinSz ) {  // split and add `v` to left leaf
               val lsz     = arrMinSz + 1
               val lkeys   = new Array[ A ]( lsz )
               if( idx > 0 ) {
                  System.arraycopy( keys, 0, lkeys, 0, idx )
               }
               lkeys( idx ) = v
               val numr    = arrMinSz - idx
               if( numr > 0 ) {
                  System.arraycopy( keys, idx, lkeys, idx + 1, numr )
               }
               val left    = new LeafImpl( lkeys )

               val rsz     = arrMinSz
               val rkeys   = new Array[ A ]( rsz )
               System.arraycopy( keys, arrMinSz, rkeys, 0, rsz )
               val right   = new LeafImpl( rkeys )

               (left, right)

            } else {               // split and add `v` to right leaf
               val lsz     = arrMinSz
               val lkeys   = new Array[ A ]( lsz )
               System.arraycopy( keys, 0, lkeys, 0, lsz )
               val left    = new LeafImpl( lkeys )

               val rsz     = arrMinSz + 1
               val rkeys   = new Array[ A ]( rsz )
               val numl    = idx - arrMinSz
               if( numl > 0 ) {
                  System.arraycopy( keys, arrMinSz, rkeys, 0, numl )
               }
               rkeys( numl ) = v
               val numr    = arrMinSz - numl
               if( numr > 0 ) {
                  System.arraycopy( keys, idx, rkeys, numl + 1, numr )
               }
               val right   = new LeafImpl( rkeys )

               (left, right)
            }
         }

         def removeColumn( idx: Int ) : LeafImpl = {
            val sz      = size - 1
            val newKeys = new Array[ A ]( sz )
            if( idx > 0 ) System.arraycopy( keys, 0, newKeys, 0, idx )
            val numr    = sz - idx
            if( numr > 0 ) System.arraycopy( keys, idx + 1, newKeys, idx, numr )
            new LeafImpl( newKeys )
         }

//         override def toString = toString( "Leaf" )

         def write( out: DataOutput ) {
            val sz = size
//            out.writeUnsignedShort( 0x200 | sz )
            out.writeUnsignedByte( 2 )
            out.writeUnsignedByte( sz )
            var i = 0; while( i < sz ) {
               serKey.write( keys( i ), out )
            i += 1 }
         }
      }

      private sealed trait HeadOrBranch /* extends Branch */ {
         def updateDown( i: Int, n: LeafOrBranch )( implicit tx: S#Tx ) : Unit

         def insertAfterSplit( pidx: Int, splitKey: A, left: LeafOrBranch, right: LeafOrBranch )
                             ( implicit tx: S#Tx ) : BranchImpl
      }

      private sealed trait BranchLike extends NodeLike with /* HeadOrBranch with */ Branch {
         def downRef( idx: Int ) : S#Ref[ LeafOrBranch ]
         def down( idx: Int )( implicit tx: S#Tx ) : LeafOrBranch
         def updateKey( idx: Int, key: A ) : BranchImpl // BranchLike
         def removeColumn( idx: Int ) : BranchImpl
         def devirtualize : BranchImpl
      }

      private final class VirtualBranch( protected val main: BranchImpl, protected val mod: ModVirtual,
                                         protected val sib: BranchImpl )
      extends BranchLike with VirtualLike {
         def downRef( idx: Int ) : S#Ref[ LeafOrBranch ] = mod match {
            case ModMergeRight      => val ridx = idx - main.size; if( ridx < 0 ) main.downRef( idx ) else sib.downRef( ridx )
            case ModBorrowFromRight => if( idx == main.size ) sib.downRef( 0 ) else main.downRef( idx )
            case ModBorrowToRight   => if( idx == 0 ) main.downRef( main.size - 1 ) else sib.downRef( idx - 1 )
            case ModMergeLeft       => // val ridx = idx - sib.size; if( ridx < 0 ) sib.downRef( idx ) else main.downRef( ridx )
               val ridx = idx - sib.size
               if( ridx < 0 ) {
                  sib.downRef( idx )
               } else {
assert( ridx < main.size, "HALLO ridx = " + ridx + " sib.size = " + sib.size + " ; main.size = " + main.size )
                  main.downRef( ridx )
               }
            case ModBorrowFromLeft  => if( idx == 0 ) sib.downRef( sib.size - 1 ) else main.downRef( idx - 1 )
         }

         def down( idx: Int )( implicit tx: S#Tx ) : LeafOrBranch = downRef( idx ).get

         def devirtualize : BranchImpl = {
            val newSz   = size
            val keys    = new Array[ A ]( newSz )
            val downs   = system.newRefArray[ LeafOrBranch ]( newSz ) // new Array[ S#Ref[ LeafOrBranch ]]( newSz )
            var i = 0
            while( i < newSz ) {
               keys( i )  = key( i )
               downs( i ) = downRef( i )
               i += 1
            }
            new BranchImpl( keys, downs )
         }

         def removeColumn( idx: Int ) : BranchImpl = {
            val newSz   = size - 1
            val keys    = new Array[ A ]( newSz )
            val downs   = system.newRefArray[ LeafOrBranch ]( newSz ) // new Array[ S#Ref[ LeafOrBranch ]]( newSz )
            var i = 0
            while( i < idx ) {
               keys( i )   = key( i )
               downs( i )  = downRef( i )
               i += 1
            }
            while( i < newSz ) {
               val i1 = i + 1
               keys( i )   = key( i1 )
               downs( i )  = downRef( i1 )
               i = i1
            }
            new BranchImpl( keys, downs )
         }

         def updateKey( idx: Int, k: A ) : BranchImpl = {
            val newSz   = size
            val keys    = new Array[ A ]( newSz )
            val downs   = system.newRefArray[ LeafOrBranch ]( newSz ) // new Array[ S#Ref[ LeafOrBranch ]]( newSz )
            var i = 0
            while( i < newSz ) {
               keys( i )   = key( i )
               downs( i )  = downRef( i )
               i += 1
            }
            keys( idx ) = k
            new BranchImpl( keys, downs )
         }
      }

      private object BranchImpl {
         def read( in: DataInput ) : BranchImpl = {
            val sz: Int = in.readUnsignedByte()
            val keys    = new Array[ A ]( sz )
            val downs   = system.newRefArray[ LeafOrBranch ]( sz )
            var i = 0; while( i < sz ) {
               keys( i ) = serKey.read( in )
            i += 1 }
            i = 0; while( i < sz ) {
               downs( i ) = system.readRef[ LeafOrBranch ]( in )
            i += 1 }
            new BranchImpl( keys, downs )
         }
      }
      private final class BranchImpl( keys: Array[ A ], downs: Array[ S#Ref[ LeafOrBranch ]])
      extends BranchLike with HeadOrBranch with LeafOrBranch {
         assert( keys.size == downs.size )

         def devirtualize : BranchImpl = this

         def virtualize( mod: ModVirtual, sib: LeafOrBranch ) : NodeLike with VirtualLike = sib match {
            case bSib: BranchImpl => new VirtualBranch( this, mod, bSib )
            case _ => sys.error( "Internal structural error - sibling not a branch: " + sib )
         }

         def key( idx: Int ) : A = keys( idx )
         def size : Int = keys.size

         def downRef( i: Int ) : S#Ref[ LeafOrBranch ] = downs( i )

         def down( i: Int )( implicit tx: S#Tx ) : LeafOrBranch = downs( i ).get

         def split( implicit tx: S#Tx ) : (BranchImpl, BranchImpl) = {
            val lsz     = arrMinSz
            val lkeys   = new Array[ A ]( lsz )
            val ldowns  = system.newRefArray[ LeafOrBranch ]( lsz ) // new Array[ S#Ref[ LeafOrBranch ]]( lsz )
            System.arraycopy( keys,  0, lkeys,  0, lsz )
            System.arraycopy( downs, 0, ldowns, 0, lsz )
            val left    = new BranchImpl( lkeys, ldowns )

            val rsz     = size - lsz
            val rkeys   = new Array[ A ]( rsz )
            val rdowns  = system.newRefArray[ LeafOrBranch ]( rsz ) // new Array[ S#Ref[ LeafOrBranch ]]( rsz )
            System.arraycopy( keys,  lsz, rkeys,  0, rsz )
            System.arraycopy( downs, lsz, rdowns, 0, rsz )
            val right   = new BranchImpl( rkeys, rdowns )

            (left, right)
         }

         def updateDown( i: Int, n: LeafOrBranch )( implicit tx: S#Tx ) {
            downs( i ).set( n )
         }

         def removeColumn( idx: Int ) : BranchImpl  = {
assert( idx >= 0 && idx < size, "idx = " + idx + "; size = " + size )
            val sz         = size - 1
            val newKeys    = new Array[ A ]( sz )
            val newDowns   = system.newRefArray[ LeafOrBranch ]( sz ) // new Array[ S#Ref[ LeafOrBranch ]]( sz )
            if( idx > 0 ) {
               System.arraycopy( keys,  0, newKeys,  0, idx )
               System.arraycopy( downs, 0, newDowns, 0, idx )
            }
            val numr    = sz - idx
            if( numr > 0 ) {
               System.arraycopy( keys,  idx + 1, newKeys,  idx, numr )
               System.arraycopy( downs, idx + 1, newDowns, idx, numr )
            }
            new BranchImpl( newKeys, newDowns )
         }

         def updateKey( idx: Int, key: A ) : BranchImpl = {
            val sz         = size
            val newKeys    = new Array[ A ]( sz )
            System.arraycopy( keys, 0, newKeys, 0, sz )  // just copy all and then overwrite one
            newKeys( idx ) = key
            new BranchImpl( newKeys, downs )
         }

         def insertAfterSplit( idx: Int, splitKey: A, left: LeafOrBranch, right: LeafOrBranch )
                             ( implicit tx: S#Tx ) : BranchImpl = {
            // we must make a copy of this branch with the
            // size increased by one. the new key is `splitKey`
            // which gets inserted at the index where we went
            // down, `idx`.
            val bsz           = size + 1
            val bkeys         = new Array[ A ]( bsz )
            val bdowns        = system.newRefArray[ LeafOrBranch ]( bsz ) // new Array[ S#Ref[ LeafOrBranch ]]( bsz )
            // copy entries left to split index
            if( idx > 0 ) {
               System.arraycopy( keys,  0, bkeys,  0, idx )
               System.arraycopy( downs, 0, bdowns, 0, idx )
            }
            // insert the left split entry
            bkeys( idx )     = splitKey
            bdowns( idx )    = system.newRef( left )
            // copy entries right to split index
            val rightOff      = idx + 1
            val numr          = bsz - rightOff
            System.arraycopy( keys, idx, bkeys, rightOff, numr )
//            // while we could copy the right split entry's key,
//            // the split operation has yielded a new right node
//            bdowns( rightOff ) = system.newRef( right )
//            if( numr > 1 ) {
//               downCopy( this, rightOff, bdowns, rightOff + 1, numr - 1 )
//            }
            System.arraycopy( downs, idx, bdowns, rightOff, numr )
            bdowns( rightOff ).set( right )

            new BranchImpl( bkeys, bdowns )
         }

         def write( out: DataOutput ) {
            out.writeUnsignedByte( 1 )
            val sz = size
            out.writeUnsignedByte( sz )
            var i = 0; while( i < sz ) {
               serKey.write( keys( i ), out )
            i += 1 }
            i = 0; while( i < sz ) {
               system.writeRef( downs( i ), out )
            i += 1 }
         }
      }

      private object Head extends HeadOrBranch {
//         val downNode = system.newRef[ NodeOrBottom ]( BottomImpl )
         // XXX TODO not nice to issue a txn here
         val downNode = system.atomic { implicit tx =>
            system.newRef[ NodeOrBottom ]( BottomImpl )
         }

         def updateDown( i: Int, n: LeafOrBranch )( implicit tx: S#Tx ) {
            assert( i == 0, "Accessing head with index > 0" )
            downNode.set( n )
         }

         def insertAfterSplit( pidx: Int, splitKey: A, left: LeafOrBranch, right: LeafOrBranch )
                             ( implicit tx: S#Tx ) : BranchImpl = {
            assert( pidx == 0 )

            val bkeys         = new Array[ A ]( 2 )
            bkeys( 0 )        = splitKey  // left node ends in the split key
            bkeys( 1 )        = maxKey    // right node ends in max key (remember parent is `Head`!)
            val bdowns        = system.newRefArray[ LeafOrBranch ]( 2 ) // new Array[ S#Ref[ LeafOrBranch ]]( 2 )
            bdowns( 0 )       = system.newRef( left )
            bdowns( 1 )       = system.newRef( right )
            new BranchImpl( bkeys, bdowns ) // new parent branch
         }
         override def toString = "Head"
      }

      private object BottomImpl extends Bottom with NodeOrBottom {
         override def toString = "Bottom"

         def write( out: DataOutput ) {
            out.writeUnsignedByte( 0 )
         }
      }
   }
}
sealed trait HASkipList[ S <: Sys[ S ], @specialized( Int, Long) A ] extends txn.SkipList[ S, A ] {
   def system: S

   def top( implicit tx: S#Tx ) : Child // HASkipList.Node[ A ]

   sealed trait Child   // Either Node or Bottom

   sealed trait Node extends Child {
      def size : Int
      def key( i: Int ): A
   }

   sealed trait Branch extends Node {
      def down( i: Int )( implicit tx: S#Tx ) : Node
   }

   sealed trait Leaf extends Node

   sealed trait Bottom extends Child

//   object Bottom extends Child {
//      override def toString = "Bottom"
//   }
}