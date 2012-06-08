/*
 *  HASkipList.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}
import annotation.{switch, tailrec}
import de.sciss.lucre.{DataOutput, DataInput}
import de.sciss.lucre.stm.{Sink, Sys, TxnSerializer}

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
 *
 * Three implementation notes: (1) We treat the nodes as immutable at the moment, storing them
 * directly in the S#Val child pointers of their parents. While this currently seems to
 * have a performance advantage (?), we could try to avoid this by using S#Refs for
 * the child pointers, making the nodes becomes mutables. We could avoid copying the
 * arrays for each insertion or deletion, at the cost of more space, but maybe better
 * performance.
 *
 * (2) The special treatment of `isRight` kind of sucks. Since now that information is
 * also persisted, we might just have two types of branches and leaves, and avoid passing
 * around this flag.
 *
 * (3) Since there is a bug with the top-down one-pass removal, we might end up removing
 * the creation of instances of virtual branches altogether again when replacing the
 * current algorithm by a two-pass one.
 *
 * TODO: nodes or at least leaves should be horizontally connected for a faster iterator
 *       and fast pair (interval) search
 */
object HASkipList {
   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   private final class SetSer[ S <: Sys[ S ], A ]( keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ])
                                              ( implicit ordering: Ordering[ S#Tx, A ],
                                                keySerializer: TxnSerializer[ S#Tx, S#Acc, A ])
   extends TxnSerializer[ S#Tx, S#Acc, HASkipList.Set[ S, A ]] {
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : HASkipList.Set[ S, A ] =
         HASkipList.Set.read[ S, A ]( in, access, keyObserver )

      def write( list: HASkipList.Set[ S, A ], out: DataOutput ) { list.write( out )}

      override def toString = "HASkipList.serializer"
   }

   private val SER_VERSION = 0

   private final class SetImpl[ S <: Sys[ S ], /* @specialized( Int ) */ A ]
      ( val id: S#ID, val minGap: Int, protected val keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ],
        _downNode: SetImpl[ S, A ] => S#Var[ Node[ S, A ]])
      ( implicit val ordering: Ordering[ S#Tx, A ],
        val keySerializer: TxnSerializer[ S#Tx, S#Acc, A ])
   extends Impl[ S, A, Unit ] with HASkipList.Set[ S, A ] {

      protected val downNode = _downNode( this )

      override def toString() = "SkipList.Set" + id
   }

   private sealed trait Impl[ S <: Sys[ S ], A, @specialized( Unit ) B ]
   extends HeadOrBranch[ S, A ] with TxnSerializer[ S#Tx, S#Acc, Node[ S, A ]] {
      impl =>

      protected def downNode: S#Var[ Node[ S, A ]]
      protected def minGap: Int
      protected def ordering: Ordering[ S#Tx, A ]
      protected def keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ]
      def keySerializer: TxnSerializer[ S#Tx, S#Acc, A ]
      def id: S#ID

      implicit private def head = this

      def arrMinSz = minGap + 1
      private def arrMaxSz = (minGap + 1) << 1   // aka arrMinSz << 1

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         out.writeInt( minGap )
         downNode.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         downNode.dispose()
      }

      /* override */ def size( implicit tx: S#Tx ) : Int = {
         val c = topN
         if( c eq null ) 0 else c.leafSizeSum - 1
      }

      def maxGap : Int = (minGap << 1) + 1 // aka arrMaxSz - 1

      def isEmpty( implicit tx: S#Tx )   = topN eq null
      def notEmpty( implicit tx: S#Tx )  = !isEmpty

      def toIndexedSeq( implicit tx: S#Tx ) : IIdxSeq[ A ] = fillBuilder( IIdxSeq.newBuilder[ A ])
      def toList( implicit tx: S#Tx ) : List[ A ] = fillBuilder( List.newBuilder[ A ])
      def toSeq(  implicit tx: S#Tx ) : Seq[  A ] = fillBuilder( Seq.newBuilder[  A ])
      def toSet(  implicit tx: S#Tx ) : ISet[ A ] = fillBuilder( ISet.newBuilder[  A ])

      def height( implicit tx: S#Tx ) : Int = {
         var n = topN
         if( n eq null ) 0 else {
            var h = 1
            while( n.isBranch ) {
               n = n.asBranch.down( 0 )
               h += 1
            }
            h
         }
      }

      def top( implicit tx: S#Tx ) : Option[ Node[ S, A ]] = Option( topN )
      @inline private def topN( implicit tx: S#Tx ) : Node[ S, A ] = /* Head.*/ downNode.get

      def debugPrint( implicit tx: S#Tx ) : String = topN.printNode( isRight = true ).mkString( "\n" )

      def isomorphicQuery( ord: Ordered[ S#Tx, A ])( implicit tx: S#Tx ) : (A, Int) = {
         def isoIndexR( n: Node[ S, A ]) : Int = {
            var idx  = 0
            val sz   = n.size - 1
            do {
               val cmp = ord.compare( n.key( idx ))
               if( cmp == 0 ) return -(idx + 1) else if( cmp < 0 ) return idx
               idx += 1
            } while( idx < sz )
            sz
         }

         def isoIndexL( n: Node[ S, A ])( implicit tx: S#Tx ) : Int = {
            @tailrec def step( idx : Int ) : Int = {
               val cmp = ord.compare( n.key( idx ))
               if( cmp == 0 ) -(idx + 1) else if( cmp < 0 ) idx else step( idx + 1 )
            }
            step( 0 )
         }

         @tailrec def stepRight( n: Node[ S, A ]) : (A, Int) = {
            val idx     = isoIndexR( n )
            val found   = idx < 0
            if( found ) {
               val idxP    = -(idx + 1)
               (n.key( idxP ), 0)
            } else if( n.isLeaf ) {
               if( idx == n.size - 1 ) (n.key( idx - 1 ), 1) else (n.key( idx ), -1)
            } else {
               val c = n.asBranch.down( idx )
               if( idx < n.size - 1 ) stepLeft( c ) else stepRight( c )
            }
         }

         @tailrec def stepLeft( n: Node[ S, A ]) : (A, Int) = {
            val idx = isoIndexL( n )
            val found   = idx < 0
            if( found ) {
               val idxP    = -(idx + 1)
               (n.key( idxP ), 0)
            } else if( n.isLeaf ) {
               (n.key( idx ), -1)
            } else {
               stepLeft( n.asBranch.down( idx ))
            }
         }

         val c = topN
         if( c eq null ) {
            throw new NoSuchElementException( "isomorphicQuery on an empty list" )
         } else {
            stepRight( c )
         }
      }

      // ---- set support ----

      def contains( v: A )( implicit tx: S#Tx ) : Boolean = {
//         if( ordering.gteq( v, maxKey )) return false

         @tailrec def stepRight( n: Node[ S, A ]) : Boolean = {
            val idx = indexInNodeR( v, n )
            if( idx < 0 ) true else if( n.isLeaf ) false else {
               val c = n.asBranch.down( idx )
               if( idx < n.size - 1 ) stepLeft( c ) else stepRight( c )
            }
         }

         @tailrec def stepLeft( n: Node[ S, A ]) : Boolean = {
            val idx = indexInNodeL( v, n )
            if( idx < 0 ) true else if( n.isLeaf ) false else stepLeft( n.asBranch.down( idx ))
         }

         val c = topN
         if( c eq null ) false else stepRight( c )
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
      private def indexInNodeR( v: A, n: Node[ S, A ])( implicit tx: S#Tx ) : Int = {
         var idx  = 0
         val sz   = n.size - 1
         do {
            val cmp = ordering.compare( v, n.key( idx ))
            if( cmp == 0 ) return -(idx + 1) else if( cmp < 0 ) return idx
            idx += 1
         } while( idx < sz )
         sz
      }

      private def indexInNodeL( v: A, n: Node[ S, A ])( implicit tx: S#Tx ) : Int = {
         @tailrec def step( idx : Int ) : Int = {
            val cmp = ordering.compare( v, n.key( idx ))
            if( cmp == 0 ) -(idx + 1) else if( cmp < 0 ) idx else step( idx + 1 )
         }
         step( 0 )
      }

      /* override */ def add( v: A )( implicit tx: S#Tx ) : Boolean = {
         val c = topN
         if( c eq null ) {
            val lkeys   = IIdxSeq[ A ]( v, null.asInstanceOf[ A ])
            val l       = new Leaf[ S, A ]( lkeys )
            /*Head.*/ downNode.set( l )
            true
         } else if( c.isLeaf ) {
            addToLeaf( v, head, 0, head, 0, c.asLeaf, isRight = true )
         } else {
            addToBranch( v, head, 0, head, 0, c.asBranch, isRight = true )
         }
      }

      private def addToLeaf( v: A, pp: HeadOrBranch[ S, A ], ppidx: Int, p: HeadOrBranch[ S, A ], pidx: Int,
                           l: Leaf[ S, A ], isRight: Boolean )
                         ( implicit tx: S#Tx ) : Boolean = {
         val idx = if( isRight ) indexInNodeR( v, l ) else indexInNodeL( v, l )
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

      @tailrec private def addToBranch( v: A, pp: HeadOrBranch[ S, A ], ppidx: Int, p: HeadOrBranch[ S, A ], pidx: Int,
                                      b: Branch[ S, A ], isRight: Boolean )
                                    ( implicit tx: S#Tx ) : Boolean = {
         val idx = if( isRight ) indexInNodeR( v, b ) else indexInNodeL( v, b )
         if( idx < 0 ) return false

         var bNew       = b
         var idxNew     = idx
         var pNew       = p
         var pidxNew    = pidx
         val bsz        = b.size
         val isRightNew = isRight && (idx == bsz - 1)

         if( bsz == arrMaxSz ) {
            val splitKey   = b.key( minGap )
            val tup        = b.split
            val left       = tup._1
            val right      = tup._2
            val pbNew      = p.insertAfterSplit( pidx, splitKey, left, right )
            pNew           = pbNew
            pp.updateDown( ppidx, pbNew )
            val mns        = arrMinSz
            if( idx < mns ) {
               bNew     = left
            } else {
               bNew     = right
               pidxNew += 1
               idxNew  -= mns
            }
            keyObserver.keyUp( splitKey )
         }

         val c = bNew.down( idxNew )
         if( c.isLeaf ) {
            addToLeaf(   v, pNew, pidxNew, bNew, idxNew, c.asLeaf, isRightNew )
         } else {
            addToBranch( v, pNew, pidxNew, bNew, idxNew, c.asBranch, isRightNew )
         }
      }

      def +=( elem: A )( implicit tx: S#Tx ) : this.type = { add( elem ); this }
      def -=( elem: A )( implicit tx: S#Tx ) : this.type = { remove( elem ); this }

      /* override */ def remove( v: A )( implicit tx: S#Tx ) : Boolean = {
         val c = topN
         if( c eq null ) {
            false
         } else if( c.isLeaf ) {
            removeFromLeaf(   v, /* Head. */downNode, c.asLeaf, isRight = true, false )
         } else {
            removeFromBranch( v, /* Head. */ downNode, c.asBranch, isRight = true, false )
         }
      }

      private def removeFromLeaf( v: A, pDown: Sink[ S#Tx, Node[ S, A ]], l: Leaf[ S, A ],
                              isRight: Boolean, lDirty: Boolean )( implicit tx: S#Tx ) : Boolean = {
         val idx     = if( isRight ) indexInNodeR( v, l ) else indexInNodeL( v, l )
         val found   = idx < 0
         val lNew    = if( found ) {
            val idxP = -(idx + 1)
            l.removeColumn( idxP )
         } else {
            l // .devirtualize
         }
         if( lDirty || (lNew ne l) ) {
            pDown.set( if( lNew.size > 1 ) lNew else null )
         }
         found
      }

      @tailrec private def removeFromBranchAndBubble( v: A, pDown: Sink[ S#Tx, Node[ S, A ]], b: Branch[ S, A ],
                                                      leafUpKey: A )( implicit tx: S#Tx ) : Boolean = {
         val bsz        = b.size
         val idxP       = bsz - 1   // that we know
         val mns        = arrMinSz
         val c          = b.down( idxP )
         val cSz        = c.size

         var bNew: Branch[ S, A ] = null
         var bDownIdx         = idxP
         var cNew: Node[ S, A ] = c

         keyObserver.keyDown( v )

         // a merge or borrow is necessary either when we descend
         // to a minimally filled child (because that child might
         // need to shrink in the next step)
         if( cSz == mns ) {
//            val idxP1      = idxP + 1
//            val bHasRight  = idxP1 < bsz

            // merge with or borrow from the left
            val idxPM1  = idxP - 1
            val cSib    = b.down( idxPM1 )
            val cSibSz  = cSib.size

            val downKey    = b.key( idxPM1 )
            keyObserver.keyDown( downKey )

            if( cSibSz == mns ) {                           // merge with the left
               // The parent needs to remove the
               // entry of the left sibling.
               val bNew0   = b.removeColumn( idxPM1 )
               bNew        = bNew0.updateKey( idxPM1, leafUpKey ) // XXX optimise by merging with previous operation
               b.downRef( idxPM1 ).dispose()
               bDownIdx    = idxPM1
//               cNew        = c.virtualize( ModMergeLeft, cSib )
               cNew        = c.mergeLeft( cSib )
            } else {                                        // borrow from the left
               // the parent needs to update the key for the
               // left sibling to match the before-last key in
               // the left sibling.
               val upKey   = cSib.key( cSibSz - 2 )
               val bNew0       = b.updateKey( idxPM1, upKey )
               bNew = bNew0.updateKey( idxP, leafUpKey )          // XXX optimise by merging with previous operation
//               bNew.setKey( idxP, leafUpKey )
               keyObserver.keyUp( upKey )
               val bDown1  = b.downRef( idxPM1 )
               bDown1.set( cSib.removeColumn( cSibSz - 1 ))
//               cNew        = c.virtualize( ModBorrowFromLeft, cSib )
               cNew        = c.borrowLeft( cSib )
            }
         } else {
            bNew  = b.updateKey( idxP, leafUpKey )
         }

         keyObserver.keyUp( leafUpKey )

         // branch changed
         val bDown =  if( bNew.size > 1 ) {
            pDown.set( bNew ) // update down ref from which it came
            bNew.downRef( bDownIdx )
         } else {
            // unfortunately we do not have `p`
//               assert( p == Head )
            bNew.downRef( 0 ).dispose()
            pDown
         }

//         val bDown = bNew.downRef( bDownIdx )
         if( cNew.isLeaf ) {
            removeFromLeaf( v, bDown, cNew.asLeaf, isRight = false, cNew ne c )
         } else {
            removeFromBranchAndBubble( v, bDown, cNew.asBranch, leafUpKey )
         }
      }

      @tailrec private def removeFromBranch( v: A, pDown: Sink[ S#Tx, Node[ S, A ]], b: Branch[ S, A ],
                                         isRight: Boolean, bDirty: Boolean )( implicit tx: S#Tx ) : Boolean = {
         val idx        = if( isRight ) indexInNodeR( v, b ) else indexInNodeL( v, b )
         val found      = idx < 0
         val idxP       = if( found ) -(idx + 1) else idx
         val bsz        = b.size
         val mns        = arrMinSz
         val c          = b.down( idxP )
         val cSz        = /* if( cFound ) c.size - 1 else */ c.size

         // if v is found, it will appear in right-most position in all following children.
         // there are two possibilities:
         // (1) a borrow-from-right or merge-with-right is performed. in this case,
         //     v is overwritten in the current branch, thus keep going normally
         //     (no need to specially treat the branch).
         // (2) none of these two operations are performed (because either the child size
         //     is greater than minimum, or v appears in right-most position in b (causing a left op).
         //     -- is this second case possible? no, because we would have encountered
         //     case (1) in the previous iteration, that is, a borrow-from-right or merge-
         //     with-right would have been performed, and thus v cannot appear in right-most
         //     position, and there cannot be a left op.
         // Therefore, we only need to specially treat case (2), that is `cSz > mns`!
         if( found && cSz > mns ) {
            // we are here, because the key was found and it would appear in the right-most position
            // in the child, unless we treat it specially here, by finding the key that will bubble
            // up!
            @tailrec def findUpKey( n: Node[ S, A ]) : A = {
               if( n.isLeaf ) {
                  n.key( n.size - 2 )
               } else {
                  findUpKey( n.asBranch.down( n.size - 1 ))
               }
            }
            val leafUpKey = findUpKey( c )
            keyObserver.keyDown( v )
            val bNew  = b.updateKey( idxP, leafUpKey )
            keyObserver.keyUp( leafUpKey )

            pDown.set( bNew ) // update down ref from which we came
            val bDown = bNew.downRef( idxP )
            return if( c.isLeaf ) {
               removeFromLeaf( v, bDown, c.asLeaf, isRight = false, false )
            } else {
               removeFromBranchAndBubble( v, bDown, c.asBranch, leafUpKey )
            }
         }

         var isRightNew = isRight && (idxP == bsz - 1)
         var bNew: Branch[ S, A ] = b // null
         var bDownIdx         = idxP
         var cNew: Node[ S, A ] = c

         // a merge or borrow is necessary either when we descend
         // to a minimally filled child (because that child might
         // need to shrink in the next step)
         if( cSz == mns ) {
            val idxP1      = idxP + 1
            val bHasRight  = idxP1 < bsz
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
                  b.downRef( idxP ).dispose()
//                  cNew        = c.virtualize( ModMergeRight, cSib )
                  cNew        = c.mergeRight( cSib )
                  isRightNew  = isRight && (idxP == bsz - 2) // ! we might be in the right-most branch now
               } else {                                      // borrow from the right
                  assert( cSibSz > mns )
                  // update the key index idxP of the
                  // originating sibling to match the first key in
                  // the right sibling
                  val upKey   = cSib.key( 0 )
                  bNew        = b.updateKey( idxP, upKey )
                  keyObserver.keyUp( upKey )
                  val bDown1  = b.downRef( idxP1 )
                  bDown1.set( cSib.removeColumn( 0 ))
//                  cNew        = c.virtualize( ModBorrowFromRight, cSib )
                  cNew        = c.borrowRight( cSib )
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

               if( cSibSz == mns ) {                           // merge with the left
                  // The parent needs to remove the
                  // entry of the left sibling.
                  bNew        = b.removeColumn( idxPM1 )
                  b.downRef( idxPM1 ).dispose()
                  bDownIdx    = idxPM1
//                  cNew        = c.virtualize( ModMergeLeft, cSib )
                  cNew        = c.mergeLeft( cSib )
               } else {                                        // borrow from the left
                  // the parent needs to update the key for the
                  // left sibling to match the before-last key in
                  // the left sibling.
                  val upKey   = cSib.key( cSibSz - 2 )
                  bNew        = b.updateKey( idxPM1, upKey )
                  keyObserver.keyUp( upKey )
                  val bDown1  = b.downRef( idxPM1 )
                  bDown1.set( cSib.removeColumn( cSibSz - 1 ))
//                  cNew        = c.virtualize( ModBorrowFromLeft, cSib )
                  cNew        = c.borrowLeft( cSib )
               }
            }
//         } else {
//            bNew  = b.devirtualize
         }

         val bDown = if( bDirty || (bNew ne b) ) { // branch changed
            if( bNew.size > 1 ) {
               pDown.set( bNew ) // update down ref from which it came
               bNew.downRef( bDownIdx )
            } else {
               // unfortunately we do not have `p`
//               assert( p == Head )
               bNew.downRef( 0 ).dispose()
               pDown
            }
         } else {
            bNew.downRef( bDownIdx )
         }

//         val bDown = bNew.downRef( bDownIdx )
         val cDirty = cNew ne c
         if( cNew.isLeaf ) {
            removeFromLeaf(   v, bDown, cNew.asLeaf, isRightNew, cDirty )
         } else {
            removeFromBranch( v, bDown, cNew.asBranch, isRightNew, cDirty )
         }
      }

      def keysIterator( implicit tx: S#Tx ) : Iterator[ S#Tx, A ] = {
         val i = new IteratorImpl
         i.init()
         i
      }

      private def fillBuilder[ Res ]( b: Builder[ A, Res ])( implicit tx: S#Tx ) : Res = {
         val i = new IteratorImpl
         i.init()
         while( i.hasNext ) {
            b += i.next() // Txn
         }
         b.result()
      }

      // ---- TxnSerializer[ S#Tx, S#Acc, Node[ S, A ]] ----
      def write( v: Node[ S, A ], out: DataOutput ) {
         if( v eq null ) {
            out.writeUnsignedByte( 0 ) // Bottom
         } else {
            v.write( out )
         }
      }
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Node[ S, A ] = {
         (in.readUnsignedByte(): @switch) match {
            case 0 => null // .asInstanceOf[ Branch[ S, A ]]
            case 1 => Branch.read( in, access, isRight = false )
            case 2 => Leaf.read(   in, access, isRight = false )
            case 5 => Branch.read( in, access, isRight = true  )
            case 6 => Leaf.read(   in, access, isRight = true  )
         }
      }

      // since Iterator is not specialized anyway, we don't care
      // that IteratorImpl won't be, either
      private final class IteratorImpl extends Iterator[ S#Tx, A ] {
         private var l: Leaf[ S, A ]   = null
         private var nextKey : A       = _
         private var isRight           = true
         private var idx               = 0
         private val stack             = collection.mutable.Stack.empty[ (Branch[ S, A ], Int, Boolean) ]
//         pushDown( 0, Head )

         override def toString = impl.toString + ".iterator"

         @tailrec private def pushDown( n: Node[ S, A ], idx0: Int, r: Boolean )( implicit tx: S#Tx ) {
            if( n.isLeaf ) {
               val l2   = n.asLeaf
               l        = l2
               idx      = 0
               isRight  = r
               nextKey  = l2.key( 0 )
            } else {
               val b    = n.asBranch
               stack.push( (b, idx0 + 1, r) )
               pushDown( b.down( idx0 ), 0, r && (idx0 == b.size - 1) )
            }
         }

         def init()( implicit tx: S#Tx ) {
            val c = topN
            if( c ne null ) pushDown( c, 0, r = true )
         }

         def hasNext : Boolean = l ne null // ordering.nequiv( nextKey, maxKey )

         def next()( implicit tx: S#Tx ) : A = {
            if( !hasNext ) throw new java.util.NoSuchElementException( "next on empty iterator" )
            val res  = nextKey
            idx     += 1
            if( idx == (if( isRight ) l.size - 1 else l.size) /* || ordering.equiv( l.key( idx ), maxKey ) */) {
               @tailrec def popUp() {
                  if( stack.isEmpty ) {
                     l        = null
                     nextKey  = null.asInstanceOf[ A ] // maxKey
                  } else {
                     val (b, i, r) = stack.pop()
                     if( i < b.size ) {
                        pushDown( b, i, r )
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
      def updateDown( i: Int, n: Node[ S, A ])( implicit tx: S#Tx ) {
//          assert( i == 0, "Accessing head with index > 0" )
         downNode.set( n )
      }

      def insertAfterSplit( pidx: Int, splitKey: A, left: Node[ S, A ], right: Node[ S, A ])
                          ( implicit tx: S#Tx, head: Impl[ S, A, _ ]) : Branch[ S, A ] = {
         val bkeys = IIdxSeq[ A ]( splitKey, null.asInstanceOf[ A ])
         val bdowns = IIdxSeq[ S#Var[ Node[ S, A ]]](
            tx.newVar( head.id, left ),
            tx.newVar( head.id, right )
         )
         new Branch[ S, A ]( bkeys, bdowns ) // new parent branch
      }
   }

   sealed trait HeadOrBranch[ S <: Sys[ S ], A ] /* extends Branch */ {
      private[HASkipList] def updateDown( i: Int, n: Node[ S, A ])( implicit tx: S#Tx ) : Unit

      private[HASkipList] def insertAfterSplit( pidx: Int, splitKey: A, left: Node[ S, A ], right: Node[ S, A ])
                                              ( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Branch[ S, A ]
   }

   sealed trait Node[ S <: Sys[ S ], @specialized( Int ) A ] {
      private[HASkipList] def removeColumn( idx: Int )( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Node[ S, A ]
      def size : Int
      def key( i: Int ): A

      private[HASkipList] def write( out: DataOutput )( implicit list: Impl[ S, A, _ ]) : Unit
      private[HASkipList] def leafSizeSum( implicit tx: S#Tx ) : Int
      private[HASkipList] def printNode( isRight: Boolean )( implicit tx: S#Tx ) : IndexedSeq[ String ]

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
      private[HASkipList] def mergeRight( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ]

      /*
       * In borrow-from-right, both parents' downs need
       * update, but identifiers are kept.
       * the parent needs to update the key for the
       * originating sibling to match the first key in
       * the right sibling (or the new last key in the
       * originating sibling).
       */
      private[HASkipList] def borrowRight( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ]

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
      private[HASkipList] def mergeLeft( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ]

      /*
       * In borrow-from-left, both parents' downs need
       * update, but identifiers are kept.
       * the parent needs to update the key for the
       * left sibling to match the before-last key in
       * the left sibling.
       */
      private[HASkipList] def borrowLeft( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ]

      def isLeaf   : Boolean
      def isBranch : Boolean
      def asLeaf   : Leaf[ S, A ]
      def asBranch : Branch[ S, A ]
   }

   object Leaf {
      private[HASkipList] def read[ S <: Sys[ S ], @specialized( Int ) A ]( in: DataInput, access: S#Acc, isRight: Boolean )
                                                                          ( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Leaf[ S, A ] = {
         import list.keySerializer
         val sz: Int = in.readUnsignedByte()
         val szi  = if( isRight ) sz - 1 else sz
         val keys = IIdxSeq.tabulate[ A ]( sz ) { i =>
            if( i < szi ) keySerializer.read( in, access ) else null.asInstanceOf[ A ]
         }
         new Leaf[ S, A ]( keys )
      }
   }
   final class Leaf[ S <: Sys[ S ], @specialized( Int ) A ]( private[HASkipList] val keys: IIdxSeq[ A ])
   extends /* LeafLike[ S, A ] with */ Node[ S, A ] {
      override def toString = keys.mkString( "Leaf(", ",", ")" )

      def key( idx: Int ) = keys( idx )
      def size : Int = keys.length

      def isLeaf   : Boolean = true
      def isBranch : Boolean = false
      def asLeaf   : Leaf[ S, A ]   = this
      def asBranch : Branch[ S, A ] = opNotSupported

      private[HASkipList] def devirtualize( implicit tx: S#Tx, list: SetImpl[ S, A ]): Leaf[ S, A ] = this

      private[HASkipList] def leafSizeSum( implicit tx: S#Tx ) : Int = size

      private[HASkipList] def printNode( isRight: Boolean )( implicit tx: S#Tx ) : IndexedSeq[ String ] = {
         val sz   = size
         val szm  = sz - 1
         val keys = Seq.tabulate( sz )( idx => if( !isRight || idx < szm ) key( idx ).toString else "M" )
         IndexedSeq( keys.mkString( "--" ))
      }

      private[HASkipList] def mergeRight( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val lSib = sib.asLeaf
         new Leaf( keys ++ lSib.keys )
      }

      private[HASkipList] def borrowRight( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val lSib = sib.asLeaf
         new Leaf( keys :+ lSib.keys.head )
      }

      private[HASkipList] def mergeLeft( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val lSib = sib.asLeaf
         new Leaf( lSib.keys ++ keys )
      }

      private[HASkipList] def borrowLeft( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val lSib = sib.asLeaf
         new Leaf( lSib.keys.last +: keys )
      }

      private[HASkipList] def insert( v: A, idx: Int )( implicit list: Impl[ S, A, _ ]) : Leaf[ S, A ] = {
         val lkeys = keys.patch( idx, IIdxSeq( v ), 0 )
         new Leaf[ S, A ]( lkeys )
      }

      private[HASkipList] def splitAndInsert( v: A, idx: Int )
                                            ( implicit list: Impl[ S, A, _ ]) : (Leaf[ S, A ], Leaf[ S, A ]) = {
//         assert( size == arrMaxSz )
         val arrMinSz = list.arrMinSz
         val (lkeys0, rkeys0) = keys.splitAt( arrMinSz )
         if( idx < arrMinSz ) {  // split and add `v` to left leaf
            val lkeys   = lkeys0.patch( idx, IIdxSeq( v ), 0 )
            val left    = new Leaf[ S, A ]( lkeys )
            val right   = new Leaf[ S, A ]( rkeys0 )

            (left, right)

         } else {               // split and add `v` to right leaf
            val left    = new Leaf[ S, A ]( lkeys0 )
            val numl    = idx - arrMinSz
            val rkeys   = rkeys0.patch( numl, IIdxSeq( v ), 0 )
            val right   = new Leaf[ S, A ]( rkeys )

            (left, right)
         }
      }

      private[HASkipList] def removeColumn( idx: Int )( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Leaf[ S, A ] = {
         val newKeys = keys.patch( idx, IIdxSeq.empty, 1 )
         new Leaf[ S, A ]( newKeys )
      }

//         override def toString = toString( "Leaf" )

      private[HASkipList] def write( out: DataOutput )( implicit list: Impl[ S, A, _ ]) {
         import list.keySerializer
         val sz      = size
         val sz1     = sz - 1
         val isRight = keys( sz1 ) == null
         val szi     = if( isRight ) sz1 else sz
         out.writeUnsignedByte( if( isRight ) 6 else 2 )
         out.writeUnsignedByte( sz )
         var i = 0; while( i < szi ) {
            keySerializer.write( keys( i ), out )
         i += 1 }
      }
   }

   object Branch {
      private[HASkipList] def read[ S <: Sys[ S ], @specialized( Int ) A ]( in: DataInput, access: S#Acc, isRight: Boolean )
                                                                          ( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Branch[ S, A ] = {
         import list._
         val sz: Int = in.readUnsignedByte()
         val szi     = if( isRight ) sz - 1 else sz
         val keys    = IIdxSeq.tabulate( sz ) { i =>
            if( i < szi ) keySerializer.read( in, access ) else null.asInstanceOf[ A ]
         }
         val downs = IIdxSeq.fill( sz )( tx.readVar[ Node[ S, A ]]( list.id, in ))
         new Branch[ S, A ]( keys, downs )
      }
   }
   final class Branch[ S <: Sys[ S ], @specialized( Int ) A ]( private[HASkipList] val keys: IIdxSeq[ A ],
                                                               private[HASkipList] val downs: IIdxSeq[ S#Var[ Node[ S, A ]]])
   extends /* BranchLike[ S, A ] with */ HeadOrBranch[ S, A ] with Node[ S, A ] {
//      assert( keys.size == downs.size )

      override def toString = keys.mkString( "Branch(", ",", ")" )

      def isLeaf   : Boolean = false
      def isBranch : Boolean = true
      def asLeaf   : Leaf[ S, A ]   = opNotSupported
      def asBranch : Branch[ S, A ] = this

      private[HASkipList] def mergeRight( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val bSib = sib.asBranch
         new Branch( keys ++ bSib.keys, downs ++ bSib.downs )
      }

      private[HASkipList] def borrowRight( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val bSib = sib.asBranch
         new Branch( keys :+ bSib.keys.head, downs :+ bSib.downs.head )
      }

      private[HASkipList] def mergeLeft( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val bSib = sib.asBranch
         new Branch( bSib.keys ++ keys, bSib.downs ++ downs )
      }

      private[HASkipList] def borrowLeft( sib: Node[ S, A ])( implicit tx: S#Tx ) : Node[ S, A ] = {
         val bSib = sib.asBranch
         new Branch( bSib.keys.last +: keys, bSib.downs.last +: downs )
      }

      private[HASkipList] def insert( v: A, idx: Int )( implicit list: SetImpl[ S, A ]) : Leaf[ S, A ] = {
         val lkeys = keys.patch( idx, IIdxSeq( v ), 0 )
         new Leaf[ S, A ]( lkeys )
      }

      private[HASkipList] def leafSizeSum( implicit tx: S#Tx ) : Int = {
         var res  = 0
         val sz   = size
         var i = 0; while( i < sz ) {
            res += down( i ).leafSizeSum
         i += 1 }
         res
      }

      private[HASkipList] def printNode( isRight: Boolean )( implicit tx: S#Tx ) : IndexedSeq[ String ] = {
         val sz      = size
         val szm     = sz - 1
         val columns = IndexedSeq.tabulate( sz ) { idx =>
            val rr      = isRight && idx == szm
            val child   = down( idx ).printNode( rr )
            val childSz = child.head.length()
            val ks      = if( rr ) "M" else key( idx ).toString
            val keySz   = ks.length()
            val colSz   = math.max( keySz, childSz ) + 2
            val keyAdd  = (if( idx == size - 1 ) " " else "-") * (colSz - keySz)
            val bar     = "|" + (" " * (colSz - 1))
            val childAdd= " " * (colSz - childSz)
            IndexedSeq( ks + keyAdd, bar ) ++ child.map( _ + childAdd )
         }
         IndexedSeq.tabulate( columns.map( _.size ).max ) { row =>
            columns.map( _.apply( row )).mkString( "" )
         }
      }

      def key( idx: Int ) : A = keys( idx )
      def size : Int = keys.length

      private[HASkipList] def downRef( i: Int ) : S#Var[ Node[ S, A ]] = downs( i )

      def down( i: Int )( implicit tx: S#Tx ) : Node[ S, A ] = downs( i ).get

      private[HASkipList] def split( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : (Branch[ S, A ], Branch[ S, A ]) = {
         import list.{size => _, _}
         val lsz     = arrMinSz
         val (lkeys, rkeys)   = keys.splitAt( lsz )
         val (ldowns, rdowns) = downs.splitAt( lsz )
         val left    = new Branch[ S, A ]( lkeys, ldowns )
         val right   = new Branch[ S, A ]( rkeys, rdowns )

         (left, right)
      }

      private[HASkipList] def updateDown( i: Int, n: Node[ S, A ])( implicit tx: S#Tx ) {
         downs( i ).set( n )
      }

      private[HASkipList] def removeColumn( idx: Int )( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Branch[ S, A ] = {
         val newKeys    = keys.patch(  idx, IIdxSeq.empty, 1 )
         val newDowns   = downs.patch( idx, IIdxSeq.empty, 1 )
         new Branch[ S, A ]( newKeys, newDowns )
      }

      private[HASkipList] def updateKey( idx: Int, key: A )( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Branch[ S, A ] = {
         val newKeys    = keys.updated( idx, key )
         new Branch[ S, A ]( newKeys, downs )
      }

      private[HASkipList] def insertAfterSplit( idx: Int, splitKey: A, left: Node[ S, A ], right: Node[ S, A ])
                                              ( implicit tx: S#Tx, list: Impl[ S, A, _ ]) : Branch[ S, A ] = {
         // we must make a copy of this branch with the
         // size increased by one. the new key is `splitKey`
         // which gets inserted at the index where we went
         // down, `idx`.
         val bkeys   = keys.patch(  idx, IIdxSeq( splitKey ), 0 )
         val bdowns  = downs.patch( idx, IIdxSeq( tx.newVar( list.id, left )), 0 )

         // copy entries right to split index
         val rightOff      = idx + 1
         bdowns( rightOff ).set( right )

         new Branch[ S, A ]( bkeys, bdowns )
      }

      private[HASkipList] def write( out: DataOutput )( implicit list: Impl[ S, A, _ ]) {
         import list.keySerializer
         val sz      = size
         val sz1     = sz - 1
         val isRight = keys( sz1 ) == null
         val szi     = if( isRight ) sz1 else sz
         out.writeUnsignedByte( if( isRight ) 5 else 1 )
// assert( sz > 0 )
         out.writeUnsignedByte( sz )
         var i = 0; while( i < szi ) {
            keySerializer.write( keys( i ), out )
         i += 1 }
         i = 0; while( i < sz ) {
            downs( i ).write( out )
         i += 1 }
      }
   }

   object Set {
      /**
       * Creates a new empty skip list with default minimum gap parameter of `2` and no key observer.
       * Type parameter `S` specifies the STM system to use. Type parameter `A`
       * specifies the type of the keys stored in the list.
       *
       * @param   tx          the transaction in which to initialize the structure
       * @param   ord         the ordering of the keys. This is an instance of `txn.Ordering` to allow
       *                      for specialized versions and transactional restrictions.
       * @param   keySerializer      the serializer for the elements, in case a persistent STM is used.
       */
      def empty[ S <: Sys[ S ], A ]( implicit tx: S#Tx, ord: Ordering[ S#Tx, A ],
                                     keySerializer: TxnSerializer[ S#Tx, S#Acc, A ]) : HASkipList.Set[ S, A ] =
         empty()

      /**
       * Creates a new empty skip list. Type parameter `S` specifies the STM system to use. Type parameter `A`
       * specifies the type of the keys stored in the list.
       *
       * @param   minGap      the minimum gap-size used for the skip list. This value must be between 1 and 126 inclusive.
       * @param   keyObserver an object which observes key promotions and demotions. Use `NoKeyObserver` (default) if
       *                      key motions do not need to be monitored. The monitoring allows the use of the skip list
       *                      for synchronized decimations of related data structures, such as the deterministic
       *                      skip quadtree.
       * @param   tx          the transaction in which to initialize the structure
       * @param   ord         the ordering of the keys. This is an instance of `txn.Ordering` to allow
       *                      for specialized versions and transactional restrictions.
       * @param   keySerializer  the serializer for the elements, in case a persistent STM is used.
       */
      def empty[ S <: Sys[ S ], A ]( minGap: Int = 2,
                                     keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ] = txn.SkipList.NoKeyObserver[ A ])
                                   ( implicit tx: S#Tx, ord: Ordering[ S#Tx, A ],
                                     keySerializer: TxnSerializer[ S#Tx, S#Acc, A ]) : HASkipList.Set[ S, A ] = {

         // 255 <= arrMaxSz = (minGap + 1) << 1
         // ; this is, so we can write a node's size as signed byte, and
         // no reasonable app would use a node size > 255
         require( minGap >= 1 && minGap <= 126, "Minimum gap (" + minGap + ") cannot be less than 1 or greater than 126" )

         val implID = tx.newID()
         new SetImpl[ S, A ]( implID, minGap, keyObserver, list => {
            tx.newVar[ Node[ S, A ]]( implID, null )( list )
         })
      }

      def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc,
         keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ] = txn.SkipList.NoKeyObserver[ A ])( implicit tx: S#Tx,
         ordering: Ordering[ S#Tx, A ], keySerializer: TxnSerializer[ S#Tx, S#Acc, A ]) : HASkipList.Set[ S, A ] = {

         val id      = tx.readID( in, access )
         val version = in.readUnsignedByte()
         require( version == SER_VERSION, "Incompatible serialized version (found " + version +
            ", required " + SER_VERSION + ")." )

         val minGap  = in.readInt()
         new SetImpl[ S, A ]( id, minGap, keyObserver, list => tx.readVar[ Node[ S, A ]]( id, in )( list ))
      }

      def serializer[ S <: Sys[ S ], A ]( keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ] = txn.SkipList.NoKeyObserver[ A ])
                                        ( implicit ordering: Ordering[ S#Tx, A ],
                                          keySerializer: TxnSerializer[ S#Tx, S#Acc, A ]): TxnSerializer[ S#Tx, S#Acc, HASkipList.Set[ S, A ]] =
         new SetSer[ S, A ]( keyObserver )

   }

   sealed trait Set[ S <: Sys[ S ], @specialized( Int ) A ] extends txn.SkipList.Set[ S, A ] {
      def top( implicit tx: S#Tx ) : Option[ HASkipList.Node[ S, A ]]
   }
}
//sealed trait HASkipList[ S <: Sys[ S ], @specialized( Int ) A ] extends txn.SkipList[ S, A ] {
//}