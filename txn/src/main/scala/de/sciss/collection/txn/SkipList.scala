/*
 *  SkipList.scala
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

import de.sciss.lucre.stm.{TxnSerializer, Mutable, Sys}
import de.sciss.lucre.{DataOutput, DataInput}
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}

object SkipList {
   /**
    * A trait for observing the promotion and demotion of a key
    * in the skip list's level hierarchy
    *
    * XXX TODO: doesn't the variance annotation of type parameter A
    * undo the specialization?? you could one otherwise pass in
    * NoKeyObserver to a SkipList[ Int ]?
    */
   trait KeyObserver[ -Tx, @specialized( Int, Long ) -A ] {
      /**
       * Notifies the observer that a given key
       * is promoted to a higher (more sparse) level
       */
      def keyUp( key : A )( implicit tx: Tx ) : Unit
      /**
       * Notifies the observer that a given key
       * is demoted to a lower (more dense) level
       */
      def keyDown( key : A )( implicit tx: Tx ) : Unit
   }

   // Note: We could also have `object NoKeyObserver extends KeyObserver[ Any, Any ]` if
   // `A` was made contravariant, too. But I guess we would end up in boxing since
   // that wouldn't be specialized any more?
   def NoKeyObserver[ A ] : KeyObserver[ Any, A ] = new NoKeyObserver[ A ]
   private final class NoKeyObserver[ A ] extends KeyObserver[ Any, A ] {
      def keyUp( key : A )( implicit tx: Any ) {}
      def keyDown( key : A )( implicit tx: Any ) {}
   }

   object Set {
      def empty[ S <: Sys[ S ], A ]( implicit tx: S#Tx, ord: Ordering[ S#Tx, A ],
                                     serKey: TxnSerializer[ S#Tx, S#Acc, A ]): SkipList.Set[ S, A ] = HASkipList.Set.empty[ S, A ]

      def empty[ S <: Sys[ S ], A ]( keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ] = txn.SkipList.NoKeyObserver[ A ])(
         implicit tx: S#Tx, ord: Ordering[ S#Tx, A ],
         serKey: TxnSerializer[ S#Tx, S#Acc, A ]): SkipList.Set[ S, A ] = HASkipList.Set.empty[ S, A ]( keyObserver = keyObserver )

      def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc,
            keyObserver: txn.SkipList.KeyObserver[ S#Tx, A ] = txn.SkipList.NoKeyObserver[ A ])( implicit tx: S#Tx,
            ordering: Ordering[ S#Tx, A ], keySerializer: TxnSerializer[ S#Tx, S#Acc, A ]) : SkipList.Set[ S, A ] =
         HASkipList.Set.read[ S, A ]( in, access, keyObserver )

   }

   trait Set[ S <: Sys[ S ], @specialized( Int, Long ) A ] extends SkipList[ S, A ] {
      /**
       * Finds the nearest item equal or greater
       * than an unknown item from an isomorphic
       * set. The isomorphism is represented by
       * a comparison function which guides the
       * binary search.
       *
       * @param   ord   a function that guides the search.
       *    should return -1 if the argument is smaller
       *    than the search key, 0 if both are equivalent,
       *    or 1 if the argument is greater than the search key.
       *    E.g., using some mapping, the function could look
       *    like `mapping.apply(_).compare(queryKey)`
       *
       * @return  the nearest item, or the maximum item
       */
      def isomorphicQuery( ord: Ordered[ S#Tx, A ])( implicit tx: S#Tx ) : (A, Int)

      def floor( key: A )( implicit tx: S#Tx ) : Option[ A ]
      def ceil( key: A )(  implicit tx: S#Tx ) : Option[ A ]

      /**
       * Inserts a new key into the list.
       *
       * @param   key  the key to insert
       * @return  `true` if the key was successfully inserted,
       *          `false` if a node with the given key already existed
       */
      def add( key: A )( implicit tx: S#Tx ) : Boolean

      def remove( key: A )( implicit tx: S#Tx ) : Boolean

      def +=( key: A )( implicit tx: S#Tx ) : this.type

      def toIndexedSeq( implicit tx: S#Tx ) : IIdxSeq[ A ]
      def toList( implicit tx: S#Tx ) : List[ A ]
      def toSeq( implicit tx: S#Tx ) : Seq[ A ]
      def toSet( implicit tx: S#Tx ) : ISet[ A ]
   }

   trait Map[ S <: Sys[ S ], @specialized( Int, Long ) A, B ] extends SkipList[ S, A ] {
//      def isomorphicQuery( ord: Ordered[ S#Tx, A ])( implicit tx: S#Tx ) : (A, B, Int)

      def put( key: A, value: B )( implicit tx: S#Tx ) : Option[ B ]

      def remove( key: A )( implicit tx: S#Tx ) : Option[ B ]

      def +=( entry: (A, B) )( implicit tx: S#Tx ) : this.type

      def toIndexedSeq( implicit tx: S#Tx ) : IIdxSeq[ (A, B) ]
      def toList( implicit tx: S#Tx ) : List[ (A, B) ]
      def toSeq( implicit tx: S#Tx ) : Seq[ (A, B) ]
      def toSet( implicit tx: S#Tx ) : ISet[ (A, B) ]

      def floor( key: A )( implicit tx: S#Tx ) : Option[ (A, B) ]
      def ceil( key: A )(  implicit tx: S#Tx ) : Option[ (A, B) ]
   }
}
sealed trait SkipList[ S <: Sys[ S ], @specialized( Int, Long ) A ] extends Mutable[ S ] {
   /**
    * Searches for the Branch of a given key.
    *
    * @param   key   the key to search for
    * @return  `true` if the key is in the list, `false` otherwise
    */
   def contains( key: A )( implicit tx: S#Tx ) : Boolean

   // ---- stuff lost from collection.mutable.Set ----

   def -=( key: A )( implicit tx: S#Tx ) : this.type
   def isEmpty( implicit tx: S#Tx ) : Boolean
   def notEmpty( implicit tx: S#Tx ) : Boolean

   def keysIterator( implicit tx: S#Tx ) : Iterator[ S#Tx, A ]

   def debugPrint( implicit tx: S#Tx ) : String

   def write( out: DataOutput ) : Unit
   def keySerializer : TxnSerializer[ S#Tx, S#Acc, A ]

   /**
    * The number of levels in the skip list.
    */
   def height( implicit tx: S#Tx ) : Int

   /**
    * The number of keys in the skip list (size of the bottom level).
    * This operation may take up to O(n) time, depending on the implementation.
    */
   def size( implicit tx: S#Tx ) : Int

   /**
    * The ordering used for the keys of this list.
    */
   implicit def ordering : Ordering[ S#Tx, A ]

   /**
    * The minimum gap within elements of each skip level
    */
   def minGap : Int

   /**
    * The maximum gap within elements of each skip level
    */
   def maxGap : Int // = (minGap << 1) + 1
}