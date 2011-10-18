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

package de.sciss.collection
package mutable

import collection.generic.CanBuildFrom
import collection.mutable.{Builder => MBuilder, Set => MSet, SetBuilder => MSetBuilder, SetLike => MSetLike}

object SkipList {
   def empty[ A ]( implicit ord: Ordering[ A ], m: MaxKey[ A ]): SkipList[ A ] = LLSkipList.empty[ A ]

   private type CC[ A ] = SkipList[ A ]
   private type Coll = CC[ _ ]

   implicit def canBuildFrom[ A : Ordering : MaxKey ] : CanBuildFrom[ Coll, A, CC[ A ]] = new SkipListCanBuildFrom

   private class SkipListCanBuildFrom[ A : Ordering : MaxKey ] extends CanBuildFrom[ Coll, A, CC[ A ]] {
      def apply( from: Coll ) = newBuilder[ A ]
      def apply() = newBuilder[ A ]
   }

   def newBuilder[ A : Ordering : MaxKey ]: MBuilder[ A, CC[ A ]] = new MSetBuilder( empty[ A ])

   /**
    * A trait for observing the promotion and demotion of a key
    * in the skip list's level hierarchy
    */
   trait KeyObserver[ /* @specialized( Int, Long ) */ -A ] {
      /**
       * Notifies the observer that a given key
       * is promoted to a higher (more sparse) level
       */
      def keyUp( key : A ) : Unit
      /**
       * Notifies the observer that a given key
       * is demoted to a lower (more dense) level
       */
      def keyDown( key : A ) : Unit
   }

   object NoKeyObserver extends KeyObserver[ Any ] {
      def keyUp( key : Any ) {}
      def keyDown( key : Any ) {}
   }
}
// XXX java.lang.NullPointerException at scala.tools.nsc.typechecker.Namers$Namer.enterSym(Namers.scala:404)
// bra bra bra. fucking scala specialization -- completely failed project
trait SkipList[ /* @specialized( Int, Long ) */ A ]
extends MSet[ A ] with MSetLike[ A, SkipList[ A ]] {

   /**
    * Needs to be overridden in subclasses.
    */
   override def empty: SkipList[ A ] = SkipList.empty[ A ]( ordering, MaxKey( maxKey ))

   /**
    * Searches for the Branch of a given key.
    *
    * @param   v  the key to search for
    * @return  `true` if the key is in the list, `false` otherwise
    */
   def contains( v: A ) : Boolean

   /**
    * Finds the nearest item equal or greater
    * than an unknown item from an isomorphic
    * set. The isomorphism is represented by
    * a comparison function which guides the
    * binary search.
    *
    * @param   compare  a function that guides the search.
    *    should return -1 if the argument is smaller
    *    than the search key, 0 if both are equivalent,
    *    or 1 if the argument is greater than the search key.
    *    E.g., using some mapping, the function could look
    *    like `mapping.apply(_).compare(queryKey)`
    *
    * @return  the nearest item, or the maximum item
    */
   def isomorphicQuery( compare: A => Int ) : A

   /**
    * Inserts a new key into the list.
    *
    * @param   v  the key to insert
    * @return  `true` if the key was successfully inserted,
    *          `false` if a node with the given key already existed
    */
   def add( v: A ) : Boolean

   /**
    * The number of levels in the skip list.
    */
   def height : Int

   /**
    * The number of keys in the skip list (size of the bottom level).
    * This operation may take up to O(n) time, depending on the implementation.
    */
   def size : Int

   /**
    * The 'maximum' key. In the ordering of the skip list,
    * no is allowed to be greater or equal to this maximum key.
    */
   def maxKey : A

   implicit def maxKeyHolder : MaxKey[ A ] = MaxKey( maxKey )

   /**
    * The ordering used for the keys of this list.
    */
   implicit def ordering : Ordering[ A ]

   /**
    * The minimum gap within elements of each skip level
    */
   def minGap : Int

   /**
    * The maximum gap within elements of each skip level
    */
   def maxGap : Int = (minGap << 1) + 1
}