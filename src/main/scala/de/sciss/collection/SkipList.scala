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

import collection.mutable.{Set => MSet}

object SkipList {
   /**
    * A trait for observing the promotion and demotion of a key
    * in the skip list's level hierarchy
    */
   trait KeyObserver[ @specialized( Int, Long ) A ] {
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

   final class NoKeyObserver[ @specialized( Int, Long ) A ] extends KeyObserver[ A ] {
      def keyUp( key : A ) {}
      def keyDown( key : A ) {}
   }
}
trait SkipList[ @specialized( Int, Long ) A ] extends MSet[ A ] {
   /**
    * Searches for the Branch of a given key.
    *
    * @param   v  the key to search for
    * @return  `true` if the key is in the list, `false` otherwise
    */
   def contains( v: A ) : Boolean

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

   /**
    * The ordering used for the keys of this list.
    */
   def ordering : Ordering[ A ]

   /**
    * The minimum gap within elements of each skip level
    */
   def minGap : Int

   /**
    * The maximum gap within elements of each skip level
    */
   def maxGap : Int = (minGap << 1) + 1
}