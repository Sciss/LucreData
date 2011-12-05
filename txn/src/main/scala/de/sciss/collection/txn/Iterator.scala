/*
 *  Iterator.scala
 *  (LucreData)
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.mutable.Builder

object Iterator {
   private final class Map[ -Tx,
      @specialized( Unit, Boolean, Int, Float, Long, Double ) +A,
      @specialized( Unit, Boolean, Int, Float, Long, Double ) B ]( peer: Iterator[ Tx, A ], fun: A => B )
   extends Iterator[ Tx, B ] {
      def hasNext = peer.hasNext
      def next()( implicit tx: Tx ) : B = fun( peer.next() )
   }

//   private final class Filter[ -Tx, @specialized( Unit, Boolean, Int, Float, Long, Double ) +A ]( peer: Iterator[ Tx, A ], p: A => Boolean )
//   extends Iterator[ Tx, A ] {
//      def hasNext = peer.hasNext
//      def next()( implicit tx: Tx ) : A = fun( peer.next() )
//   }

//   private final class Take[ -Tx, @specialized( Unit, Boolean, Int, Float, Long, Double ) +A ]( peer: Iterator[ Tx, A ], n: Int )
//   extends Iterator[ Tx, A ] {
//      private var left = n
//      def hasNext = peer.hasNext && (left > 0)
//      def next()( implicit tx: Tx ) : A = fun( peer.next() )
//   }
}
trait Iterator[ -Tx, @specialized( Unit, Boolean, Int, Float, Long, Double ) +A ] {
   peer =>

   def hasNext : Boolean
   def next()( implicit tx: Tx ) : A

   final def toIndexedSeq( implicit tx: Tx ) : IIdxSeq[ A ] = fromBuilder( IIdxSeq.newBuilder[ A ])
   final def toList( implicit tx: Tx ) : List[ A ] = fromBuilder( List.newBuilder[ A ])
   final def toSeq( implicit tx: Tx ) : Seq[ A ] = fromBuilder( Seq.newBuilder[ A ])
   final def toSet[ B >: A ]( implicit tx: Tx ) : Set[ B ] = fromBuilder( Set.newBuilder[ B ])

   private def fromBuilder[ To ]( b: Builder[ A, To ])( implicit tx: Tx ) : To = {
      while( hasNext ) b += next()
      b.result()
   }

   final def toMap[ T, U ]( implicit tx: Tx, ev: A <:< (T, U) ) : Map[ T, U ] = {
      val b = Map.newBuilder[ T, U ]
      while( hasNext ) b += next()
      b.result()
   }

   final def map[ B ]( fun: A => B ) : Iterator[ Tx, B ]      = new Iterator.Map( this, fun )
//   final def filter( p: A => Boolean ) : Iterator[ Tx, A ]    = new Iterator.Filter( this, p )
//   final def filterNot( p: A => Boolean ) : Iterator[ Tx, A ] = new Iterator.FilterNot( this, p )
//   final def take( n: Int ) : Iterator[ A ] = new Iterator.Take( this, n )

   final def isEmpty : Boolean = !hasNext
}