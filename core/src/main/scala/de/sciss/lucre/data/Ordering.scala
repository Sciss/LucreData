/*
 *  Ordering.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.lucre
package data

import scala.{specialized => spec}
//import stm.{SpecGroup => ialized}

object Ordering {
   implicit object Int extends Ordering[ Any, scala.Int ] {
      def compare( a: scala.Int, b: scala.Int )( implicit tx: Any ) : scala.Int                = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Int, b: scala.Int )( implicit tx: Any ) : Boolean              = a < b
      override def lteq( a: scala.Int, b: scala.Int )( implicit tx: Any ) : Boolean            = a <= b
      override def gt( a: scala.Int, b: scala.Int )( implicit tx: Any ) : Boolean              = a > b
      override def gteq( a: scala.Int, b: scala.Int )( implicit tx: Any ) : Boolean            = a >= b
      override def equiv( a: scala.Int, b: scala.Int )( implicit tx: Any ) : Boolean           = a == b
      override def nequiv( a: scala.Int, b: scala.Int )( implicit tx: Any ) : Boolean          = a != b
      override def max( a: scala.Int, b: scala.Int )( implicit tx: Any ) : scala.Int           = if( a >= b ) a else b
      override def min( a: scala.Int, b: scala.Int )( implicit tx: Any ) : scala.Int           = if( a < b ) a else b
   }

   implicit object Float extends Ordering[ Any, scala.Float ] {
      def compare( a: scala.Float, b: scala.Float )( implicit tx: Any ) : scala.Int            = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Float, b: scala.Float )( implicit tx: Any ) : Boolean          = a < b
      override def lteq( a: scala.Float, b: scala.Float )( implicit tx: Any ) : Boolean        = a <= b
      override def gt( a: scala.Float, b: scala.Float )( implicit tx: Any ) : Boolean          = a > b
      override def gteq( a: scala.Float, b: scala.Float )( implicit tx: Any ) : Boolean        = a >= b
      override def equiv( a: scala.Float, b: scala.Float )( implicit tx: Any ) : Boolean       = a == b
      override def nequiv( a: scala.Float, b: scala.Float )( implicit tx: Any ) : Boolean      = a != b
      override def max( a: scala.Float, b: scala.Float )( implicit tx: Any ) : scala.Float     = if( a >= b ) a else b
      override def min( a: scala.Float, b: scala.Float )( implicit tx: Any ) : scala.Float     = if( a < b ) a else b
   }

   implicit object Long extends Ordering[ Any, scala.Long ] {
      def compare( a: scala.Long, b: scala.Long )( implicit tx: Any ) : scala.Int              = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Long, b: scala.Long )( implicit tx: Any ) : Boolean            = a < b
      override def lteq( a: scala.Long, b: scala.Long )( implicit tx: Any ) : Boolean          = a <= b
      override def gt( a: scala.Long, b: scala.Long )( implicit tx: Any ) : Boolean            = a > b
      override def gteq( a: scala.Long, b: scala.Long )( implicit tx: Any ) : Boolean          = a >= b
      override def equiv( a: scala.Long, b: scala.Long )( implicit tx: Any ) : Boolean         = a == b
      override def nequiv( a: scala.Long, b: scala.Long )( implicit tx: Any ) : Boolean        = a != b
      override def max( a: scala.Long, b: scala.Long )( implicit tx: Any ) : scala.Long        = if( a >= b ) a else b
      override def min( a: scala.Long, b: scala.Long )( implicit tx: Any ) : scala.Long        = if( a < b ) a else b
   }

   implicit object Double extends Ordering[ Any, scala.Double ] {
      def compare( a: scala.Double, b: scala.Double )( implicit tx: Any ) : scala.Int          = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Double, b: scala.Double )( implicit tx: Any ) : Boolean        = a < b
      override def lteq( a: scala.Double, b: scala.Double )( implicit tx: Any ) : Boolean      = a <= b
      override def gt( a: scala.Double, b: scala.Double )( implicit tx: Any ) : Boolean        = a > b
      override def gteq( a: scala.Double, b: scala.Double )( implicit tx: Any ) : Boolean      = a >= b
      override def equiv( a: scala.Double, b: scala.Double )( implicit tx: Any ) : Boolean     = a == b
      override def nequiv( a: scala.Double, b: scala.Double )( implicit tx: Any ) : Boolean    = a != b
      override def max( a: scala.Double, b: scala.Double )( implicit tx: Any ) : scala.Double  = if( a >= b ) a else b
      override def min( a: scala.Double, b: scala.Double )( implicit tx: Any ) : scala.Double  = if( a < b ) a else b
   }

  implicit def fromMath[A](implicit underlying: math.Ordering[A]): Ordering[Any, A] =
    new MathWrapper[A](underlying)

  implicit def fromOrdered[Tx, A <: Ordered[Tx, A]]: Ordering[Tx, A] = new OrderedWrapper[Tx, A]

  private final class MathWrapper[A](underlying: math.Ordering[A]) extends Ordering[Any, A] {
    def compare(a: A, b: A)(implicit tx: Any): Int = underlying.compare(a, b)
  }

  private final class OrderedWrapper[Tx, A <: Ordered[Tx, A]] extends Ordering[Tx, A] {
    def compare(a: A, b: A)(implicit tx: Tx): Int = a.compare(b)
  }
}

trait Ordering[-Tx, @spec(ValueSpec) A] {
  def compare(a: A, b: A)(implicit tx: Tx): Int
  def lt     (a: A, b: A)(implicit tx: Tx): Boolean  = compare(a, b)  < 0
  def lteq   (a: A, b: A)(implicit tx: Tx): Boolean  = compare(a, b) <= 0
  def gt     (a: A, b: A)(implicit tx: Tx): Boolean  = compare(a, b)  > 0
  def gteq   (a: A, b: A)(implicit tx: Tx): Boolean  = compare(a, b) >= 0
  def equiv  (a: A, b: A)(implicit tx: Tx): Boolean  = compare(a, b) == 0
  def nequiv (a: A, b: A)(implicit tx: Tx): Boolean  = compare(a, b) != 0
  def max    (a: A, b: A)(implicit tx: Tx): A        = if (compare(a, b) >= 0) a else b
  def min    (a: A, b: A)(implicit tx: Tx): A        = if (compare(a, b)  < 0) a else b
}