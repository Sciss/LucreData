/*
 *  Iterator.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package data

import collection.immutable.{IndexedSeq => Vec}
import collection.mutable
import annotation.tailrec

object Iterator {
  private final class Map[-Tx, +A, B](peer: Iterator[Tx, A], fun: A => B)
  extends Iterator[Tx, B] {
    def hasNext(implicit tx: Tx): Boolean  = peer.hasNext
    def next() (implicit tx: Tx): B        = fun(peer.next())

    override def toString = peer.toString + ".map(" + fun + ")"
  }

  def empty: Iterator[Any, Nothing] = Empty

  private object Empty extends Iterator[Any, Nothing] {
    def hasNext(implicit tx: Any): Boolean = false
    def next() (implicit tx: Any): Nothing = endReached()

    override def toString = "empty iterator"
  }

  def wrap[A](plain: collection.Iterator[A]): Iterator[Any, A] = new Wrap(plain)

  private final class Wrap[A](peer: collection.Iterator[A])
    extends Iterator[Any, A] {
    def hasNext(implicit tx: Any) = peer.hasNext

    def next()(implicit tx: Any): A = peer.next()

    override def toString = peer.toString()
  }

  private final class Filter[-Tx, A](peer: Iterator[Tx, A], p: A => Boolean)
    extends Iterator[Tx, A] {

    private var nextValue = Option.empty[A]

    @tailrec def step()(implicit tx: Tx): Unit =
      if (!peer.hasNext) {
        nextValue = None
      } else {
        val n = peer.next()
        if (p(n)) {
          nextValue = Some(n)
        } else {
          step()
        }
      }

    def hasNext(implicit tx: Tx): Boolean = nextValue.isDefined

    def next()(implicit tx: Tx): A = {
      val res = nextValue.getOrElse(endReached())
      step()
      res
    }

    override def toString = peer.toString + ".filter(" + p + ")"
  }

  private final class Collect[-Tx, A, B](peer: Iterator[Tx, A], pf: PartialFunction[A, B])
    extends Iterator[Tx, B] {

    private val pfl = pf.lift
    private var nextValue = Option.empty[B]

    @tailrec def step()(implicit tx: Tx): Unit =
      if (!peer.hasNext) {
        nextValue = None
      } else {
        val n = pfl(peer.next())
        if (n.isDefined) {
          nextValue = n
        } else {
          step()
        }
      }

    def hasNext(implicit tx: Tx): Boolean = nextValue.isDefined

    def next()(implicit tx: Tx): B = {
      val res = nextValue.getOrElse(endReached())
      step()
      res
    }

    override def toString = peer.toString + ".collect(" + pf + ")"
  }

  private final class FlatMap[-Tx, A, B](peer: Iterator[Tx, A], fun: A => Iterable[B])
    extends Iterator[Tx, B] {

    private var nextValue: collection.Iterator[B] = collection.Iterator.empty

    @tailrec def step()(implicit tx: Tx): Unit =
      if (peer.hasNext) {
        val it = fun(peer.next()).iterator
        if (it.hasNext) {
          nextValue = it
        } else {
          step()
        }
      }

    def hasNext(implicit tx: Tx): Boolean = nextValue.hasNext

    def next()(implicit tx: Tx): B = {
      val res = nextValue.next()
      if (!nextValue.hasNext) step()
      res
    }

    override def toString = peer.toString + ".flatMap(" + fun + ")"
  }
}
trait Iterator[-Tx, +A] {
  peer =>

  def hasNext(implicit tx: Tx): Boolean
  def next() (implicit tx: Tx): A

  final def foreach(fun: A => Unit)(implicit tx: Tx): Unit =
    while (hasNext) fun(next())

  final def toIndexedSeq  (implicit tx: Tx): Vec[A] = fromBuilder(Vector.newBuilder[A])
  final def toList        (implicit tx: Tx): List[A]    = fromBuilder(List.newBuilder[A])
  final def toSeq         (implicit tx: Tx): Seq[A]     = fromBuilder(Seq.newBuilder[A])
  final def toSet[B >: A] (implicit tx: Tx): Set[B]     = fromBuilder(Set.newBuilder[B])

  private def fromBuilder[To](b: mutable.Builder[A, To])(implicit tx: Tx): To = {
    while (hasNext) b += next()
    b.result()
  }

  final def toMap[T, U](implicit tx: Tx, ev: A <:< (T, U)): Map[T, U] = {
    val b = Map.newBuilder[T, U]
    while (hasNext) b += next()
    b.result()
  }

  final def map[B](fun: A => B)(implicit tx: Tx): Iterator[Tx, B] = new Iterator.Map(this, fun)

  final def flatMap[B](fun: A => Iterable[B])(implicit tx: Tx): Iterator[Tx, B] = {
    val res = new Iterator.FlatMap(this, fun)
    res.step()
    res
  }

  final def filter(p: A => Boolean)(implicit tx: Tx): Iterator[Tx, A] = {
    val res = new Iterator.Filter(this, p)
    res.step()
    res
  }

  final def collect[B](pf: PartialFunction[A, B])(implicit tx: Tx): Iterator[Tx, B] = {
    val res = new Iterator.Collect(this, pf)
    res.step()
    res
  }

  final def filterNot(p: A => Boolean)(implicit tx: Tx): Iterator[Tx, A] = {
    val res = new Iterator.Filter(this, p.andThen(!_))
    res.step()
    res
  }

  final def isEmpty (implicit tx: Tx): Boolean = !hasNext
  final def nonEmpty(implicit tx: Tx): Boolean = hasNext

  protected final def endReached(): Nothing = throw new java.util.NoSuchElementException("next on empty iterator")
}