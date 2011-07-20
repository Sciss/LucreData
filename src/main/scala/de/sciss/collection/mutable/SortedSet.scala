package de.sciss.collection.mutable

import collection.{ SortedSet => CSortedSet, SortedSetLike => CSortedSetLike }
import collection.generic.{ GenericCompanion, MutableSetFactory }
import collection.mutable.{ Set => MSet, SetLike => MSetLike }

//abstract class MutableSortedSetFactory[CC[A] <: SortedSet[A] with MSet[A] with MSetLike[A, CC[A]]] extends MutableSetFactory[CC]

//object SortedSet extends MutableSortedSetFactory[ SortedSet ]
object SortedSet extends MutableSetFactory[ SortedSet ] {
  def empty[A](implicit ord: Ordering[A]): SortedSet[A] = sys.error( "TODO" )
}

trait SortedSet[ A ] extends MSet[ A ] with MSetLike[ A, SortedSet[ A ]] with CSortedSet[ A ] with CSortedSetLike[ A, SortedSet[ A ]] {
  override def companion: GenericCompanion[SortedSet] = SortedSet
   /**
    * Needs to be overridden in subclasses.
    */
   override def empty: SortedSet[A] = SortedSet.empty[A]
}