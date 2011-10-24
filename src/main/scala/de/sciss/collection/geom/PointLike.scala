package de.sciss.collection.geom

trait PointLike[D <: Dim] {
   def distanceSq( that: PointLike[ D ]) : Long
   def contains( p: PointLike[ D ]) : Boolean
   def orient( b: PointLike[ D ]) : Int

   final def area : Long = 1L
}