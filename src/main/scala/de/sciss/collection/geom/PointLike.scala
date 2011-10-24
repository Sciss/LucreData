package de.sciss.collection.geom

trait PointLike[ D <: Dim[ D ]] {
   def distanceSq( that: D#Point ) : Long
   def contains( p: D#Point ) : Boolean
   def orient( b: D#Point ) : Int

   def area : Long // = 1L
}