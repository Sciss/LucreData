package de.sciss.collection.geom

trait PointLike[ D <: Space[ D ]] {
   def distanceSq( that: D#Point ) : Long
   def contains( p: D#Point ) : Boolean
   def orient( b: D#Point ) : Int

   def area : Long // = 1L
}