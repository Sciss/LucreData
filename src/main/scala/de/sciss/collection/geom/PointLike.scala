package de.sciss.collection.geom

trait PointLike[ D <: Dim[ D ]] {
   def distanceSq( that: D#PointType ) : Long
   def contains( p: D#PointType ) : Boolean
   def orient( b: D#PointType ) : Int

   def area : Long // = 1L
}