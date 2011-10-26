//package de.sciss.collection.geom
//
//trait PointLike[ D <: Space[ D ]] {
//   /**
//    * The dot product of the vector formed
//    * by this point and `that` point with itself.
//    * In other words,
//    * the 'squared' euclidean distance from this
//    * point to `that` point. In 2D, squared is
//    * literally squared, e.g. (that.x - this.x)^2 + (that.y - this.y)^2.
//    * For 3D, (that.x - this.x)^2 + (that.y - this.y)^2 + (that.z - this.z)^2
//    */
////   def distanceSq( that: D#Point ) : D#Large
////   def contains( p: D#Point ) : Boolean
////   def orient( b: D#Point ) : Int
//
////   def area : Long // = 1L
//}