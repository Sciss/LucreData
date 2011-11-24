//package de.sciss.collection
//package txn.geom
//
//import de.sciss.lucrestm.{DataOutput, Writer}
//
//
//trait Point3DLike extends geom.Point3DLike with Writer
//
//final case class Point3D( x: Int, y: Int, z: Int ) extends Point3DLike with Writer {
//   def +( p: geom.Point3D ) = Point3D( x + p.x, y + p.y, z + p.z )
//   def -( p: geom.Point3D ) = Point3D( x - p.x, y - p.y, z + p.z )
//
//   def write( out: DataOutput ) {
//      out.writeInt( x )
//      out.writeInt( y )
//      out.writeInt( z )
//   }
//}
