//package de.sciss.collection
//package txn.geom
//
//import de.sciss.lucrestm.{DataOutput, Writer}
//
//
//trait SquareLike extends geom.SquareLike with Writer
//
//final case class Square( cx: Int, cy: Int, extent: Int ) extends SquareLike with Writer {
//   def write( out: DataOutput ) {
//      out.writeInt( cx )
//      out.writeInt( cy )
//      out.writeInt( extent )
//   }
//}