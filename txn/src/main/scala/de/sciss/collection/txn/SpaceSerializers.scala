package de.sciss.collection
package txn

import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.stm.Serializer
import geom.{Cube, Point3D, Square, IntPoint2D}

object SpaceSerializers {
   implicit object Point2DSerializer extends Serializer[ geom.IntPoint2D ] {
      def read( in: DataInput ) : IntPoint2D = {
         val x = in.readInt()
         val y = in.readInt()
         IntPoint2D( x, y )
      }

      def write( p: IntPoint2D, out: DataOutput ) {
         out.writeInt( p.x )
         out.writeInt( p.y )
      }
   }

   implicit object SquareSerializer extends Serializer[ geom.Square ] {
      def read( in: DataInput ) : Square = {
         val cx      = in.readInt()
         val cy      = in.readInt()
         val extent  = in.readInt()
         Square( cx, cy, extent )
      }

      def write( q: Square, out: DataOutput ) {
         out.writeInt( q.cx )
         out.writeInt( q.cy )
         out.writeInt( q.extent )
      }
   }

   implicit object Point3DSerializer extends Serializer[ geom.Point3D ] {
       def read( in: DataInput ) : Point3D = {
          val x = in.readInt()
          val y = in.readInt()
          val z = in.readInt()
          Point3D( x, y, z )
       }

       def write( p: Point3D, out: DataOutput ) {
          out.writeInt( p.x )
          out.writeInt( p.y )
          out.writeInt( p.z )
       }
    }

    implicit object CubeSerializer extends Serializer[ geom.Cube ] {
       def read( in: DataInput ) : Cube = {
          val cx      = in.readInt()
          val cy      = in.readInt()
          val cz      = in.readInt()
          val extent  = in.readInt()
          Cube( cx, cy, cz, extent )
       }

       def write( q: Cube, out: DataOutput ) {
          out.writeInt( q.cx )
          out.writeInt( q.cy )
          out.writeInt( q.cz )
          out.writeInt( q.extent )
       }
    }
}