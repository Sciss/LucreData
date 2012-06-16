package de.sciss.collection
package txn

import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.stm.Serializer
import geom.{IntCube, IntPoint3D, IntSquare, IntPoint2D}

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

   implicit object SquareSerializer extends Serializer[ geom.IntSquare ] {
      def read( in: DataInput ) : IntSquare = {
         val cx      = in.readInt()
         val cy      = in.readInt()
         val extent  = in.readInt()
         IntSquare( cx, cy, extent )
      }

      def write( q: IntSquare, out: DataOutput ) {
         out.writeInt( q.cx )
         out.writeInt( q.cy )
         out.writeInt( q.extent )
      }
   }

   implicit object Point3DSerializer extends Serializer[ geom.IntPoint3D ] {
       def read( in: DataInput ) : IntPoint3D = {
          val x = in.readInt()
          val y = in.readInt()
          val z = in.readInt()
          IntPoint3D( x, y, z )
       }

       def write( p: IntPoint3D, out: DataOutput ) {
          out.writeInt( p.x )
          out.writeInt( p.y )
          out.writeInt( p.z )
       }
    }

    implicit object CubeSerializer extends Serializer[ geom.IntCube ] {
       def read( in: DataInput ) : IntCube = {
          val cx      = in.readInt()
          val cy      = in.readInt()
          val cz      = in.readInt()
          val extent  = in.readInt()
          IntCube( cx, cy, cz, extent )
       }

       def write( q: IntCube, out: DataOutput ) {
          out.writeInt( q.cx )
          out.writeInt( q.cy )
          out.writeInt( q.cz )
          out.writeInt( q.extent )
       }
    }
}