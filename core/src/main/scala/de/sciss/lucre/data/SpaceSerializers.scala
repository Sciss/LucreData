package de.sciss.lucre
package data

import stm.Serializer
import geom.{LongSquare, LongPoint2D, IntCube, IntPoint3D, IntSquare, IntPoint2D}

object SpaceSerializers {
   // ---- int space ----

   implicit object IntPoint2DSerializer extends Serializer[ geom.IntPoint2D ] {
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

   implicit object IntSquareSerializer extends Serializer[ geom.IntSquare ] {
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

   implicit object IntPoint3DSerializer extends Serializer[ geom.IntPoint3D ] {
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

   implicit object IntCubeSerializer extends Serializer[ geom.IntCube ] {
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

   // ---- long space ----

   implicit object LongPoint2DSerializer extends Serializer[ geom.LongPoint2D ] {
      def read( in: DataInput ) : LongPoint2D = {
         val x = in.readLong()
         val y = in.readLong()
         LongPoint2D( x, y )
      }

      def write( p: LongPoint2D, out: DataOutput ) {
         out.writeLong( p.x )
         out.writeLong( p.y )
      }
   }

   implicit object LongSquareSerializer extends Serializer[ geom.LongSquare ] {
      def read( in: DataInput ) : LongSquare = {
         val cx      = in.readLong()
         val cy      = in.readLong()
         val extent  = in.readLong()
         LongSquare( cx, cy, extent )
      }

      def write( q: LongSquare, out: DataOutput ) {
         out.writeLong( q.cx )
         out.writeLong( q.cy )
         out.writeLong( q.extent )
      }
   }
}