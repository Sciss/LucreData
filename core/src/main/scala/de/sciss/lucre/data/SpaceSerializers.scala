package de.sciss.lucre
package data

import stm.ImmutableSerializer
import geom.{IntHyperCubeN, IntPointN, IntSpace, LongSquare, LongPoint2D, IntCube, IntPoint3D, IntSquare, IntPoint2D}

object SpaceSerializers {
   // ---- int space ----

   implicit object IntPoint2DSerializer extends ImmutableSerializer[ geom.IntPoint2D ] {
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

   implicit object IntSquareSerializer extends ImmutableSerializer[ geom.IntSquare ] {
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

   implicit object IntPoint3DSerializer extends ImmutableSerializer[ geom.IntPoint3D ] {
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

   implicit object IntCubeSerializer extends ImmutableSerializer[ geom.IntCube ] {
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

   import IntSpace.NDim

   implicit object IntPointNSerializer extends ImmutableSerializer[ NDim#Point ] {
      def write( v: NDim#Point, out: DataOutput ) {
         val c = v.components
         out.writeInt( c.size )
         c.foreach( out.writeInt )
      }

      def read( in: DataInput ) : NDim#Point = {
         val sz   = in.readInt()
         val c    = Vector.fill( sz )( in.readInt() )
         IntPointN( c )
      }
   }

   implicit object IntHyperCubeNSerializer extends ImmutableSerializer[ NDim#HyperCube ] {
      def write( v: NDim#HyperCube, out: DataOutput ) {
         val c = v.components
         out.writeInt( c.size )
         c.foreach( out.writeInt )
         out.writeInt( v.extent )
      }

      def read( in: DataInput ) : NDim#HyperCube = {
         val sz   = in.readInt()
         val c    = Vector.fill( sz )( in.readInt() )
         val ext  = in.readInt()
         IntHyperCubeN( c, ext )
      }
   }

   // ---- long space ----

   implicit object LongPoint2DSerializer extends ImmutableSerializer[ geom.LongPoint2D ] {
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

   implicit object LongSquareSerializer extends ImmutableSerializer[ geom.LongSquare ] {
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