package de.sciss.collection
package txn.geom

import de.sciss.lucrestm.{DataInput, Writer}

object Space {
   sealed trait TwoDim extends Space[ TwoDim ] {
      type Point        = Point2DLike
      type HyperCube    = SquareLike
   }
   object TwoDim extends TwoDim {
      val maxPoint      = Point2D( Int.MaxValue, Int.MaxValue )
      val dim           = 2

      def readPoint( in: DataInput ) : Point2D = {
         val x = in.readInt()
         val y = in.readInt()
         Point2D( x, y )
      }

      def readHyperCube( in: DataInput ) : Square = {
         val cx      = in.readInt()
         val cy      = in.readInt()
         val extent  = in.readInt()
         Square( cx, cy, extent )
      }
   }

   sealed trait ThreeDim extends Space[ ThreeDim ] {
      type Point           = Point3DLike
      type HyperCube       = CubeLike
   }
   object ThreeDim extends ThreeDim {
      val maxPoint   = Point3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
      val dim        = 3

      val bigZero    = BigInt( 0 )

      def readPoint( in: DataInput ) : Point3D = {
         val x = in.readInt()
         val y = in.readInt()
         val z = in.readInt()
         Point3D( x, y, z )
      }

      def readHyperCube( in: DataInput ) : Cube = {
         val cx      = in.readInt()
         val cy      = in.readInt()
         val cz      = in.readInt()
         val extent  = in.readInt()
         Cube( cx, cy, cz, extent )
      }
   }
}
trait Space[ Self <: Space[ Self ]] extends geom.Space[ Self ] {
   type Point <: Writer
   type HyperCube <: geom.HyperCube[ Self ] with Writer

   def readPoint( in: DataInput ) : Self#Point
   def readHyperCube( in: DataInput ) : Self#HyperCube
}