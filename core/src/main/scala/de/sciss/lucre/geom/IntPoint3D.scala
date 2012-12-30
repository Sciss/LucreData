/*
 *  IntPoint3D.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package geom

trait IntPoint3DLike {
   import IntSpace.ThreeDim._

   def x: Int
   def y: Int
   def z: Int

   def distanceSq( that: PointLike ) : BigInt = {
      val dx = that.x.toLong - x.toLong
      val dy = that.y.toLong - y.toLong
      val dz = that.z.toLong - z.toLong
      BigInt( dx * dx + dy * dy ) + BigInt( dz * dz )
   }
}

final case class IntPoint3D( x: Int, y: Int, z: Int ) extends IntPoint3DLike {
   def +( p: IntPoint3D ) = IntPoint3D( x + p.x, y + p.y, z + p.z )
   def -( p: IntPoint3D ) = IntPoint3D( x - p.x, y - p.y, z - p.z )
}
