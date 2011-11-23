package de.sciss.collection
package txn.geom

import de.sciss.lucrestm.{DataOutput, Writer}

trait CubeLike extends geom.CubeLike with Writer

final case class Cube( cx: Int, cy: Int, cz: Int, extent: Int )
extends CubeLike with Writer {
   def write( out: DataOutput ) {
      out.writeInt( cx )
      out.writeInt( cy )
      out.writeInt( cz )
      out.writeInt( extent )
   }
}