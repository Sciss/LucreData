/*
 *  Quad.scala
 *  (TreeTests)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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
 *
 *
 *  Changelog:
 */

package de.sciss.tree

case class Point( x: Int, y: Int ) {
//   def orthoDist( p: Point ) : Int = math.max( math.abs( x - p.x ), math.abs( y - p.y ))
   def +( p: Point ) = Point( x + p.x, y + p.y )
   def -( p: Point ) = Point( x - p.x, y - p.y )
}

sealed trait Quad[ V ] {
   def center: Point
   def extent: Int
}
final case class QuadEmpty[ V ]( center: Point, extent: Int ) extends Quad[ V ]
final case class QuadLeaf[ V ]( center: Point, extent: Int, point: Point, value: V ) extends Quad[ V ]
trait QuadNode[ V ] extends Quad[ V ] {
   def nw: Quad[ V ]
   def ne: Quad[ V ]
   def sw: Quad[ V ]
   def se: Quad[ V ]
   def insert( point: Point, value: V ) : QuadNode[ V ]
}
