/*
 *  Space.scala
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

import de.sciss.serial.ImmutableSerializer

object Space {
  final val bigZero = BigInt(0)
  final val bigOne  = BigInt(1)
}

/**
 * A `Space` abstracts over the number of dimensions
 * that are used for point and hypercube operations.
 *
 * Big thanks to Aleksey Nikiforov for figuring out
 * how to plug the types together...
 */
trait Space[D <: Space[D]] {
  /**
   * The point in the space
   */
  type PointLike
  /* <: Writer */
  // <: PointLike[ Self ]
  type Point <: D#PointLike

  /**
   * The square or hypercube in the space.
   */
  type HyperCubeLike <: geom.HyperCube[D]
  type HyperCube     <: D#HyperCubeLike

  //   /**
  //    * Represents larger values from multiplications
  //    * (e.g. areas, squared distances).
  //    */
  //   @specialized( Int, Long ) type BigNum

  /**
   * Given that the space is limited, this represents the farthest
   * point in the space, typically which each coordinate component
   * equal to `Int.MaxValue`.
   */
  def maxPoint: D#Point // Like

  /**
   * The number of dimensions in the space.
   */
  def dim: Int

  implicit def lexicalOrder: Ordering[D#PointLike]

  implicit def pointSerializer    : ImmutableSerializer[D#Point    ]
  implicit def hyperCubeSerializer: ImmutableSerializer[D#HyperCube]
}