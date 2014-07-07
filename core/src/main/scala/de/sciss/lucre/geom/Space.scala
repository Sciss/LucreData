/*
 *  Space.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
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
  //   type BigNum

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