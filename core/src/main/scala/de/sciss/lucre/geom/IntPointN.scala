package de.sciss.lucre.geom

import collection.immutable.{IndexedSeq => IIdxSeq}
import IntSpace.NDim

trait IntPointNLike {
   def components: IIdxSeq[ Int ]
   final def apply( idx: Int ) : Int = components( idx )
   final def dim : Int = components.size

   final def distanceSq( that: NDim#PointLike ) : BigInt = {
      var sqrSum = Space.bigZero
      var idx = 0; while( idx < dim ) {
         val delta = that( idx ) - this( idx )
         sqrSum += delta * delta
      idx += 1 }
      sqrSum
   }
}

final case class IntPointN( components: IIdxSeq[ Int ]) extends IntPointNLike {
//   if( components.size != dim ) throw new IllegalArgumentException( "Expected " + dim + " components: " + components )

   def +( that: NDim#Point ) = IntPointN( IIdxSeq.tabulate( dim )( idx => this( idx ) + that( idx )))
   def -( that: NDim#Point ) = IntPointN( IIdxSeq.tabulate( dim )( idx => this( idx ) - that( idx )))
}

