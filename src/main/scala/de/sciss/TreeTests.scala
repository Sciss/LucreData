package de.sciss

import collection.immutable.{IndexedSeq => IIdxSeq}

object TreeTests {
   def main( args: Array[ String ]) : Unit = staticTest

   /**
    * Alstrup et al - Marked Ancestor Problems
    * ch. 5 -- Algorithms for Static Trees
    */
   def staticTest {
      trait Tree[ C <: Tree[ _ ]] {
         def children: IIdxSeq[ C ]
         def isLeaf = children.isEmpty
//         def appendChild( t: Tree ) : Unit
         def isHeavy = children.size >= 2
         def weight: Int = children.map( _.weight ).sum + children.count( _.isHeavy )
         def size: Int = children.map( _.size ).sum + 1
         def MH( c: Double ) : Double = math.pow( math.log( size ), 0.25 ) * c
      }

      trait MacroTree extends Tree[ MicroTree ] {

      }

      trait SimpleTree extends Tree[ SimpleTree ] {
      }

      trait MicroTree extends SimpleTree {
         def level: Int
         def numMarkedAncestors( t: SimpleTree ) : Int
      }
   }
}