package de.sciss

import collection.immutable.{IndexedSeq => IIdxSeq}
import annotation.tailrec
import sys.error // suckers

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

         /**
          * '... we define that a node in a tree is heavy
          *  iff it has at least two children in the tree'
          */
         def isHeavy = children.size >= 2

         /**
          * 'For any node v, let weight(v) be the number of
          *  heavy descendants to v in T'
          */
         def weight: Int = children.map( _.weight ).sum + children.count( _.isHeavy )

         /**
          * Number of nodes in the (sub)tree. Aka 'n'
          */
         def size: Int = children.map( _.size ).sum + 1

         /**
          * 'Let MH be c log^1/4 n, for some constant c'
          */
         def mh( c: Double ) : Double = math.pow( math.log( size ), 0.25 ) * c
      }

      trait MacroTree extends Tree[ MicroTree ] {

      }

      trait SimpleTree extends Tree[ SimpleTree ] {
         /**
          * Iteratively decomposes T into T', 'the tree T from which
          * all micro trees have
          * been removed, hence T' is the tree induced by the nodes
          * which are are not removed', along with the micro trees.
          * These are defined by their roots as follows: 'For any node
          * v, where weight(v) < MH and weight(parent(v)) >= MH, we say
          * v is the root in a micro tree and the nodes in the micro
          * tree and its descendents.' The iteration is performed
          * 'recursively until the root of T is included in a micro tree.
          * The level of the micro tree then equals the step in which
          * it was constructed ...'
          */
         def decompose( c: Double ) : IIdxSeq[ MicroTree ] = decomposeStep( c, 0, IIdxSeq.empty )._2

         def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree ]) : (Option[ SimpleTree ], IIdxSeq[ MicroTree ])
      }

      trait MicroTree extends SimpleTree {
         def level: Int
         def numMarkedAncestors( t: SimpleTree ) : Int
      }

      case class SimpleTreeImpl( children: IIdxSeq[ SimpleTree ]) extends SimpleTree {
         @tailrec
         final def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree ]) : (Option[ SimpleTree ], IIdxSeq[ MicroTree ]) = {
            if( weight >= mh( c )) {
               (None, res :+ MicroTreeImpl( children, step ))
            } else {
               val (mroots, rem)    = children.partition( ch => ch.weight >= ch.mh( c ))
               val micros1          = res ++ mroots.map( ch => MicroTreeImpl( ch.children, step ))
               val (rem1, micros2)  = rem.map( _.decomposeStep( c, step, micros1 )).unzip
               val rem2             = rem1.collect({ case Some( x ) => x })
               val newT             = copy( children = rem2 )
               newT.decomposeStep( c, step + 1, micros2.flatten )
            }
         }
      }

      case class MicroTreeImpl( children: IIdxSeq[ SimpleTree ], level: Int ) extends MicroTree {
         def numMarkedAncestors( t: SimpleTree ) : Int = error( "TODO" )
         def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree ]) : (Option[ SimpleTree ], IIdxSeq[ MicroTree ]) = error( "TODO" )
      }
   }
}