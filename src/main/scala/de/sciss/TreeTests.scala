package de.sciss

import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import annotation.tailrec
import sys.error
import java.io.{FileOutputStream, OutputStreamWriter, File}

// suckers

object TreeTests {
   def main( args: Array[ String ]) : Unit = staticTest

   /**
    * Alstrup et al - Marked Ancestor Problems
    * ch. 5 -- Algorithms for Static Trees
    */
   def staticTest {
      trait Tree[ L, C <: Tree[ L, _ ]] {
         def label : L
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

//      trait MacroTree extends Tree[ MicroTree ] {
//
//      }

      trait SimpleTree[ L ] extends Tree[ L, SimpleTree[ L ]] {
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
         def decompose( c: Double ) : IIdxSeq[ MicroTree[ L ]] = {
            var res        = IIdxSeq.empty[ MicroTree[ L ]]
            var keepGoing  = true
            var t          = this
            var step       = 0
            while( keepGoing ) {
               val (newTO, micros) = t.decomposeStep( c, step, IIdxSeq.empty )
               res ++= micros
               newTO match {
                  case Some( newT ) =>
                     t = newT
                     step += 1
                  case None =>
                     keepGoing = false
               }
            }
            res
         }

         def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree[ L ]]) : (Option[ SimpleTree[ L ]], IIdxSeq[ MicroTree[ L ]])

         def toDot( edgeTest: (SimpleTree[ L ], SimpleTree[ L ]) => Boolean = (_, _) => true ) : String = {
            var sb = new StringBuilder()
            sb.append( "digraph Tree {\n" )
//            children.foldLeft( 0 )( (id, ch) => ch.appendDot( 0, id, sb ))
            appendDot( (this, -1), 0, sb, edgeTest )
            sb.append( "}\n" )
            sb.toString()
         }

         protected def appendDot( p: (SimpleTree[ L ], Int), id: Int, sb: StringBuilder, edgeTest: (SimpleTree[ L ], SimpleTree[ L ]) => Boolean ) : Int = {
            sb.append( "  " + id + " [label=\"" + label.toString + " : " + weight + " : " + (math.round( mh( 1 ) * 100 ) * 0.01) +"\"]\n" )
            val (parent, pid) = p
            if( pid >= 0 ) {
               sb.append( "  " + pid + " -> " + id + (if( edgeTest( parent, this )) "\n" else " [style=dotted]\n") )
            }
            children.foldLeft( id + 1 )( (cid, ch) => ch.appendDot( (this, id), cid, sb, edgeTest ))
         }
      }

      trait MicroTree[ L ] extends SimpleTree[ L ] {
         def level: Int
         def numMarkedAncestors( t: SimpleTree[ L ]) : Int
      }

      case class SimpleTreeImpl[ L ]( label: L, children: IIdxSeq[ SimpleTree[ L ]]) extends SimpleTree[ L ] {
         // @tailrec
         final def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree[ L ]]) : (Option[ SimpleTree[ L ]], IIdxSeq[ MicroTree[ L ]]) = {
            if( weight < mh( c )) {
               (None, res :+ MicroTreeImpl( label, children, step ))
            } else {
               val (mroots, rem)    = children.partition( ch => ch.weight < ch.mh( c ))
//               val oldSz            = res.size
               val micros1          = res ++ mroots.map( ch => MicroTreeImpl( label, ch.children, step ))
//               val (rem1, micros2)  = rem.map( _.decomposeStep( c, step, micros1 )).unzip
//               val rem2             = rem1.collect({ case Some( x ) => x })
//               val newT             = copy( children = rem2 )
//               newT.decomposeStep( c, step + 1, micros2.flatten )
               val (rem4, micros4)  = rem.foldLeft( (IIdxSeq.empty[ SimpleTree[ L ]], micros1) )({ case ((rem1, micros2), ch) =>
                  val (remo, micros3) = ch.decomposeStep( c, step, micros2 )
                  val rem3 = remo match {
                     case Some( rem2 ) => rem1 :+ rem2
                     case None         => rem1
                  }
                  (rem3, micros3)
               })
               val newT             = copy( children = rem4 )
//               if( micros4.size == oldSz ) {
                  (Some( newT ), micros4)
//               }
//               newT.decomposeStep( c, step + 1, micros4 )
            }
         }
      }

      case class MicroTreeImpl[ L ]( label: L, children: IIdxSeq[ SimpleTree[ L ]], level: Int ) extends MicroTree[ L ] {
         def numMarkedAncestors( t: SimpleTree[ L ]) : Int = error( "TODO" )
         def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree[ L ]]) : (Option[ SimpleTree[ L ]], IIdxSeq[ MicroTree[ L ]]) = error( "TODO" )
      }

      def createRandomTree( depth: Int, minChildren: Int = 0, maxChildren: Int = 4 ) : SimpleTree[ Int ] = {
         require( minChildren >= 0 && maxChildren >= minChildren )
         var i = 0
         def iter( d: Int ) : SimpleTree[ Int ] = {
            val numCh   = if( d == 0 ) 0 else util.Random.nextInt( maxChildren - minChildren + 1 )
            val subs    = IIdxSeq.fill( numCh )( iter( d - 1 ))
            val label   = i
            i += 1
            SimpleTreeImpl( label, subs )
         }
         iter( depth )
      }

      val t    = createRandomTree( 4 )
//      val m    = t.decompose( 3.0 / math.log(2) )
//      val mr: Set[ Int ] = m.map( _.label )( breakOut )
//      println( "micro roots: " + mr )
//      val str  = t.toDot { (p, c) => !mr.contains( c.label )}
      val str  = t.toDot()
      val f    = new File( new File( util.Properties.userHome, "Desktop" ), "tree.dot" )
      val w    = new OutputStreamWriter( new FileOutputStream( f ), "UTF-8" )
      w.write( str )
      w.close()
   }
}