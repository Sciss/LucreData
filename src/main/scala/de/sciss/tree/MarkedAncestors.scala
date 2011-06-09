/*
 *  MarkedAncestors.scala
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

import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import sys.error
import java.io.{FileOutputStream, OutputStreamWriter, File}

// Note: It was not possible to get clarification from the
// original paper authors regarding the division of
// the micro trees into dynamic paths; the implementation
// is thus abandoned at this stage.
object MarkedAncestors extends App {
   staticTest

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

         def numChildren = children.size

         /**
          * '... we define that a node in a tree is heavy
          *  iff it has at least two children in the tree'
          */
         def isHeavy = numChildren >= 2

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
//                     keepGoing = false
                  case None =>
                     keepGoing = false
               }
            }
            res
         }

         def decomposeStep( c: Double, step: Int, res: IIdxSeq[ MicroTree[ L ]]) : (Option[ SimpleTree[ L ]], IIdxSeq[ MicroTree[ L ]])

         def toDot( nodeLabel: SimpleTree[ L ] => String = n => ("label=" + n.label),
                    edgeLabel: (SimpleTree[ L ], SimpleTree[ L ]) => Option[ String ] = (_, _) => None ) : String = {
            var sb = new StringBuilder()
            sb.append( "digraph Tree {\n" )
//            children.foldLeft( 0 )( (id, ch) => ch.appendDot( 0, id, sb ))
            appendDot( (this, -1), 0, sb, nodeLabel, edgeLabel )
            sb.append( "}\n" )
            sb.toString()
         }

         protected def appendDot( p: (SimpleTree[ L ], Int), id: Int, sb: StringBuilder,
                                  nodeLabel: SimpleTree[ L ] => String, edgeLabel: (SimpleTree[ L ], SimpleTree[ L ]) => Option[String] ) : Int = {
            sb.append( "  " + id + " [" + nodeLabel( this ) + "]\n" )

            val (parent, pid) = p
            if( pid >= 0 ) {
               sb.append( "  " + pid + " -> " + id + (edgeLabel( parent, this ) match {
                  case Some( str ) => " [" + str + "]\n"
                  case None => "\n"
               }))
            }
            children.foldLeft( id + 1 )( (cid, ch) => ch.appendDot( (this, id), cid, sb, nodeLabel, edgeLabel ))
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
//if( mroots.nonEmpty ) println( "mroots : " + mroots.map( _.label ))
//               val oldSz            = res.size
               val micros1          = res ++ mroots.map( ch => MicroTreeImpl( ch.label, ch.children, step ))
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
            val numCh   = if( d == 0 ) 0 else util.Random.nextInt( maxChildren - minChildren + 1 ) + minChildren
            val subs    = IIdxSeq.fill( numCh )( iter( d - 1 ))
            val label   = i
            i += 1
            SimpleTreeImpl( label, subs )
         }
         iter( depth )
      }

      //util.Random.setSeed( 10L )
      val depth         = 23
      val minChildren   = 1
      val maxChildren   = 2
      val t    = createRandomTree( depth, minChildren, maxChildren )
      val c    = 2.0
      val m    = t.decompose( c ) // 3.0 / math.log(2) )
      val mr: Set[ Int ] = m.map( _.label )( breakOut )
      val ls   = {
         var res = Map.empty[ Int, Int ]
         def gugu( lvl: Int, t: SimpleTree[ Int ]) {
            res += t.label -> lvl
            t.children.foreach( gugu( lvl, _ ))
         }
         m.foreach( mt => gugu( mt.level, mt ))
         res
      }
      val numLevels = ls.values.toSet.max + 1
println( "n = " + t.size + "; depth = " + depth + "; numLevels = " + numLevels + "; max-size(micro) = " + m.maxBy( _.size ).size )
      val ns   = {
         var res = Map.empty[ Int, SimpleTree[ Int ]]
         def gugu( t: SimpleTree[ Int ]) {
            res += t.label -> t
            t.children.foreach( gugu( _ ))
         }
         m.foreach( mt => gugu( mt ))
         res
      }

      println( "micro roots: " + mr )
      val str  = t.toDot( nodeLabel = { n0 =>
         val n = ns( n0.label )
         val lb = "label=\"" + n.label + (if( n.numChildren == 0 ) "" else (" : " + n.weight + " ~ " + {
               val i = (n.mh( c ) * 100 + 0.5).toInt
               (i / 100).toString + "." + (i % 100).toString
            })) + "\""
         val fill = "style=filled,fillcolor=\"" + (ls( n.label ).toDouble / numLevels) + ",0.25,1.0\""
         lb + "," + fill
      }, edgeLabel = { (p, c) =>
         if( mr.contains( c.label )) Some( "style=dotted" ) else None
      })
//      val str  = t.toDot()
      val f    = new File( new File( util.Properties.userHome, "Desktop" ), "tree.dot" )
      val w    = new OutputStreamWriter( new FileOutputStream( f ), "UTF-8" )
      w.write( str )
      w.close()
   }
}