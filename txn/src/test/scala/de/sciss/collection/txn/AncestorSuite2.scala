package de.sciss.collection
package txn

import org.scalatest.{GivenWhenThen, FeatureSpec}
import java.io.File
import de.sciss.lucrestm.{BerkeleyDB, Sys, InMemory}
import annotation.tailrec
import collection.immutable.IntMap

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.txn.AncestorSuite2
 * }}
 */
class AncestorSuite2 extends FeatureSpec with GivenWhenThen {
   val NUM1                   = 10000   // gets frickin slow, hopefully online in the control structure 10000     // 933 // 10000
   val MARK_LIVE              = 0.25      // percentage of elements marked (0 to 1)
//   val MARK_POST              = 0.25
   val RETRO_CHILD_PERCENTAGE = 0.2       // from those elements marked, amount which are inserted as retro-children (0 to 1)
   val RETRO_PARENT_PERCENTAGE= 0.2       // from those elements marked, amount which are inserted as retro-parents (0 to 1)
//   val INCR_TEST_STEP         = 10

   // when the map is tested in the incremental build-up, only perform test every x iterations,
   // where x is calculated as INCR_TEST_FACTOR.pow(n)
   val INCR_TEST_FACTOR       = 1.2

   val INMEMORY               = true
   val DATABASE               = true

   def seed : Long            = 444L

   var verbose                = false

   if( INMEMORY ) {
      withSys[ InMemory ]( "Mem", () => new InMemory, (_, _) => () )
   }
   if( DATABASE ) {
      withSys[ BerkeleyDB ]( "BDB", () => {
         val dir     = File.createTempFile( "ancestor", "_database" )
         dir.delete()
         dir.mkdir()
         val f       = new File( dir, "data" )
         println( f.getAbsolutePath )
         BerkeleyDB.open( f )
      }, { case (bdb, success) =>
//         if( success ) {
//            val sz = bdb.numUserRecords
////            if( sz != 0 ) bdb.atomic( implicit tx => bdb.debugListUserRecords() ).foreach( println )
////            assert( sz == 0, "Final DB user size should be 0, but is " + sz )
//         }
         bdb.close()
      })
   }

   def withSys[ S <: Sys[ S ]]( sysName: String, sysCreator: () => S, sysCleanUp: (S, Boolean) => Unit )
                              ( implicit smf: Manifest[ S ]) {

      def scenarioWithTime( name: String, descr: String )( body: => Unit ) {
         scenario( descr ) {
            val t1 = System.currentTimeMillis()
            body
            val t2 = System.currentTimeMillis()
            println( "For " + name + "(" + sysName + ") the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
         }
      }


      feature( "Marked ancestor lookup is verified on a dedicated structore in " + sysName ) {
         scenarioWithTime( "Marked-Ancestors", "Verifying marked ancestor lookup" ) {
            implicit val system  = sysCreator()
            var success = false
            try {
               given( "a randomly filled and marked tree, and a manually maintained duplicate" )

               val full    = system.atomic { implicit tx =>
                  Ancestor.newTree[ S, Int ]( 0 )
               }
               val map     = system.atomic { implicit tx =>
                  Ancestor.newMap[ S, Int, Int ]( full, 0 )
               }
               val rnd     = new util.Random( seed )
//               val empty   = IndexedSeq.empty[ Int ]

               var mapRepr = IntMap[ Vertex ]( 0 -> Vertex( -1, Set.empty, Some( 0 )))
               var mapFV   = IntMap[ Ancestor.Vertex[ S, Int ]]( 0 -> full.root )
//               var values  = IndexedSeq.tabulate[ Int ]( NUM1 )( identity )

               when( "during insertion for all existing vertices their marked ancestor is queried" )
               then( "the results should be identical to manual brute force search" )

               var NEXT_TEST  = 0

               for( version <- 1 to NUM1 ) {
//println( version )
                  val update = system.atomic { implicit tx =>
                     val ref        = rnd.nextInt( version )
                     val draw       = rnd.nextDouble()
                     var _mapFV     = mapFV
                     var _mapRepr   = mapRepr
//                     var _values    = values
//if( version == 7000 ) {
//   println( version )
//}
//if( version == NUM1 ) {
//   println( "aqui" )
//}

                     val mark = if( rnd.nextDouble() < MARK_LIVE ) {
//                        val valIdx  = rnd.nextInt( _values.size )
//                        val value   = _values( valIdx )
//                        _values     = _values.patch( valIdx, empty, 1 )    // ouch, this is ultra slow
                        val value   = rnd.nextInt()
                        Some( value )

                     } else None

                     val fullVertex = if( ref == 0 || draw < (RETRO_CHILD_PERCENTAGE + RETRO_PARENT_PERCENTAGE) ) {
                        val ch = full.insertChild( mapFV( ref ), version )
                        _mapFV   += version -> ch
                        _mapRepr += version -> Vertex( ref, Set.empty, mark )
                        _mapRepr += ref -> { val old = _mapRepr( ref ); old.copy( children = old.children + version )}
                        ch

                     } else if( draw < RETRO_CHILD_PERCENTAGE ) {
                        val ch = full.insertRetroChild( mapFV( ref ), version )
                        _mapFV   += version -> ch
                        _mapRepr += version -> Vertex( ref, _mapRepr.get( ref ).map( _.children ).getOrElse( Set.empty ), mark )
                        _mapRepr += ref -> _mapRepr( ref ).copy( children = Set( version ))
                        ch

                     } else {
                        val ch   = full.insertRetroParent( mapFV( ref ), version )
                        val par  = _mapRepr( ref ).parent
                        _mapFV   += version -> ch
                        _mapRepr += version -> Vertex( par, Set( ref ), mark )
                        _mapRepr += par -> { val old = _mapRepr( par ); old.copy( children = old.children - ref + version )}
                        _mapRepr += ref -> _mapRepr( ref ).copy( parent = version )
                        ch
                     }

                     mark.foreach { value =>
                        assert( map.add( fullVertex, value ), "Mark.add says mark existed for " + version )
                     }

                     (_mapFV, _mapRepr) // , _values
                  }
                  mapFV    = update._1
                  mapRepr  = update._2
//                  values   = update._3

                  if( version >= NEXT_TEST || (version == NUM1) ) {
                     NEXT_TEST = math.max( version + 1, (NEXT_TEST * INCR_TEST_FACTOR).toInt )
// println( version )
                     // now verify all marked ancestors
                     for( query <- 1 to version ) {
                        val (fullVertex, ancValue) = system.atomic { implicit tx =>
                           map.nearest( mapFV( query ))
                        }
                        val ancVersion = fullVertex.version
                        @tailrec def findManual( i: Int ) : (Int, Int) = {
                           val v = mapRepr( i )
                           v.mark match {
                              case Some( _value ) if( i <= query ) => (i, _value)
                              case _ => findManual( v.parent )
                           }
                        }
                        val (manVersion, manValue) = findManual( query )
                        assert( ancVersion == manVersion, "Query " + query + " yields marked version " + ancVersion +
                           " where manual result says it should be " + manVersion )
                        assert( ancValue == manValue, "Query " + query + " yields marked value " + ancValue +
                           " where manual result says it should be " + manValue )
                     }
                  }
               }

               success = true

            } finally {
               sysCleanUp( system, success )
            }
         }
      }
   }

   private final case class Vertex( parent: Int, children: Set[ Int ], mark: Option[ Int ])
}