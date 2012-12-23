package de.sciss.lucre.data.gui

import java.awt.EventQueue
import java.io.File
import de.sciss.lucre.stm.{InMemory, Durable}
import de.sciss.lucre.stm.store.BerkeleyDB

/**
 * Options:
 * - "--db" launch with database on desktop.
 * - "--dbtmp" launch with database in temporary folder.
 * - "--memory" launch with in-memory stm
 */
object InteractiveSkipOctreeApp extends App with Runnable {
   import InteractiveSkipOctreePanel._

   EventQueue.invokeLater( this )
   def run() {
      val a = args.headOption.getOrElse( "" )
      if( a.startsWith( "--db" )) {
         val dir     = if( a == "--dbtmp" ) {
            File.createTempFile( "octree", "_database" )
         } else {
            new File( sys.props( "user.home" ), "octree_database" )
         }
         dir.delete()
         dir.mkdir()
         val f       = new File( dir, "data" )
         println( f.getAbsolutePath )
         implicit val system: Durable = Durable( BerkeleyDB.open( f ))
         val model = makeModel2D( system ) {
            system.step( implicit tx => system.debugListUserRecords() ).foreach( println _ )
         }
         makeFrame( model )

      } else {
         implicit val system: InMemory = InMemory()
         val model = makeModel2D( system ) {
            println( "(Consistency not checked)" )
         }
         makeFrame( model )
      }
   }
}