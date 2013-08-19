package de.sciss.lucre.data.gui

import java.awt.EventQueue
import java.io.File
import de.sciss.lucre.stm.{InMemory, Durable}
import de.sciss.lucre.stm.store.BerkeleyDB

/** Simple GUI app to probe the txn.HASkipList interactively. */
object InteractiveSkipListApp extends App with Runnable {
  import InteractiveSkipListView._

  EventQueue.invokeLater(this)

  def run(): Unit = {
    val a = args.headOption.getOrElse("")
    val (frame, view) = if (a.startsWith("--db")) {
      val dir = if (a == "--dbtmp") {
        File.createTempFile("skiplist", "_database")
      } else {
        new File(sys.props("user.home"), "skiplist_database")
      }
      dir.delete()
      dir.mkdir()
      println(dir.getAbsolutePath)
      implicit val system: Durable = Durable(BerkeleyDB.open(dir))
      val iv = apply[Durable](system)
      val fr = makeFrame(iv)
      //         val fut = new FutureObserver[ Durable ]
      //         implicit val ser = HASkipList.Set.serializer[ Durable, Int ]( fut )
      //         val access = system.root[ HASkipList.Set[ Durable, Int ]] { implicit tx =>
      //            HASkipList.Set.empty[ Durable, Int ]( minGap = 1, keyObserver = fut )
      //         }
      //         val res = new InteractiveSkipListView[ Durable ]( access )
      //         fut.init( res )
      //         res
      (fr, iv)

    } else {
      implicit val system: InMemory = InMemory()
      val iv = apply[InMemory](system)
      val fr = makeFrame(iv)
      //         val fut = new FutureObserver[ InMemory ]
      //         implicit val ser = HASkipList.Set.serializer[ InMemory, Int ]( fut )
      //         val access = system.root { implicit tx =>
      //            HASkipList.Set.empty[ InMemory, Int ]( minGap = 1, keyObserver = fut )
      //         }
      //         val res = new InteractiveSkipListView[ InMemory ]( access )
      //         fut.init( res )
      //         res
      (fr, iv)
    }

    new de.sciss.pdflitz.SaveAction(view :: Nil).setupMenu(frame)
  }
}