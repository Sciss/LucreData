package de.sciss.lucre.data.gui

import java.awt.EventQueue
import java.io.File
import de.sciss.lucre.stm.{Sys, InMemory, Durable}
import de.sciss.lucre.stm.store.BerkeleyDB
import javax.swing.JFrame

/** Simple GUI app to probe the txn.HASkipList interactively. */
object InteractiveSkipListApp extends App with Runnable {
  import InteractiveSkipListView._

  EventQueue.invokeLater(this)

  def run(): Unit = {
    val a = args.headOption.getOrElse("")
    if (a.startsWith("--db")) runDurable(a)
    else                      runInMemory()
  }

  private def runDurable(a: String): Unit = {
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
    addPDFSupport(iv, fr)
  }

  private def runInMemory(): Unit = {
    implicit val system: InMemory = InMemory()
    val iv = apply[InMemory](system)
    val fr = makeFrame(iv)
    addPDFSupport(iv, fr)
  }

  private def addPDFSupport[S <: Sys[S]](view: InteractiveSkipListView[S], frame: JFrame): Unit =
    new de.sciss.pdflitz.SaveAction(view :: Nil).setupMenu(frame)
}