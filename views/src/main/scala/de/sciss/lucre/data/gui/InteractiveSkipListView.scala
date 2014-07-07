/*
 *  InteractiveSkipListView.scala
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
package data
package gui

import java.awt.event.{ActionListener, ActionEvent}
import java.awt.{Color, FlowLayout, BorderLayout, Dimension}
import javax.swing.{Box, JLabel, SwingConstants, WindowConstants, JFrame, JTextField, JButton, JPanel}
import stm.{Source, Cursor, Sys}

object InteractiveSkipListView {
  def apply[S <: Sys[S]](system: S)(implicit cursor: Cursor[S]): InteractiveSkipListView[S] = {
    val fut = new FutureObserver[S]
    implicit val ser = HASkipList.Set.serializer[S, Int](fut)
    val access = system.root { implicit tx =>
      HASkipList.Set.empty[S, Int](minGap = 1, keyObserver = fut)
    }
    val res = new InteractiveSkipListView[S](access)
    fut.init(res)
    res
  }

  def makeFrame[S <: Sys[S]](iv: InteractiveSkipListView[S]): JFrame = {
    val f = new JFrame("SkipList")
    val cp = f.getContentPane
    //      val iv   = new InteractiveSkipListView( system )
    cp.add(iv, BorderLayout.CENTER)

    f.pack()
    f.setLocationRelativeTo(null)
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    f.setVisible(true)
    f
  }

  final class FutureObserver[S <: Sys[S]] extends SkipList.KeyObserver[S#Tx, Int] {
    @volatile private var view: SkipList.KeyObserver[S#Tx, Int] = null

    def keyUp(key: Int)(implicit tx: S#Tx): Unit =
      if (view != null) view.keyUp(key)

    def keyDown(key: Int)(implicit tx: S#Tx): Unit =
      if (view != null) view.keyDown(key)

    def init(v: SkipList.KeyObserver[S#Tx, Int]): Unit = view = v
  }
}

class InteractiveSkipListView[S <: Sys[S]](access: Source[S#Tx, HASkipList.Set[S, Int]])(implicit cursor: Cursor[S])
  extends JPanel(new BorderLayout()) with SkipList.KeyObserver[S#Tx, Int] {
  view =>

  private val rnd   = new util.Random(1L)
  private var obsUp = IndexedSeq.empty[Int]
  private var obsDn = IndexedSeq.empty[Int]

  //   val l = {
  //      implicit val sys = system
  //      sys.step { implicit tx => txn.HASkipList.empty[ S, Int ]( minGap = 1, keyObserver = view )}
  //   }

  //   val l = _create( this )
  val slv: HASkipListView[S, Int] = new HASkipListView(l(_))

  def l(implicit tx: S#Tx): HASkipList.Set[S, Int] = access()

  slv.setPreferredSize(new Dimension(16 * 64 + 16, 3 * 64 + 16))

  add(slv, BorderLayout.CENTER)
  private val p = new JPanel(new FlowLayout())

  private def but(lb: String)(action: => Unit): Unit = {
    val b = new JButton(lb)
    b.setFocusable(false)
    b.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        action
        slv.repaint()
      }
    })
    p.add(b)
  }

  private def space(): Unit = p.add(Box.createHorizontalStrut(8))

  private def label(text: String): Unit = {
    val l = new JLabel(text, SwingConstants.RIGHT)
    p.add(l)
  }

  private val ggNum = new JTextField(2)
  p.add(ggNum)

  private def tryNum(fun: Int => Unit): Unit =
    try {
      val i = ggNum.getText.toInt; fun(i)
    } catch {
      case nfe: NumberFormatException =>
    }

  private val ggStatus = new JTextField(12)
  ggStatus.setEditable(false)

  private def status(str: String): Unit = ggStatus.setText(str)

  private val colrGreen = new Color(0x00, 0xA0, 0x00)

  def butAddRemove(name: String)(fun: (S#Tx, Int) => Boolean): Unit =
    but(name) {
      tryNum { i =>
        obsUp = IndexedSeq.empty
        obsDn = IndexedSeq.empty
        val res = cursor.step(tx => fun(tx, i))
        status(res.toString)
        slv.highlight = (obsUp.map(_ -> colrGreen) ++ obsDn.map(_ -> Color.red)).toMap + (i -> Color.blue)
      }
    }

  //   private def atomic[ A ]( fun: InTxn => A ) : A = {
  //      TxnExecutor.defaultAtomic( fun )
  //   }

  butAddRemove("Add")((txn, key) =>
    l(txn).add(key)(txn)
  )
  butAddRemove("Remove") {
    (txn, key) =>
      l(txn).remove(key)(txn)
  }

  but("Contains") {
    tryNum { key =>
      val res = cursor.step {
        implicit tx => l.contains(key)
      }
      status(res.toString)
      slv.highlight = Map(key -> Color.blue)
    }
  }

  private def butFloorCeil(label: String)(fun: (HASkipList.Set[S, Int], Int, S#Tx) => Option[Int]): Unit =
    but(label) {
      tryNum { key =>
        val res = cursor.step(implicit tx => fun(l, key, tx))
        res match {
          case Some(key2) =>
            status(key2.toString)
            slv.highlight = Map(key2 -> Color.blue)
          case None =>
            status("not found")
            slv.highlight = Map.empty
        }
      }
    }

  butFloorCeil("Floor")((l, key, tx) => l.floor(key)(tx))
  butFloorCeil("Ceil" )((l, key, tx) => l.ceil (key)(tx))

  space()
  label("Randomly:")

  private def addRandom(num: Int): Unit = {
    obsUp = IndexedSeq.empty
    obsDn = IndexedSeq.empty
    val ps = Seq.fill(num)(rnd.nextInt(100))
    println(ps)
    status(ps.lastOption.map(_.toString).getOrElse(""))
    cursor.step { implicit tx =>
      ps.foreach(l.add)
    }
    slv.highlight = (obsUp.map(_ -> colrGreen) ++ obsDn.map(_ -> Color.red) ++ ps.map(_ -> Color.blue)).toMap
  }

  but("Add 1x") {
    addRandom(1)
  }
  but("Add 10x") {
    addRandom(10)
  }

  space()

  but("Print List") {
    println(cursor.step(implicit tx => l.toList))
  }

  p.add(ggStatus)
  add(p, BorderLayout.SOUTH)

  def keyUp(key: Int)(implicit tx: S#Tx): Unit = {
    println("Lvl up:   " + key)
    obsUp :+= key
  }

  def keyDown(key: Int)(implicit tx: S#Tx): Unit = {
    println("Lvl down: " + key)
    obsDn :+= key
  }
}