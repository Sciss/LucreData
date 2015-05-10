/*
 *  InteractiveSkipOctreePanel.scala
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

import java.awt.{Insets, Color, FlowLayout, BorderLayout}
import javax.swing.{JComponent, JLabel, SwingConstants, Box, WindowConstants, JComboBox, AbstractButton, JTextField, JButton, JFrame, JPanel}
import java.awt.event.{MouseListener, MouseMotionListener, ActionListener, MouseEvent, MouseAdapter, ActionEvent}
import geom.{IntSpace, Space, QueryShape, IntDistanceMeasure2D, DistanceMeasure, IntPoint2D, IntSquare}
import IntSpace.TwoDim
import stm.{Source, Cursor, Sys}

object InteractiveSkipOctreePanel {
  val seed = 0L

  def makeModel2D[S <: Sys[S]](system: S)(cons: => Unit)(implicit cursor: Cursor[S]): Model2D[S] = {
    implicit val pointView  = (p: IntPoint2D, t: Any) => p
    import TwoDim.pointSerializer
    implicit val reader     = DeterministicSkipOctree.serializer[S, TwoDim, IntPoint2D]
    val access = system.root {
      implicit tx =>
        DeterministicSkipOctree.empty[S, TwoDim, IntPoint2D](
          IntSquare(sz, sz, sz), skipGap = 1)
    }
    new Model2D[S](cursor, access, () => cons)
  }

  def makeFrame[S <: Sys[S], D <: Space[D], Point <: D#PointLike](model: Model[S, D, Point]): JFrame = {
    val f = new JFrame("Skip Octree")
    //      f.setResizable( false )
    val cp = f.getContentPane
    val iv = model.newPanel()
    cp.add(iv, BorderLayout.CENTER)
    f.pack()
    f.setLocationRelativeTo(null)
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    f.setVisible(true)
    f
  }

  private val sz = 256

  final class Model2D[S <: Sys[S]](val cursor: Cursor[S],
                                   access: Source[S#Tx, DeterministicSkipOctree[S, TwoDim, IntPoint2D]],
                                   cons: () => Unit, val nTimes: Int = 10)
    extends Model[S, TwoDim, IntPoint2D] {

    def tree(implicit tx: S#Tx): SkipOctree[S, TwoDim, IntPoint2D] = access()

    def queryShape(sq: IntSquare) = sq

    def point(coords: IndexedSeq[Int]) = coords match {
      case IndexedSeq(x, y) => IntPoint2D(x, y)
    }

    def coords(p: TwoDim#PointLike): IndexedSeq[Int] = IndexedSeq(p.x, p.y)

    def hyperCube(coords: IndexedSeq[Int], ext: Int) = coords match {
      case IndexedSeq(x, y) => IntSquare(x, y, ext)
    }

    def consistency(): Unit = cons()

    val view = {
      val res = new SkipQuadtreeView[S, IntPoint2D](access, cursor, identity)
      res.topPainter = Some(topPaint)
      res
    }

    def repaint(): Unit = view.repaint()

    //      val baseDistance = IntDistanceMeasure2D$.euclideanSq

    def highlight: Set[IntPoint2D] = view.highlight

    def highlight_=(points: Set[IntPoint2D]): Unit = view.highlight = points

    val distanceMeasures = IndexedSeq(
      "Euclidean" -> IntDistanceMeasure2D.euclideanSq,
      "Maximum"   -> IntDistanceMeasure2D.chebyshev,
      "Next Span" -> IntDistanceMeasure2D.nextSpanEvent(IntSquare(sz, sz, sz)),
      "Prev Span" -> IntDistanceMeasure2D.prevSpanEvent(IntSquare(sz, sz, sz)),
      "Minimum"   -> IntDistanceMeasure2D.vehsybehc
    )

    var rangeHyperCube = Option.empty[IntSquare]

    private val colrTrns = new Color(0x00, 0x00, 0xFF, 0x40)

    private def topPaint(h: QuadView.PaintHelper): Unit =
      rangeHyperCube.foreach { q =>
        h.g2.setColor(Color.blue)
        val side = q.extent << 1
        h.g2.drawRect(q.left, q.top, side, side)
        h.g2.setColor(colrTrns)
        h.g2.fillRect(q.left, q.top, side, side)
      }
  }

  //   private final class Model3D[ S <: Sys[ S ]]( tree: txn.SkipOctree[ S, Space.IntThreeDim, IntPoint3DLike ])
  //   extends Model[ S, Space.IntThreeDim ] {
  //
  //      def queryShape( c: IntCubeLike ) = c
  //      def point( coords: IndexedSeq[ Int ]) = coords match {
  //         case IndexedSeq( x, y, z ) => IntPoint3D( x, y, z )
  //      }
  //      def coords( p: IntPoint3DLike ) : IndexedSeq[ Int ] = IndexedSeq( p.x, p.y, p.z )
  //      def hyperCube( coords: IndexedSeq[ Int ], ext: Int ) = coords match {
  //         case IndexedSeq( x, y, z ) => IntCube( x, y, z, ext )
  //      }
  //
  //      val view = new SkipOctree3DView( tree )
  //      def repaint(): Unit = { view.treeUpdated() }
  ////      val baseDistance = IntDistanceMeasure3D$.euclideanSq
  //      def highlight: Set[ IntPoint3DLike ] = view.highlight
  //      def highlight_=( points: Set[ IntPoint3DLike ]): Unit = { view.highlight = points }
  //
  //      val distanceMeasures = IndexedSeq(
  //         "Euclidean" -> IntDistanceMeasure3D$.euclideanSq,
  //         "MaximumXY" -> IntDistanceMeasure3D$.chebyshevXY,
  //         "MinimumXY" -> IntDistanceMeasure3D$.vehsybehcXY
  //      )
  //
  //      var rangeHyperCube = Option.empty[ IntCubeLike ]
  //
  //      def addPDFSupport( f: JFrame ): Unit = {
  //         PDFSupport.addMenu[ JComponent ]( f, view :: Nil, _ => () )
  //      }
  //   }

  trait Model[S <: Sys[S], D <: Space[D], Point <: D#PointLike] {
    def nTimes: Int

    def consistency(): Unit

    def tree(implicit tx: S#Tx): SkipOctree[S, D, Point]

    def view: JComponent

    final def insets: Insets = view.getInsets

    def point(coords: IndexedSeq[Int]): Point

    def coords(p: D#PointLike): IndexedSeq[Int]

    def hyperCube(coords: IndexedSeq[Int], ext: Int): D#HyperCube

    //      def baseDistance: DistanceMeasure[ _, D ]
    def distanceMeasures: IndexedSeq[(String, DistanceMeasure.Ops[_, D])]

    var highlight: Set[Point]

    final def pointString(p: D#PointLike): String = coords(p).mkString("(", ",", ")")

    final def newPanel(): InteractiveSkipOctreePanel[S, D, Point] = new InteractiveSkipOctreePanel(this)

    def queryShape(q: D#HyperCube): QueryShape[_, D]

    def repaint(): Unit

    var rangeHyperCube: Option[D#HyperCube]

    def cursor: Cursor[S]

    final def addMouseAdapter(ma: MouseListener with MouseMotionListener): Unit = {
      view.addMouseListener(ma)
      view.addMouseMotionListener(ma)
    }
  }
}

class InteractiveSkipOctreePanel[S <: Sys[S], D <: Space[D], Point <: D#PointLike](
    val model: InteractiveSkipOctreePanel.Model[S, D, Point])
  extends JPanel(new BorderLayout()) {

  import InteractiveSkipOctreePanel._

  import model.{tree => t}

  private val (space, numOrthants) = model.cursor.step { implicit tx =>
    val tr = t
    (tr.space, tr.numOrthants)
  }

  private val rnd = new util.Random(seed)

  private val in = model.insets

  private var distFilter: DistanceMeasure.Ops[_, D] => DistanceMeasure[_, D] = identity
  private var baseDistance = model.distanceMeasures(0)._2
  private var distMeasure: DistanceMeasure[_, D] = baseDistance

  def recalcDistMeasure(): Unit = distMeasure = distFilter(baseDistance)

  private val ggCoord = IndexedSeq.fill(space.dim)(new JTextField(3))

  private val ggExt = new JTextField(3)

  private def updateNum(coords: Seq[Int]): Unit =
    coords.zip(ggCoord).foreach {
      case (value, gg) => gg.setText(value.toString)
    }

  private def tryPoint(fun: Point => Unit): Unit =
    try {
      //         val p = IntPoint2D( ggX.getText.toInt, ggY.getText.toInt )
      val p = model.point(ggCoord.map(_.getText.toInt))
      fun(p)
    } catch {
      case n: NumberFormatException =>
    }

  private def tryHyperCube(fun: D#HyperCube => Unit): Unit =
    try {
      val ext = ggExt.getText.toInt
      require(ext > 0)
      //         val q = IntSquare( ggX.getText.toInt, ggY.getText.toInt, ext )
      val q = model.hyperCube(ggCoord.map(_.getText.toInt), ext)
      fun(q)
    } catch {
      case n: NumberFormatException =>
    }

  private val p = new JPanel(new FlowLayout())

  private def but(lb: String)(action: => Unit): AbstractButton = {
    val b = new JButton(lb)
    b.putClientProperty("JButton.buttonType", "bevel")
    b.putClientProperty("JComponent.sizeVariant", "mini")
    b.setFocusable(false)
    b.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        action
        model.repaint()
      }
    })
    p.add(b)
    b
  }

  private def combo(items: String*)(action: Int => Unit): Unit /* JComboBox */ = {
    val b = new JComboBox(items.toArray[AnyRef])
    b.putClientProperty("JComboBox.isSquare", true)
    b.putClientProperty("JComponent.sizeVariant", "mini")
    b.setFocusable(false)
    b.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = action(b.getSelectedIndex)
    })
    p.add(b)
    // b
  }

  private def makeSpace(): Unit = p.add(Box.createHorizontalStrut(8))

  private def label(text: String): Unit = {
    val l = new JLabel(text, SwingConstants.RIGHT)
    l.putClientProperty("JComponent.sizeVariant", "mini")
    p.add(l)
  }

  but("Help") {
    println( """
               |- Number fields are cx, cy, and extent (for range queries)
               |- Mouse:
               |     Double-click to insert point
               |     Alt-click to remove point
               |     Shift-drag for range query
               |     Ctrl-click for NN
               |""".stripMargin)
  }

  but("Dump") {
    atomic { implicit tx =>
      t.iterator.foreach(println)
      println()
      println(t.debugPrint())
    }
  }

  ggCoord.foreach(p.add)
  p.add(ggExt)

  private val ggAdd = but("Add") {
    tryPoint { p =>
      atomic { implicit tx =>
        t += p
      }
      model.highlight = Set(p)
    }
  }

  private val ggRemove = but("Remove") {
    tryPoint { p =>
      atomic { implicit tx =>
        t -= p
      }
    }
  }

  but("Contains") {
    tryPoint { p =>
      status(atomic { implicit tx => t.contains(p) }.toString)
      model.highlight = Set(p)
    }
  }

  private def rangeString(pt: Set[Point]): String = {
    val s = pt.map(model.pointString).mkString(" ")
    if (s.isEmpty) "(empty)" else s
  }

  private def atomic[Z](block: S#Tx => Z): Z = model.cursor.step(block)

  def findNN(): Unit = {
    tryPoint { p =>
      val set = atomic { implicit tx =>
        t.nearestNeighborOption(p, metric = distMeasure).map(Set(_)).getOrElse(Set.empty)
      }
      model.highlight = set
      status(rangeString(set))
    }
  }

  combo(model.distanceMeasures.map(_._1): _*) { i =>
    baseDistance = model.distanceMeasures(i)._2
    recalcDistMeasure()
  }

  combo("All Orthants" +: Seq.tabulate(numOrthants << 1)(i =>
      if (i < numOrthants) (i + 1).toString else "Except " + (i + 1 - numOrthants)): _*) { i =>
    val j = i - 1
    val k = j - numOrthants
    distFilter = if (k >= 0) {
      _.exceptOrthant(k)
    } else if (j >= 0) {
      _.orthant(j)
    } else {
      identity
    }
    recalcDistMeasure()
  }

  but("NN")(findNN())

  but("Range") {
    tryHyperCube { q =>
      val set = atomic { implicit tx =>
        t.rangeQuery(model.queryShape(q)).toSet
      }
      status(rangeString(set.take(3)))
      println(rangeString(set))
    }
  }

  makeSpace()
  label("Randomly:")

  private def addPoints(num: Int): Unit = {
    val ps = Seq.fill(num)(model.point(IndexedSeq.fill(space.dim)(rnd.nextInt(512))))
    atomic { implicit tx =>
      ps.foreach(t += _)
    }
    model.highlight = ps.toSet
  }

  but("Add 1x") { addPoints(1) }

  but("Add " + model.nTimes + "x") {
    addPoints(model.nTimes)
  }

  makeSpace()
   label( "In order:" )

  private def removePoints(num: Int): Unit =
    atomic { implicit tx =>
      val lb = List.newBuilder[Point]
      val it = t.iterator
      var i = 0
      while (i < num && it.hasNext) {
        lb += it.next()
        i += 1
      }
      val ps = lb.result()
      ps.foreach(t -= _)
    }

  but("Remove 1x") {
    removePoints(1)
  }

  but("Remove " + model.nTimes + "x") {
    removePoints(model.nTimes)
  }

  makeSpace()

  but("Consistency") {
    model.consistency()
  }

  private val ma = new MouseAdapter {
    var drag = Option.empty[(MouseEvent, Option[MouseEvent])]

    override def mouseDragged(e: MouseEvent): Unit =
      drag match {
        case Some((m1, None)) =>
          val dist = e.getPoint.distance(m1.getPoint)
          if (dist > 4) {
            drag(m1, e)
          }
        case Some((m1, Some(_))) => drag(m1, e)
        case _ =>
      }

    def drag(m1: MouseEvent, m2: MouseEvent): Unit = {
      drag = Some(m1 -> Some(m2))
      val ext = math.max(math.abs(m1.getPoint.x - m2.getPoint.x),
                         math.abs(m1.getPoint.y - m2.getPoint.y))
      ggExt.setText(ext.toString)
      tryHyperCube { q =>
        model.rangeHyperCube = Some(q)
        val set = atomic { implicit tx =>
          t.rangeQuery(model.queryShape(q)).toSet
        }
        model.highlight = set
        model.repaint()
        status(rangeString(set.take(3)))
      }
    }

    override def mouseReleased(e: MouseEvent): Unit =
      drag match {
        case Some((_, Some(_))) =>
          model.rangeHyperCube = None
          model.repaint()
        case _ =>
      }
      drag = None

    override def mousePressed(e: MouseEvent): Unit = {
      val x = e.getX - in.left
      val y = e.getY - in.top
      updateNum(Seq(x, y))
      if (e.isControlDown) {
        findNN()
        model.repaint()
      } else if (e.isAltDown) {
        // remove point
        ggRemove.doClick(100)
      } else if (e.isShiftDown) {
        // range search
        drag = Some(e -> None)
      } else if (e.getClickCount == 2) {
        // add point
        ggAdd.doClick(100)
      }
    }
  }
  model.addMouseAdapter(ma)

  add(model.view, BorderLayout.CENTER)
  private val ggStatus = new JTextField(16)
  ggStatus.setEditable(false)

  private def status(str: String): Unit = ggStatus.setText(str)

  p.add(ggStatus)
  add(p, BorderLayout.SOUTH)

  //addPoints( 20 )
  //removePoints( 14 )

  //   def verifyConsistency() {
  //      val q = t.hyperCube
  //      var h = t.lastTree
  //      var curreUnlinkedHyperCubes   = Set.empty[ D#HyperCube ]
  //      var currPoints                = Set.empty[ D#Point ]
  //      var prevs = 0
  //      do {
  //         assert( h.hyperCube == q, "Root level hyper-cube is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
  //         val nextUnlinkedQuad2Ds   = curreUnlinkedHyperCubes
  //         val nextPoints             = currPoints
  //         curreUnlinkedHyperCubes    = Set.empty
  //         currPoints                 = Set.empty
  //         def checkChildren( n: t.QNode, depth: Int ) {
  //            def assertInfo = " in level n-" + prevs + " / depth " + depth
  //
  //            var i = 0; while( i < t.numOrthants ) {
  //               n.child( i ) match {
  //                  case c: t.QNode =>
  //                     val nq = n.hyperCube.orthant( i )
  //                     val cq = c.hyperCube
  //                     assert( nq.contains( cq ), "Node has invalid hyper-cube (" + cq + "), expected: " + nq + assertInfo )
  //                     assert( n.hyperCube.indexOf( cq ) == i, "Mismatch between index-of and used orthant (" + i + "), with parent " + n.hyperCube + " and " + cq )
  //                     c.nextOption match {
  //                        case Some( next ) =>
  //                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
  //                           assert( next.hyperCube == cq, "Next hyper-cube does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
  //                        case None =>
  //                           assert( !nextUnlinkedQuad2Ds.contains( cq ), "Double missing link for " + cq + assertInfo )
  //                     }
  //                     c.prevOption match {
  //                        case Some( prev ) =>
  //                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
  //                           assert( prev.hyperCube == cq, "Next hyper-cube do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
  //                        case None => curreUnlinkedHyperCubes += cq
  //                     }
  //                     checkChildren( c, depth + 1 )
  //                  case l: t.QLeaf =>
  //                     currPoints += l.value
  //                  case _: t.QEmpty =>
  //               }
  //            i += 1 }
  //         }
  //         checkChildren( h, 0 )
  //         val pointsOnlyInNext    = nextPoints.filterNot( currPoints.contains( _ ))
  //         assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
  //         h                       = h.prevOption.orNull
  //         prevs += 1
  //      } while( h != null )
  //
  //      println( "Consistency check successful." )
  //   }
}