///*
// *  SkipQuadTreeView.scala
// *  (TreeTests)
// *
// *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// *
// *
// *  Changelog:
// */
//
//package de.sciss.collection
//package view
//
//import prefuse.action.layout.graph.ForceDirectedLayout
//import prefuse.visual.VisualItem
//import prefuse.util.ColorLib
//import prefuse.activity.Activity
//import prefuse.action.{RepaintAction, ActionList}
//import prefuse.{Constants, Visualization, Display}
//import prefuse.visual.expression.InGroupPredicate
//import prefuse.render.{DefaultRendererFactory, EdgeRenderer, LabelRenderer}
//import collection.breakOut
//import collection.immutable.{IntMap, IndexedSeq => IIdxSeq}
//import prefuse.controls._
//import prefuse.data.event.TupleSetListener
//import prefuse.data.tuple.TupleSet
//import prefuse.data.{Tuple, Node, Graph}
//import javax.swing.{JComponent}
//import java.beans.{PropertyChangeListener, PropertyChangeEvent}
//import prefuse.action.assignment.{StrokeAction, ColorAction}
//import java.awt.{BasicStroke, Color}
//import prefuse.data.expression.{AbstractPredicate}
//import sys.error
//
//object VersionTreeView {
//   class Vertex( val id: Int, val parent: Vertex, var children: IIdxSeq[ Vertex ]) {
//      override def toString = "v" + id
//   }
//}
//
//class VersionTreeView {
//   import VersionTreeView._
//
//   private val grpGraph    = "graph"
//   private val grpNodes    = "graph.nodes"
//   private val grpEdges    = "graph.edges"
//   private val actColor    = "color"
//   private val actLayout   = "layout"
//   private val colPath     = "path"
//   private val colCursor   = "cursor"
//   private val vis         = new Visualization()
//   private val g           = new Graph()
//
//   private var nodeMap     = IntMap.empty[ Node ]
//
//   type Selection = (VersionPath, List[ Csr ])
//
//   private var mapCsr   = Map.empty[ Csr, CursorListener ]
//
//   private val selListeners   = new JComponent {
//      var oldSel: List[ Selection ] = Nil
//      val propSel = "selection"
//
//      def newSelection( sel: List[ Selection ]) {
//         val o    = oldSel
//         oldSel   = sel
//         firePropertyChange( propSel, o, sel )
//      }
//
//      def addL( fun: List[ Selection ] => Unit ) {
//         val l = new SelectionActionListener( fun )
//         addPropertyChangeListener( propSel, l )
//      }
//
//      def removeL( fun: List[ Selection ] => Unit ) {
//         val l = new SelectionActionListener( fun )
//         removePropertyChangeListener( propSel, l )
//      }
//
//      private case class SelectionActionListener( fun: List[ Selection ] => Unit )
//      extends PropertyChangeListener {
//         def propertyChange( e: PropertyChangeEvent ) {
//            fun( e.getNewValue().asInstanceOf[ List[ Selection ]])
//         }
//      }
//   }
//
//   val panel : JComponent = {
//      val display       = new Display( vis )
//      val vg            = vis.addGraph( grpGraph, g )
//      val nodes         = g.getNodeTable()
//      g.addColumn( VisualItem.LABEL, classOf[ String ])
//      g.addColumn( colPath, classOf[ Vector[ Version ]])
//      g.addColumn( colCursor, classOf[ List[ Csr ]]) // XXX maybe not the smartest way to occupy space in each vertex?
//
//      // colors
//      val colrGray   = ColorLib.rgb( 0xC0, 0xC0, 0xC0 )
//      val colrFocus  = ColorLib.rgb( 0x60, 0x60, 0xC0 )
//      val colrBlack  = ColorLib.rgb( 0x00, 0x00, 0x00 )
//      val colrWhite  = ColorLib.rgb( 0xFF, 0xFF, 0xFF )
//      val colrHighlight = ColorLib.rgb( 0xFF, 0x00, 0x60 )
//      val predFocus        = new InGroupPredicate( Visualization.FOCUS_ITEMS )
//      val predCursor       = new AbstractPredicate() {
//         override def getBoolean( t: Tuple ) : Boolean = t.get( colCursor ) != Nil
//      }
//      val actionNodeDraw   = new ColorAction( grpNodes, VisualItem.STROKECOLOR, colrGray )
//      val actionNodeFill   = new ColorAction( grpNodes, VisualItem.FILLCOLOR,   colrGray )
//      val actionTextColor  = new ColorAction( grpNodes, VisualItem.TEXTCOLOR,   colrBlack )
//      val actionEdgeColor  = new ColorAction( grpEdges, VisualItem.STROKECOLOR, colrGray )
//      val actionNodeStroke = new StrokeAction( grpNodes )
//      actionNodeStroke.add( predCursor, new BasicStroke( 2f ))
//      actionNodeFill.add( predFocus, colrFocus )
//      actionNodeDraw.add( predCursor, colrHighlight )
//      actionNodeDraw.add( predFocus, colrFocus )
//      actionTextColor.add( predFocus, colrWhite )
//
//      val lay = new ForceDirectedLayout( grpGraph )
//      val nodeRenderer = new LabelRenderer( VisualItem.LABEL )
//      val edgeRenderer = new EdgeRenderer( Constants.EDGE_TYPE_LINE, Constants.EDGE_ARROW_FORWARD )
//      nodeRenderer.setRoundedCorner( 4, 4 )
//      val rf = new DefaultRendererFactory( nodeRenderer )
//      rf.add( new InGroupPredicate( grpEdges), edgeRenderer )
//      vis.setRendererFactory( rf )
//
//      // fix selected focus nodes
//      val focusGroup = vis.getGroup( Visualization.FOCUS_ITEMS )
//      focusGroup.addTupleSetListener( new TupleSetListener {
//          def tupleSetChanged( ts: TupleSet, add: Array[ Tuple ], remove: Array[ Tuple ]) {
//             selListeners.newSelection( selection )
//             vis.run( actColor )
//          }
//      })
//
//      // quick repaint
//      val actionColor = new ActionList()
//      actionColor.add( actionTextColor )
//      actionColor.add( actionNodeDraw )
//      actionColor.add( actionNodeFill )
//      actionColor.add( actionNodeStroke )
//      actionColor.add( actionEdgeColor )
//      vis.putAction( actColor, actionColor )
//
//      val actionLayout = new ActionList( Activity.INFINITY, 50 )
//      actionLayout.add( lay )
//      actionLayout.add( new RepaintAction() )
//      vis.putAction( actLayout, actionLayout )
//      vis.alwaysRunAfter( actColor, actLayout )
//
//      display.addControlListener( new ZoomToFitControl() )
//      display.addControlListener( new WheelZoomControl() )
//      display.addControlListener( new PanControl() )
//      display.addControlListener( new DragControl() )
//      display.addControlListener( new FocusControl( 1, actColor ))
//
//      edgeRenderer.setHorizontalAlignment1( Constants.CENTER )
//      edgeRenderer.setHorizontalAlignment2( Constants.CENTER )
//      edgeRenderer.setVerticalAlignment1( Constants.CENTER )
//      edgeRenderer.setVerticalAlignment2( Constants.CENTER )
//
//      display.setForeground( Color.WHITE )
//      display.setBackground( Color.BLACK )
//
//      ancestorAction( display ) {
//         stopAnimation
//         try {
//            sys.t { implicit c =>
//               addFullVertices( sys.dag )
//               addFullCursors( sys.kProjector.cursors )
//               sys.addListener( lSys )
//               sys.kProjector.addListener( lCsr)
//            }
//         } finally {
//            startAnimation
//         }
//      } {
//         stopAnimation
//         sys.t { implicit c =>
//            sys.removeListener( lSys )
//            sys.kProjector.removeListener( lCsr )
//         }
//      }
//
//      display.setSize( 300, 300 )
//      display
//   }
//
//   def addSelectionListener( fun: List[ Selection ] => Unit ) {
//      selListeners.addL( fun )
//   }
//
//   def removeSelectionListener( fun: List[ Selection ] => Unit ) {
//      selListeners.removeL( fun )
//   }
//
//   def selection : List[ Selection ] = {
//      val tupT = vis.getGroup( Visualization.FOCUS_ITEMS ).tuples.asInstanceOf[ java.util.Iterator[ Tuple ]]
//      val iter = collection.JavaConversions.asScalaIterator( tupT )
//      iter.map( t => {
//         val vp   = VersionPath.wrap( t.get( colPath ).asInstanceOf[ Vector[ Version ]])
//         val csr  = t.get( colCursor ).asInstanceOf[ List[ Csr ]]
//         (vp, csr)
//      }).toList
//   }
//
//   private def stopAnimation {
//      vis.cancel( actColor )
//      vis.cancel( actLayout )
//   }
//
//   private def startAnimation {
//      vis.run( actColor )
//   }
//
//   private def checkKarlheinz( pNode: Node ) {
//      val lulu = vis.getVisualItem( grpGraph, pNode )
//      if( vis.getGroup( Visualization.FOCUS_ITEMS ).containsTuple( lulu )) {
//         selListeners.newSelection( selection )
//      }
//   }
//
//   private def addVertex( parent: Version, child: VersionPath ) {
//      stopAnimation
//      try {
//         val pNode   = g.addNode()
//         val childV  = child.version
//         pNode.setString( VisualItem.LABEL, childV.toString )
//         pNode.set( colPath, child.path )
//         pNode.set( colCursor, Nil )
//         nodeMap += childV.id -> pNode
//         nodeMap.get( parent.id ).foreach( g.addEdge( _, pNode ))
//      } finally {
//         startAnimation
//      }
//   }
//}