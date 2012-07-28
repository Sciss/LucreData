/*
 *  PDFSupport.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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
 */

package de.sciss.lucre
package data
package gui

import java.awt.event.ActionEvent
import javax.swing.{JOptionPane, JScrollPane, DefaultListCellRenderer, Icon, JList, Action, AbstractAction, JMenuItem, JMenu, JMenuBar, JFrame, JComponent}
import java.awt.{FileDialog, Graphics2D, Graphics, Component}
import java.io.{FileOutputStream, File}
import com.itextpdf.text.{Document => IDocument, Rectangle => IRectangle}
import com.itextpdf.text.pdf.PdfWriter
import collection.breakOut

object PDFSupport {
   def addMenu[ A <: JComponent ]( f: JFrame, views: Seq[ A ], prepare: A => Unit = (_: A) => (), usePrefSize: Boolean = true ) {
      val mb            = new JMenuBar()
      val mFile         = new JMenu( "File" )
      val miExportPDF   = new JMenuItem( new AbstractAction( "Export as PDF..." ) {
         action =>
         def name = getValue( Action.NAME ).toString
         def actionPerformed( e: ActionEvent ) {
            val viewO: Option[ A ] = views.toList match {
               case Nil => None
               case v :: Nil => Some( v )
               case _ =>
                  var w = 0
                  var h = 0
                  views foreach { view =>
                     val p = view.getPreferredSize
                     val pw = math.min( 64, p.width >> 3 )
                     val ph = math.min( 64, p.height >> 3 )
                     w = math.max( w, pw )
                     h = math.max( h, ph )
                  }
                  val viewsi = views.toIndexedSeq
                  val list = new JList( Array.tabulate[ AnyRef ]( viewsi.size )( i => "#" + (i + 1) ))
                  val icons: IndexedSeq[ Icon ] = viewsi.zipWithIndex.map({ case (view, idx) =>
                     new Icon {
                        def getIconWidth  = w
                        def getIconHeight = h
                        def paintIcon( c: Component, g: Graphics, x: Int, y: Int ) {
                           val g2         = g.asInstanceOf[ Graphics2D ]
                           val atOrig     = g2.getTransform
                           val clipOrig   = g2.getClip
                           g2.clipRect( x, y, w, h )
                           g2.translate( x, y )
                           g2.scale( 0.125, 0.125 )
                           view.paint( g2 )
                           g2.setTransform( atOrig )
                           g2.setClip( clipOrig )
                        }
                     }
                  })( breakOut )
                  list.setCellRenderer( new DefaultListCellRenderer {
                     override def getListCellRendererComponent( l: JList, value: AnyRef, idx: Int, isSelected: Boolean, hasFocus: Boolean ) : Component = {
                        super.getListCellRendererComponent( l, value, idx, isSelected, hasFocus )
                        setIcon( icons( idx ))
                        this
                     }
                  })
                  list.setSelectedIndex( 0 )
                  val scroll = new JScrollPane( list )
                  val res = JOptionPane.showConfirmDialog( f, scroll, name, JOptionPane.OK_CANCEL_OPTION )
                  val selIdx = list.getSelectedIndex
                  if( res == JOptionPane.YES_OPTION && selIdx >= 0 ) Some( viewsi( selIdx )) else None
            }
            viewO foreach { view =>
               val fDlg = new FileDialog( f, name, FileDialog.SAVE )
               fDlg.setVisible( true )
               val file = fDlg.getFile
               val dir  = fDlg.getDirectory
               if( file == null || dir == null ) return
               prepare( view )
               createPDF( new File( dir, file ), view, usePrefSize )
            }
         }
      })
      mFile.add( miExportPDF )
      mb.add( mFile )
      f.setJMenuBar( mb )
   }

   def createPDF( file: File, view: JComponent, usePrefSize: Boolean = true, margin: Int = 0 ) {
      val viewSz     = if( usePrefSize ) view.getPreferredSize else view.getSize()
      val width      = viewSz.width + (margin << 1)
      val height     = viewSz.height + (margin << 1)
      val pageSize	= new IRectangle( 0, 0, width, height )
      val doc		   = new IDocument( pageSize, margin, margin, margin, margin )
      val stream	   = new FileOutputStream( file )
      val writer	   = PdfWriter.getInstance( doc, stream )

      doc.open()
      val cb		   = writer.getDirectContent
      val tp		   = cb.createTemplate( viewSz.width, viewSz.height )
      val g2		   = tp.createGraphics( viewSz.width, viewSz.height /*, fontMapper */ )
//val in = view.getInsets
//g2.translate( -in.left, -in.top )
//g2.translate( margin, margin )
      view.paint( g2 )
      g2.dispose()
      cb.addTemplate( tp, margin, margin )
      doc.close()
   }
}