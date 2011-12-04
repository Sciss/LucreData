package edu.hendrix.ozark.burch.wireframe;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** Represents a window for the user to view a model. This is
 * simply a JFrame whose content pane contains a blank space
 * for the canvas. The content pane uses a BorderLayout, and
 * the blank space is situated in the center of that layout.
 * One can add additional components to the north, south, east,
 * or west - and one can set the JMenuBar. */
public class ModelViewer extends JFrame {
	private class Canvas extends JPanel {
		public Canvas(Dimension size) {
			setBackground(Color.white);
			setPreferredSize(size);
		}

		public void paintComponent(Graphics g) {
			super.paintComponent(g);
			Dimension d = getSize();
			double w = d.getWidth();
			double h = d.getHeight();
			Graphics3D g3 = new Graphics3D(g);
			g3.setViewTransform(V);
			g3.setProjectionTransform(P);
			g3.setViewFrameTransform(Transform.translate(1, 1, 1)
					.prepend(Transform.scale(w / 2, -h / 2, 1))
					.prepend(Transform.translate(0, h, 0)));
			model.draw(g3);
		}
	}

	private class WindowCloser extends WindowAdapter {
		public void windowClosing(WindowEvent e) { System.exit(0); }
	}

	private Canvas canvas;
	private Model model;
	private Transform V;
	private Transform P;

	/** Creates a ModelViewer window whose canvas is of the
	 * given dimension. */
	public ModelViewer(Dimension size) {
		canvas = new Canvas(size);
		model = Model.EMPTY;
		V = Transform.IDENTITY;
		P = Transform.IDENTITY;

		setTitle("Model Viewer");
		addWindowListener(new WindowCloser());

		getContentPane().add(canvas);
		pack();
	}

	/** Registers a key listener with the model view. */
	public void addKeyListener(KeyListener l) { canvas.addKeyListener(l); }
	/** Removes a key listener previously registered with the model view. */
	public void removeKeyListener(KeyListener l) { canvas.removeKeyListener(l); }
	/** Registers a focus listener with the model view. */
	public void addFocusListener(FocusListener l) { canvas.addFocusListener(l); }
	/** Removes a focus listener previously registered with the model view. */
	public void removeFocusListener(FocusListener l) { canvas.removeFocusListener(l); }
	/** Grabs the keyboard focus for the model view. */
	public void grabFocus() { canvas.grabFocus(); }

	/** Modifies the model currently being viewed. */
	public void setModel(Model m) {
		model = m;
		canvas.repaint();
	}

	/** Modifies the view transform used to view the model. */
	public void setViewTransform(Transform x) {
		V = x;
		canvas.repaint();
	}

	/** Modifies the projection transform used to view the model. */
	public void setProjectionTransform(Transform x) {
		P = x;
		canvas.repaint();
	}

}
