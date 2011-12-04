package edu.hendrix.ozark.burch.wireframe;

import java.awt.Dimension;
import java.awt.event.*;
import javax.swing.*;

public class TestWindow extends ModelViewer {
	private static double RADIUS = 3;

	private class Controller extends Thread
			implements KeyListener, FocusListener {
		int dx;
		int dy;
		Vector u; // up vector
		Vector v; // right vector
		Vector n; // outward vector
		boolean view_dirty;

		Controller() {
			dx = 0;
			dy = 0;
			resetPosition();
		}

		public void resetPosition() {
			u = Vector.create(0, 1, 0);
			v = Vector.create(1, 0, 0);
			n = Vector.create(0, 0, 1);
			view_dirty = true;
			computeView();
		}

		public void run() {
			while(true) {
				try {
					Thread.sleep(40);
				} catch(InterruptedException e) { }
				step();
				computeView();
			}
		}

		private void step() {
			Vector newn = n;
			if(dy != 0) newn = newn.add(u.scale(0.1 * dy));
			if(dx != 0) newn = newn.add(v.scale(0.1 * dx));
			if(newn == n) return; // nothing more to do
			newn = newn.scale(1.0 / newn.getLength());

			n = newn.scale(1.0 / newn.getLength());
			u = u.subtract(u.projectOnto(n));
			v = v.subtract(v.projectOnto(n));
			view_dirty = true;
		}

		private void computeView() {
			if(!view_dirty) return; // nothing more to do
			view_dirty = false;
			setViewTransform(TransformUtility.viewTransform(
				Point.ORIGIN.addScaled(RADIUS, n), Point.ORIGIN, u));
		}

		public void keyPressed(KeyEvent e) {
			switch(e.getKeyCode()) {
			case KeyEvent.VK_UP: case KeyEvent.VK_KP_UP: dy++; break;
			case KeyEvent.VK_DOWN: case KeyEvent.VK_KP_DOWN: dy--; break;
			case KeyEvent.VK_LEFT: case KeyEvent.VK_KP_LEFT: dx--; break;
			case KeyEvent.VK_RIGHT: case KeyEvent.VK_KP_RIGHT: dx++; break;
			}
		}

		public void keyReleased(KeyEvent e) {
			switch(e.getKeyCode()) {
			case KeyEvent.VK_UP: case KeyEvent.VK_KP_UP: dy--; break;
			case KeyEvent.VK_DOWN: case KeyEvent.VK_KP_DOWN: dy++; break;
			case KeyEvent.VK_LEFT: case KeyEvent.VK_KP_LEFT: dx++; break;
			case KeyEvent.VK_RIGHT: case KeyEvent.VK_KP_RIGHT: dx--; break;
			}
		}

		public void keyTyped(KeyEvent e) { }

		public void focusGained(FocusEvent e) { }
		public void focusLost(FocusEvent e) { dx = 0; dy = 0; }

	}

	private class FileMenu extends JMenu
			implements ActionListener {
		private JMenuItem reset;
		private JMenuItem quit;

		public FileMenu() {
			super("File");

			reset = new JMenuItem("Reset");
			reset.addActionListener(this);
			this.add(reset);

			quit = new JMenuItem("Quit");
			quit.addActionListener(this);
			this.add(quit);
		}

		public void actionPerformed(ActionEvent e) {
			Object src = e.getSource();
			if(src == quit) {
				System.exit(0);
			} else if(src == reset) {
				controller.resetPosition();
			}
		}
	}

	private class ProjectionMenu extends JMenu
			implements ActionListener {
		private JRadioButtonMenuItem ortho;
		private JRadioButtonMenuItem persp;

		public ProjectionMenu() {
			super("Projection");

			ButtonGroup bg = new ButtonGroup();
			ortho = new JRadioButtonMenuItem("Orthographic");
			ortho.addActionListener(this);
			this.add(ortho); bg.add(ortho);
			persp = new JRadioButtonMenuItem("Perspective");
			persp.addActionListener(this);
			this.add(persp); bg.add(persp);

			setToPerspective();
		}

		public void actionPerformed(ActionEvent e) {
			if(ortho.isSelected()) setToOrthographic();
			if(persp.isSelected()) setToPerspective();
		}

		public void setToOrthographic() {
			ortho.setSelected(true);
			setProjectionTransform(TransformUtility.orthographicProjection(-1, 1, -1, 1, RADIUS - 1, RADIUS + 1));
		}

		public void setToPerspective() {
			persp.setSelected(true);
			setProjectionTransform(TransformUtility.perspectiveProjection(-1, 1, -1, 1, RADIUS - 1, RADIUS + 1));
		}
	}

	private Controller controller;

	public TestWindow(Model model) {
		super(new Dimension(300, 300));
		controller = new Controller();

		JMenuBar menubar = new JMenuBar();
		menubar.add(new FileMenu());
		menubar.add(new ProjectionMenu());
		setJMenuBar(menubar);

		setModel(model);
		addKeyListener(controller);
		addFocusListener(controller);
		grabFocus();
		controller.start();
	}
}
