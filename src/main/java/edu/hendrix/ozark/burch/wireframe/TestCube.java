package edu.hendrix.ozark.burch.wireframe;

import java.awt.event.*;
import javax.swing.*;

public class TestCube {
	private TestCube() { }

	public static void main(String[] args) {
		Model square = new Polygon(new Point[] {
			Point.create(-1,  1, 0),
			Point.create( 1,  1, 0),
			Point.create( 1, -1, 0),
			Point.create(-1, -1, 0),
		});
		Model cube = new Composite(new Model[] {
			square.translate(0, 0,  1),
			square.translate(0, 0, -1),
			square.rotateX(Math.PI / 2.0).translate(0,  1, 0),
			square.rotateX(Math.PI / 2.0).translate(0, -1, 0),
		});
		Model cubeStack = new Composite(new Model[] {
			cube.scale(0.4, 0.4, 0.4).translate(0, -0.3, 0),
			cube.scale(0.2, 0.2, 0.2).translate(0, 0.3, 0),
			cube.scale(0.1, 0.1, 0.1).translate(0, 0.6, 0),
			cube.scale(0.2, 0.2, 0.2).translate(0.6, -0.5, 0),
			cube.scale(0.1, 0.1, 0.1).translate(0.9, -0.6, 0),
		});

		TestWindow window = new TestWindow(cubeStack);
		window.setVisible(true);
	}
}
