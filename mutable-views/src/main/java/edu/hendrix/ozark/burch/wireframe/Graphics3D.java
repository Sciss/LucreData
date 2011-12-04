package edu.hendrix.ozark.burch.wireframe;

import java.awt.Graphics;
import java.awt.Color;

/** Represents a ``paintbrush'' with which we can draw in three
 * dimensions. */
public class Graphics3D {
	private Graphics g;
	private Transform M;   // model transform
	private Transform V;   // view transform
	private Transform P;   // perspective transform
	private Transform Vf;  // view frame transform

	/** Constructs a Graphics3D object using the given Graphics
	 * object for drawing in two dimensions. */
	public Graphics3D(Graphics g) {
		this.g = g;
		V = Transform.IDENTITY;
		M = Transform.IDENTITY;
		P = Transform.IDENTITY;
		Vf = Transform.IDENTITY;
	}

	/** Returns the current color of this paintbrush. */
	public Color getColor() { return g.getColor(); }
	/** Returns the model transform M used in the pipeline. */
	public Transform getModelTransform() { return M; }
	/** Returns the view transform V used in the pipeline. */
	public Transform getViewTransform() { return V; }
	/** Returns the projection transform P used in the pipeline. */
	public Transform getProjectionTransform() { return P; }
	/** Returns the viewplane transform Vf used in the pipeline. */
	public Transform getViewFrameTransform() { return Vf; }

	/** Alters the current color of this paintbrush. */
	public void setColor(Color c) { g.setColor(c); }
	/** Alters the model transform M used in the pipeline. */
	public void setModelTransform(Transform t) { M = t; }
	/** Alters the view transform V used in the pipeline. */
	public void setViewTransform(Transform t) { V = t; }
	/** Alters the projection transform P used in the pipeline. */
	public void setProjectionTransform(Transform t) { P = t; }
	/** Alters the viewplane transform Vf used in the pipeline. */
	public void setViewFrameTransform(Transform t) { Vf = t; }

	/** Draws a polygon using this paintbrush. The polygon is
	 * drawn as a series of line segments connecting each adjacent pair
	 * of points in the array, including a segment between the
	 * final point of the array and the first point in the
	 * array. */
	public void drawPolygon(Point[] pts) {
		for(int i = 1; i < pts.length; i++) {
			drawLine(pts[i - 1], pts[i]);
		}
		drawLine(pts[pts.length - 1], pts[0]);
	}

	/** Draws a line segment using this paintbrush. */
	public void drawLine(Point a, Point b) {
		a = sendThroughPipeline(a);
		b = sendThroughPipeline(b);
		g.drawLine((int) a.getX(), (int) a.getY(),
				(int) b.getX(), (int) b.getY());
	}

	/** Returns the result of feeding the given point through
	 * the graphics pipeline. */
	private Point sendThroughPipeline(Point p) {
		p = M.transform(p);
		p = V.transform(p);
		p = P.transform(p);
		p = Vf.transform(p);
		return p;
	}
}
