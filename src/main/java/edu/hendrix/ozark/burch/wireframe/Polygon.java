package edu.hendrix.ozark.burch.wireframe;

import java.awt.Graphics;
import java.awt.Color;

/** Represents a polygonal face of a solid. */
public class Polygon extends Model {
	private Point[] pts;

	/** Constructs a polygon with the given points. The final
	 * point in the array is presumed to connect with the
	 * initial point in the array. */
	public Polygon(Point[] points) {
		pts = new Point[points.length];
		System.arraycopy(points, 0, pts, 0, pts.length);
	}

	/** Draws this polygon using the given Graphics3D. */
	public void draw(Graphics3D g) {
		g.drawPolygon(pts);
	}
}
