package edu.hendrix.ozark.burch.wireframe;

/** A set of utility methods for generating transformation
 * matrices of interest. */
public class TransformUtility {
	private TransformUtility() { }

	/** Creates a transformation for switching from world
	 * coordinates to view coordinates. The view
	 * is based at the given location <code>eye</code>,
	 * facing toward the locaion <code>look</code>, with the
	 * <code>up</code> vector specifying which direction the
	 * top of the screen is from <code>eye</code>. Note that all
	 * three parameters are specified in world coordinates. */
	public static Transform viewTransform(Point eye, Point look,
			Vector up) {
		Vector n = eye.subtract(look);
		Vector u = up.cross(n);
		Vector v = n.cross(u);
		Vector y = eye.subtract(Point.ORIGIN);
		n = n.normalize();
		u = u.normalize();
		v = v.normalize();
		return Transform.create(new double[] {
			u.getX(), u.getY(), u.getZ(), -y.dot(u),
			v.getX(), v.getY(), v.getZ(), -y.dot(v),
			n.getX(), n.getY(), n.getZ(), -y.dot(n),
			0, 0, 0, 1 });
	}

	/** Creates a projection matrix transforming the
	 * rectangular view volume into the standard cube.
	 * The view volume is a rectangular solid aligned with
	 * the view coordinates' axes, and it is specified by the range of
	 * x-coordinates [<code>x0</code>,<code>x1</code>], the range of
	 * y-coordinates [<code>y0</code>,<code>y1</code>], and the range
	 * of z-coordinates [<code>N</code>,<code>F</code>]. */
	public static Transform orthographicProjection(double x0, double x1,
			double y0, double y1, double N, double F) {
		return Transform.scale(2 / (x1 - x0), 2 / (y1 - y0), 2 / (F - N))
			.append(Transform.translate(-(x0 + x1) / 2, -(y0 + y1) / 2, -(N + F) / 2));
	}

	/** Creates a projection matrix transforming a frustum view volume
	 * into the standard cube. The view volume is a truncated
	 * pyramid proceeding along the z-axis. The pyramid has its base at
	 * -<code>F</code>, and its peak at the origin, but the peak
	 * is truncated off with a plane parallel to the base at the
	 * z-coordinate -<code>N</code>. The width and breadth of the
	 * frustum is specified through the size of the pyramid's
	 * slice with the near plane. In particular, that slice is
	 * a rectangle covering the range of
	 * x-coordinates [<code>x0</code>,<code>x1</code>] and the range of
	 * y-coordinates [<code>y0</code>,<code>y1</code>]. */
	public static Transform perspectiveProjection(double x0, double x1,
			double y0, double y1, double N, double F) {
		// This is currently identical to orthographic
		// projection. You will delete this and replace it with
		// your own definition.
		return Transform.scale(2 / (x1 - x0), 2 / (y1 - y0), 2 / (F - N))
			.append(Transform.translate(-(x0 + x1) / 2, -(y0 + y1) / 2, -(N + F) / 2));
	}
}
