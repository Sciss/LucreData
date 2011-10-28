package edu.hendrix.ozark.burch.wireframe;

/** Represents a point in three dimensions, with the additional
 * value for homogeneous coordinates. */
public class Point {
	public static Point ORIGIN = new Point(0, 0, 0, 1);

	private double x;
	private double y;
	private double z;
	private double h;

	private Point(double x, double y, double z, double h) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.h = h;
	}

	/** Returns the x-coordinate of this point. */
	public double getX() { return x; }
	/** Returns the y-coordinate of this point. */
	public double getY() { return y; }
	/** Returns the z-coordinate of this point. */
	public double getZ() { return z; }
	/** Returns the value added to this point for homogeneous coordinates. */
	public double getHomogeneous() { return h; }

	/** Returns a conventional string representation of this
	 * point. */
	public String toString() {
		return "(" + x + "," + y + "," + z + "," + h + ")";
	}

	/** Returns the distance from this point to the other
	 * point, both being scaled by their homogeneous
	 * coordinates. */
	public double getDistanceTo(Point other) {
		double oh = other.h;
		double dx = this.x / h - other.x / oh;
		double dy = this.y / h - other.y / oh;
		double dz = this.z / h - other.z / oh;
		return Math.sqrt(dx * dx + dy * dy + dz * dz);
	}

	/** Returns the vector starting at the other point and
	 * going to this point. Computing this involves scaling
	 * both points by their homogeneous values. */
	public Vector subtract(Point other) {
		double oh = other.h;
		double dx = this.x / h - other.x / oh;
		double dy = this.y / h - other.y / oh;
		double dz = this.z / h - other.z / oh;
		return Vector.create(dx, dy, dz);
	}

	/** Returns the point reached by starting at this point and adding
	 * a vector scaled by the given scalar. */
	public Point addScaled(double scalar, Vector v) {
		scalar *= h;
		return Point.create(x + scalar * v.getX(), y + scalar * v.getY(), z + scalar * v.getZ(), h);
	}

	/** Returns the point reached by starting at this point and adding
	 * a vector scaled by the given scalar. */
	public Point add(Vector v) {
		return this.addScaled(1, v);
	}

	/** Creates a point with the given coordinates, where the
	 * homogeneous value is 1. */
	public static Point create(double x, double y, double z) {
		return new Point(x, y, z, 1);
	}

	/** Creates a point with the given coordinates. */
	public static Point create(double x, double y, double z, double h) {
		return new Point(x, y, z, h);
	}
}
