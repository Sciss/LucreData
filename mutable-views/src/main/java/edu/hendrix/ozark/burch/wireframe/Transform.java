package edu.hendrix.ozark.burch.wireframe;

import java.text.DecimalFormat;

/** Represents a transformation of points, using an internal
 * matrix. */
public class Transform {
	/** Represents the identity transformation, in which points
	 * are transformed into themselves. */
	public static final Transform IDENTITY = new Transform(
			new double[] { 1, 0, 0, 0,
				           0, 1, 0, 0,
						   0, 0, 1, 0,
						   0, 0, 0, 1 });
	private static DecimalFormat form = new DecimalFormat("#0.00");

	private static final int SIZE = 4;

	private double[] data;

	private Transform(double[] data) {
		this.data = data;
	}

	public String toString() {
		StringBuffer buf = new StringBuffer("^");
		for(int i = 0; i < data.length; i++) {
			if(i % SIZE == 0) {
				if(i > 0) buf.append("] ");
				buf.append("[");
			} else {
				buf.append(",");
			}
			buf.append(form.format(data[i]));
		}
		buf.append("]");
		return buf.toString();
	}

	/** Returns the concatenation of two transformations, where
	 * points are first transformed by the parameter transformation,
	 * and the result is then transformed by this transformation.
	 * The resulting transformation is simply the matrix product
	 * of this matrix by the parameter matrix. */
	public Transform append(Transform other) {
		double[] ret = new double[SIZE * SIZE];
		for(int r = 0; r < ret.length; r++) {
			double val = 0.0;
			for(int a = (r / SIZE) * SIZE, b = r % SIZE;
					b < SIZE * SIZE;
					a++, b += SIZE) {
				val += this.data[a] * other.data[b];
			}
			ret[r] = val;
		}
		return new Transform(ret);
	}

	/** Returns the concatenation of two transformations, where
	 * points are first transformed by this transformation, and
	 * the result is then transformed by the parameter transformation.
	 * The resulting transformation is simply the matrix product
	 * of the parameter matrix by this matrix. */
	public Transform prepend(Transform other) {
		return other.append(this);
	}

	/** Returns the point resulting from transforming the given
	 * point according to this transformation. */
	public Point transform(Point p) {
		double x = p.getX();
		double y = p.getY();
		double z = p.getZ();
		double h = p.getHomogeneous();

		double a = data[ 0] * x + data[ 1] * y + data[ 2] * z
			+ data[ 3] * h;
		double b = data[ 4] * x + data[ 5] * y + data[ 6] * z
			+ data[ 7] * h;
		double c = data[ 8] * x + data[ 9] * y + data[10] * z
			+ data[11] * h;
		double s = data[12] * x + data[13] * y + data[14] * z
			+ data[15] * h;

		return Point.create(a, b, c, s);
	}

	/** Creates a transform with the given values in the
	 * matrix containing all 16 values in row order. The parameter
	 * should be an array of 16 values, with the first four
	 * values representing the matrix's top row, and the last four
	 * values representing the matrix's bottom row. */
	public static Transform create(double[] mat) {
		if(mat == null || mat.length != 16) {
			throw new IllegalArgumentException("Transform.create " +
				"expects an array with 16 values, given in row order.");
		}
		return new Transform(mat);
	}

	/** Creates a transform representing the translation of
	 * coordinates by the given distances along the x-, y-, and
	 * z-axes. */
	public static Transform translate(double x, double y, double z) {
		return new Transform(new double[] {
			1, 0, 0, x,
			0, 1, 0, y,
			0, 0, 1, z,
			0, 0, 0, 1 });
	}

	/** Creates a transform representing scaling the x-, y-,
	 * and z-coordinates as specified. Depending on the
	 * parameters, this could represent stretching the
	 * coordinate system, compressing the
	 * coordinate system, or reflecting the
	 * coordinate system of an axis - or some combination
	 * thereof. */
	public static Transform scale(double x, double y, double z) {
		return new Transform(new double[] {
			x, 0, 0, 0,
			0, y, 0, 0,
			0, 0, z, 0,
			0, 0, 0, 1 });
	}

	/** Creates a transform representing rotating the given
	 * number of radians around the x-axis. */
	public static Transform rotateX(double theta) {
		double c = Math.cos(theta);
		double s = Math.sin(theta);
		return new Transform(new double[] {
			1, 0, 0, 0,
			0, c,-s, 0,
			0, s, c, 0,
			0, 0, 0, 1 });
	}

	/** Creates a transform representing rotating the given
	 * number of radians around the y-axis. */
	public static Transform rotateY(double theta) {
		double c = Math.cos(theta);
		double s = Math.sin(theta);
		return new Transform(new double[] {
			 c, 0, s, 0,
			 0, 1, 0, 0,
			-s, 0, c, 0,
			 0, 0, 0, 1 });
	}

	/** Creates a transform representing rotating the given
	 * number of radians around the z-axis. */
	public static Transform rotateZ(double theta) {
		double c = Math.cos(theta);
		double s = Math.sin(theta);
		return new Transform(new double[] {
			c,-s, 0, 0,
			s, c, 0, 0,
			0, 0, 1, 0,
			0, 0, 0, 1 });
	}

}
