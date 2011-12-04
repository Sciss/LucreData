package edu.hendrix.ozark.burch.wireframe;

/** Represents a three-dimensional vector. */
public class Vector {
	private double x;
	private double y;
	private double z;

	private Vector(double x, double y, double z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	/** Returns the change in x represented by this vector. */
	public double getX() { return x; }

	/** Returns the change in y represented by this vector. */
	public double getY() { return y; }

	/** Returns the change in z represented by this vector. */
	public double getZ() { return z; }

	/** Returns the length, or magnitude, of this vector. */
	public double getLength() {
		return Math.sqrt(x * x + y * y + z * z);
	}

	/** Returns a vector that is the normalization of this
	 * vector. That is, the returned vector is in the same
	 * direction but has a length of 1. */
	public Vector normalize() {
		double len = getLength();
		if(len == 0.0) return this;
		return this.scale(1.0 / len);
	}

	/** Returns the result of a dot product between this vector
	 * and another. */
	public double dot(Vector other) {
		return this.x * other.x + this.y * other.y + this.z * other.z;
	}

	/** Returns the result of a cross product between this vector
	 * and another. */
	public Vector cross(Vector other) {
		return Vector.create(
				this.y * other.z - this.z * other.y,
				this.z * other.x - this.x * other.z,
				this.x * other.y - this.y * other.x);
	}

	/** Returns the result of adding this vector to another. */
	public Vector add(Vector other) {
		return Vector.create(this.x + other.x, this.y + other.y, this.z + other.z);
	}

	/** Returns the result of subtracting another vector from
	 * this one. */
	public Vector subtract(Vector other) {
		return Vector.create(this.x - other.x, this.y - other.y, this.z - other.z);
	}

	/** Returns the result of projecting this vector onto the
	 * direction of another. The returned vector will be in the
	 * same direction of the parameter vector. */
	public Vector projectOnto(Vector other) {
		double x = this.dot(other);
		return other.scale(x);
	}

	/** Returns a vector that is in the same direction as this
	 * vector, but whose magnitude has been scaled by the given
	 * scalar. */
	public Vector scale(double scalar) {
		if(scalar == 1.0) return this;
		return Vector.create(scalar * this.x, scalar * this.y, scalar * this.z);
	}
	
	/** Returns a traditional representation of this vector. */
	public String toString() { return "(" + x + "," + y + "," + z + ")"; }

	/** Creates a vector with the given x-, y-, and
	 * z-coordinates. */
	public static Vector create(double x, double y, double z) {
		return new Vector(x, y, z);
	}
}
