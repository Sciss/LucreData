package edu.hendrix.ozark.burch.wireframe;

import java.awt.Graphics;
import java.awt.Color;

/** Represents a figure in three-dimensional space. In a
 * well-designed system, each Model implementation would be an
 * immutable class. */
public abstract class Model {
	/** The null model, containing no components. */
	public static final Model EMPTY = new Empty();

	private static class Empty extends Model {
		public void draw(Graphics3D g) { }
		public Model transform(Transform xform) { return this; }
	}

	private static class TransformedModel extends Model {
		private Model model;
		private Transform x;

		public TransformedModel(Model model, Transform xform) {
			this.model = model;
			this.x = xform;
		}

		public void draw(Graphics3D g) {
			Transform old = g.getModelTransform();
			g.setModelTransform(old.append(x));
			model.draw(g);
			g.setModelTransform(old);
		}

		public Model transform(Transform xform) {
			return new TransformedModel(model, xform.append(x));
		}
	}

	private static class ColoredModel extends Model {
		private Model model;
		private Color color;
		public ColoredModel(Model m, Color c) { model = m; color = c; }
		public void draw(Graphics3D g) {
			Color old = g.getColor();
			g.setColor(color);
			model.draw(g);
			g.setColor(old);
		}
	}

	public abstract void draw(Graphics3D g);

	/** Returns a model equivalent to this one, except that its
	 * default color is the given color instead. */
	public Model color(Color c) {
		return new ColoredModel(this, c);
	}

	/** Returns a model equivalent to this one, except where
	 * its coordinate system has been transformed as given. */
	public Model transform(Transform xform) {
		return new TransformedModel(this, xform);
	}

	/** Returns a model equivalent to this one, except where
	 * its coordinate system has been scaled in the given
	 * proportions along the x-, y-, and z-axes. */
	public Model scale(double x, double y, double z) {
		return transform(Transform.scale(x, y, z));
	}

	/** Returns a model equivalent to this one, except where
	 * its coordinate system has been translated the given
	 * distance along the x-, y-, and z-axes. */
	public Model translate(double x, double y, double z) {
		return transform(Transform.translate(x, y, z));
	}

	/** Returns a model equivalent to this one, except where
	 * its coordinate system has been rotated the given number
	 * of radians around the x-axis. */
	public Model rotateX(double theta) {
		return transform(Transform.rotateX(theta));
	}

	/** Returns a model equivalent to this one, except where
	 * its coordinate system has been rotated the given number
	 * of radians around the y-axis. */
	public Model rotateY(double theta) {
		return transform(Transform.rotateY(theta));
	}

	/** Returns a model equivalent to this one, except where
	 * its coordinate system has been rotated the given number
	 * of radians around the z-axis. */
	public Model rotateZ(double theta) {
		return transform(Transform.rotateZ(theta));
	}
}
