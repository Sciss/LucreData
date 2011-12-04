package edu.hendrix.ozark.burch.wireframe;

import java.awt.Graphics;

/** Represents a conglomeration of several models into a single
 * model. */
public class Composite extends Model {
	private Model[] models;

	/** Constructs a model representing the union of all the
	 * given models in the array. */
	public Composite(Model[] models) {
		this.models = new Model[models.length];
		System.arraycopy(models, 0, this.models, 0, models.length);
	}

	/** Draws all the models contained in this composite model. */
	public void draw(Graphics3D g) {
		for(int i = 0; i < models.length; i++) {
			models[i].draw(g);
		}
	}
}
