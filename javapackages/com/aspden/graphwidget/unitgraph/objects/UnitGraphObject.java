package com.aspden.graphwidget.unitgraph.objects;

import com.aspden.graphwidget.unitgraph.*;
import java.awt.*;

/** An entity that lives on a {@link UnitGraph }. It is responsible for drawing itself.
 */
public interface UnitGraphObject
{
    /** Draw the entity into a graphics context, appropriately scaled.
     * @param g The graphics context.
     * @param w The width of the graph.
     * @param h The height of the graph.
     */
	abstract public void draw(Graphics g, int w, int h);
}
