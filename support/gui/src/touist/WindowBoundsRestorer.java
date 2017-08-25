package touist;

/**
 This file is part of the Zeidon Java Object Engine (Zeidon JOE).
 Zeidon JOE is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 Zeidon JOE is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.
 You should have received a copy of the GNU Lesser General Public License
 along with Zeidon JOE.  If not, see <http://www.gnu.org/licenses/>.
 Copyright 2009-2015 QuinSoft
 */

import java.awt.*;
import java.util.prefs.Preferences;

import gui.AbstractComponentPanel;
import org.apache.commons.lang3.StringUtils;

/**
 * This class can be used to save/restore Swing windows and components.
 *
 * @author dgc
 *
 */
public class WindowBoundsRestorer
{
    private Preferences prefs;

    public WindowBoundsRestorer(Preferences p)
    {
         prefs = p;
    }

    private void setBounds( String key, Component c )
    {
        key = key + c.getName();

        String position = prefs.get(key,"");
        if ( c.getName() != null && ! StringUtils.isBlank( position ) )
        {
            String[] nums = position.split( "," );
            c.setBounds( Integer.parseInt( nums[0] ), Integer.parseInt( nums[1] ),
                    Integer.parseInt( nums[2] ), Integer.parseInt( nums[3] ) );
        }

        if ( c instanceof Container )
        {
            key = key + "/";
            Container container = (Container) c;
            for ( Component child : container.getComponents() )
                setBounds( key, child );
        }
    }

    /**
     * @param component Any component in the Swing app.  The top-most container will be
     * determined from this component.
     */
    public void restore( Component component )
    {
        Component top = component;
        while ( top.getParent() != null )
            top = top.getParent();

        setBounds( "", top );
    }

    private void getBounds( String key, Component c )
    {
        key = key + c.getName();
        String position = String.format( "%d,%d,%d,%d", c.getX(), c.getY(), c.getWidth(), c.getHeight() );
        prefs.put( key, position );
        if ( c instanceof Container )
        {
            key = key + "/";
            Container container = (Container) c;
            for ( Component child : container.getComponents() )
                getBounds( key, child );
        }
    }

    public void save( Component component )
    {
        Component top = component;
        while ( top.getParent() != null )
            top = top.getParent();

        getBounds( "", top );
    }
}
