/*

Copyright (C) 2011-2014 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QApplication>
#include <QList>
#include <QMouseEvent>
#include <QRectF>

#include "Backend.h"
#include "Canvas.h"
#include "ContextMenu.h"
#include "GLCanvas.h"
#include "Utils.h"

namespace QtHandles
{

void Canvas::redraw (bool sync)
{
  if (sync)
    qWidget ()->repaint ();
  else
    qWidget ()->update ();
}

void Canvas::blockRedraw (bool block)
{
  m_redrawBlocked = block;
}

void Canvas::canvasPaintEvent (void)
{
  if (! m_redrawBlocked)
    {
      gh_manager::auto_lock lock;

      draw (m_handle);

      if (m_mouseMode == ZoomMode && m_mouseAxes.ok ())
	drawZoomBox (m_mouseAnchor, m_mouseCurrent);
    }
}

void Canvas::canvasMouseMoveEvent (QMouseEvent* event)
{
  gh_manager::auto_lock lock;
  graphics_object ax = gh_manager::get_object (m_mouseAxes);

  if (m_mouseMode != NoMode && ax.valid_object ())
    {
      axes::properties& ap = Utils::properties<axes> (ax);

      switch (m_mouseMode)
	{
	case RotateMode:
	    {
	      Matrix bb = ap.get_boundingbox (true);
	      Matrix view = ap.get_view ().matrix_value ();

	      // Compute new view angles
	      view(0) += ((m_mouseCurrent.x () - event->x ())
			  * (180.0 / bb(2)));
	      view(1) += ((event->y () - m_mouseCurrent.y ())
			  * (180.0 / bb(3)));

	      // Clipping
	      view(1) = std::min (view(1), 90.0);
	      view(1) = std::max (view(1), -90.0);
	      if (view(0) > 180.0)
		view(0) -= 360.0;
	      else if (view(0) < -180.0)
		view(0) += 360.0;

	      // Snapping
	      double snapMargin = 1.0;
	      for (int a = -90; a <= 90; a += 90)
		if ((a - snapMargin) < view(1)
		    && view(1) < (a + snapMargin))
		  {
		    view(1) = a;
		    break;
		  }
	      for (int a = -180; a <= 180; a += 180)
		if ((a - snapMargin) < view(0)
		    && view(0) < (a + snapMargin))
		  {
		    if (a == 180)
		      view(0) = -180;
		    else
		      view(0) = a;
		    break;
		  }

	      // Update axes properties
	      ap.set_view (view);

	      // Update current mouse position
	      m_mouseCurrent = event->pos ();

	      // Force immediate redraw
	      redraw (true);
	    }
	  break;
	case ZoomMode:
	  m_mouseCurrent = event->pos();
	  redraw (true);
	  break;
	case PanMode:
	  break;
	default:
	  break;
	}
    }
}

void Canvas::canvasMousePressEvent (QMouseEvent* event)
{
  gh_manager::auto_lock lock;
  graphics_object obj = gh_manager::get_object (m_handle);

  if (obj.valid_object ())
    {
      graphics_object figObj (obj.get_ancestor ("figure"));
      graphics_object currentObj, axesObj;
      QList<graphics_object> axesList;

      Matrix children = obj.get_properties ().get_children ();
      octave_idx_type num_children = children.numel ();

      for (int i = 0; i < num_children; i++)
	{
	  graphics_object childObj (gh_manager::get_object (children(i)));

          if (childObj.isa ("axes"))
            axesList.append (childObj);
	  else if (childObj.isa ("uicontrol") || childObj.isa ("uipanel"))
	    {
	      Matrix bb = childObj.get_properties ().get_boundingbox (false);
	      QRectF r (bb(0), bb(1), bb(2), bb(3));

	      r.adjust (-5, -5, 5, 5);
	      if (r.contains (event->posF ()))
		{
		  currentObj = childObj;
		  break;
		}
	    }
	}

      if (! currentObj)
        {
          for (QList<graphics_object>::ConstIterator it = axesList.begin ();
               it != axesList.end (); ++it)
            {
              graphics_object go = selectFromAxes (*it, event->pos ());

              if (go)
                {
                  currentObj = go;
                  axesObj = *it;
                }
              // FIXME: is this really necessary? the axes object should
              //        have been selected through selectFromAxes anyway
              else if (it->get_properties ().is_hittest ())
                {
                  Matrix bb = it->get_properties ().get_boundingbox (true);
                  QRectF r (bb(0), bb(1), bb(2), bb(3));

                  if (r.contains (event->posF ()))
                    axesObj = *it;
                }

              if (axesObj)
                break;
            }

          if (axesObj)
            {
              if (axesObj.get_properties ().handlevisibility_is ("on"))
                Utils::properties<figure> (figObj)
                  .set_currentaxes (axesObj.get_handle ().as_octave_value ());
              if (! currentObj)
                currentObj = axesObj;
            }
        }

      if (! currentObj)
        currentObj = obj;

      if (currentObj.get_properties ().handlevisibility_is ("on"))
        Utils::properties<figure> (figObj)
          .set_currentobject (currentObj.get_handle ().as_octave_value ());
      else
        Utils::properties<figure> (figObj).set_currentobject (octave_NaN);

      Figure* fig = dynamic_cast<Figure*> (Backend::toolkitObject (figObj));

      MouseMode newMouseMode = NoMode;

      if (fig)
	newMouseMode = fig->mouseMode ();

      switch (newMouseMode)
	{
	case NoMode:
	  gh_manager::post_set (figObj.get_handle (), "selectiontype",
				Utils::figureSelectionType (event), false);
	  gh_manager::post_set (figObj.get_handle (), "currentpoint",
				Utils::figureCurrentPoint (figObj, event),
				false);
	  gh_manager::post_callback (figObj.get_handle (),
				     "windowbuttondownfcn");
          gh_manager::post_callback (currentObj.get_handle (),
                                     "buttondownfcn");
	  if (event->button () == Qt::RightButton)
	    ContextMenu::executeAt (currentObj.get_properties (),
				    event->globalPos ());
	  break;
	case RotateMode:
	case ZoomMode:
	case PanMode:
	  if (axesObj)
	    {
	      if (event->buttons () == Qt::LeftButton
		  && event->modifiers () == Qt::NoModifier)
		{
		  m_mouseAnchor = m_mouseCurrent = event->pos ();
		  m_mouseAxes = axesObj.get_handle ();
		  m_mouseMode = newMouseMode;
		}
	      else if (newMouseMode == ZoomMode
		       && event->modifiers () == Qt::NoModifier)
		{
		  switch (event->buttons ())
		    {
		    case Qt::RightButton:
		      Utils::properties<axes> (axesObj).unzoom ();
		      break;
		    case Qt::MidButton:
			{
			  axes::properties& ap =
			    Utils::properties<axes> (axesObj);

			  ap.clear_zoom_stack ();
			  ap.set_xlimmode ("auto");
			  ap.set_ylimmode ("auto");
			  ap.set_zlimmode ("auto");
			}
		      break;
		    }
		  redraw (false);
		}
	    }
	  break;
	default:
	  break;
	}
    }
}

void Canvas::canvasMouseReleaseEvent (QMouseEvent* event)
{
  if (m_mouseMode == ZoomMode
      && m_mouseAxes.ok ()
      && m_mouseAnchor != event->pos ())
    {
      gh_manager::auto_lock lock;
      graphics_object ax = gh_manager::get_object (m_mouseAxes);

      if (ax.valid_object ())
	{
	  axes::properties& ap = Utils::properties<axes> (ax);

	  ColumnVector p0 = ap.pixel2coord (m_mouseAnchor.x (),
					    m_mouseAnchor.y ());
	  ColumnVector p1 = ap.pixel2coord (event->x (),
					    event->y ());

	  Matrix xl (1, 2, 0.0);
	  Matrix yl (1, 2, 0.0);

	  xl(0) = std::min (p0(0), p1(0));
	  xl(1) = std::max (p0(0), p1(0));
	  yl(0) = std::min (p0(1), p1(1));
	  yl(1) = std::max (p0(1), p1(1));

	  ap.zoom (xl, yl);

	  redraw (false);
	}
    }
  else if (m_mouseMode == NoMode)
    {
      gh_manager::auto_lock lock;
      graphics_object obj = gh_manager::get_object (m_handle);

      if (obj.valid_object ())
        {
          graphics_object figObj (obj.get_ancestor ("figure"));

	  gh_manager::post_set (figObj.get_handle (), "currentpoint",
				Utils::figureCurrentPoint (figObj, event),
				false);
          gh_manager::post_callback (figObj.get_handle (),
                                     "windowbuttonupfcn");
        }
    }

  m_mouseAxes = graphics_handle ();
  m_mouseMode = NoMode;
}

bool Canvas::canvasKeyPressEvent (QKeyEvent* event)
{
  if (m_eventMask & KeyPress)
    {
      octave_scalar_map eventData = Utils::makeKeyEventStruct (event);

      gh_manager::post_set (m_handle, "currentcharacter",
                            eventData.getfield ("Character"), false);
      gh_manager::post_callback (m_handle, "keypressfcn", eventData);

      return true;
    }

  return false;
}

bool Canvas::canvasKeyReleaseEvent (QKeyEvent* event)
{
  if (! event->isAutoRepeat () && (m_eventMask & KeyRelease))
    {
      gh_manager::post_callback (m_handle, "keyreleasefcn",
                                 Utils::makeKeyEventStruct (event));

      return true;
    }

  return false;
}

Canvas* Canvas::create (const std::string& /* name */, QWidget* parent,
			const graphics_handle& handle)
{
  // Only OpenGL
  return new GLCanvas (parent, handle);
}

}; // namespace QtHandles
