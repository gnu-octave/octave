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
#include <QBitmap>
#include <QCursor>
#include <QList>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QRectF>

#include "Backend.h"
#include "Canvas.h"
#include "ContextMenu.h"
#include "GLCanvas.h"
#include "QtHandlesUtils.h"

#include "gl2ps-renderer.h"

namespace QtHandles
{

void Canvas::redraw (bool sync)
{
  QWidget *w = qWidget ();

  if (w)
    {
      if (sync)
        w->repaint ();
      else
        w->update ();
    }
}

void Canvas::blockRedraw (bool block)
{
  m_redrawBlocked = block;
}

void Canvas::setCursor (MouseMode mode)
{
  QWidget *w = qWidget ();

  if (w)
    {
      static QCursor origCursor = w->cursor ();

      switch (mode)
        {
        case PanMode:
        case RotateMode:
          w->setCursor (Qt::OpenHandCursor);
          break;

        case ZoomInMode:
        case ZoomOutMode:
          // FIXME: distinguish zoom in/out.
          w->setCursor (QBitmap (":/images/zoom.png"));
          break;

        default:
          w->setCursor (origCursor);
          break;
        }
    }
}

void Canvas::print (const QString& file_cmd, const QString& term)
{
  graphics_object obj = gh_manager::get_object (m_handle);

  if (obj.valid_object ())
    {
      graphics_object figObj (obj.get_ancestor ("figure"));

      gl2ps_print (figObj, file_cmd.toStdString (), term.toStdString ());
    }
}

void Canvas::updateCurrentPoint(const graphics_object& fig,
                                const graphics_object& obj, QMouseEvent* event)
{
  gh_manager::post_set (fig.get_handle (), "currentpoint",
                        Utils::figureCurrentPoint (fig, event), false);

  Matrix children = obj.get_properties ().get_children ();
  octave_idx_type num_children = children.numel ();

  for (int i = 0; i < num_children; i++)
    {
      graphics_object childObj (gh_manager::get_object (children(i)));

      if (childObj.isa ("axes"))
        {
          axes::properties& ap = Utils::properties<axes> (childObj);
          Matrix x_zlim = ap.get_transform_zlim ();
          graphics_xform x_form = ap.get_transform ();

          ColumnVector p1 = x_form.untransform (event->x (), event->y (),
                                                x_zlim(0));
          ColumnVector p2 = x_form.untransform (event->x (), event->y (),
                                                x_zlim(1));

          Matrix cp (2, 3, 0.0);

          cp(0,0) = p1(0); cp(0,1) = p1(1); cp(0,2) = p1(2);
          cp(1,0) = p2(0); cp(1,1) = p2(1); cp(1,2) = p2(2);

          gh_manager::post_set (childObj.get_handle (), "currentpoint", cp,
                                false);
        }
    }
}

void Canvas::canvasToggleAxes (const graphics_handle& handle)
{
  gh_manager::auto_lock lock;

  graphics_object go = gh_manager::get_object (handle);

  if (go.valid_object ())
    {
      figure::properties& fp = Utils::properties<figure> (go);

      graphics_handle ah = fp.get_currentaxes ();

      graphics_object ax = gh_manager::get_object (ah);

      if (ax.valid_object ())
        {
          axes::properties& ap = Utils::properties<axes> (ax);

          if (ap.handlevisibility_is ("on"))
            {
              ap.set_visible (! ap.is_visible ());

              redraw (true);
            }
        }
    }
}

void Canvas::canvasToggleGrid (const graphics_handle& handle)
{
  gh_manager::auto_lock lock;

  graphics_object go = gh_manager::get_object (handle);

  if (go.valid_object ())
    {
      figure::properties& fp = Utils::properties<figure> (go);

      graphics_handle ah = fp.get_currentaxes ();

      graphics_object ax = gh_manager::get_object (ah);

      if (ax.valid_object ())
        {
          axes::properties& ap = Utils::properties<axes> (ax);

          if (ap.handlevisibility_is ("on") && ap.is_visible ())
            {
              std::string tmp;

              // If any grid is off, then turn them all on.  If they are all
              // on, then turn them off.

              std::string state = ((ap.get_xgrid () == "off"
                                    || ap.get_ygrid () == "off"
                                    || ap.get_zgrid () == "off")
                                   ? "on" : "off");

              ap.set_xgrid (state);
              ap.set_ygrid (state);
              ap.set_zgrid (state);

              redraw (true);
            }
        }
    }
}

static void
autoscale_axes (axes::properties& ap)
{
  ap.clear_zoom_stack ();

  ap.set_xlimmode ("auto");
  ap.set_ylimmode ("auto");
  ap.set_zlimmode ("auto");
}

void Canvas::canvasAutoAxes (const graphics_handle& handle)
{
  gh_manager::auto_lock lock;

  graphics_object go = gh_manager::get_object (handle);

  if (go.valid_object ())
    {
      figure::properties& fp = Utils::properties<figure> (go);

      graphics_handle ah = fp.get_currentaxes ();

      graphics_object ax = gh_manager::get_object (ah);

      if (ax.valid_object ())
        {
          axes::properties& ap = Utils::properties<axes> (ax);

          if (ap.handlevisibility_is ("on") && ap.is_visible ())
            {
              autoscale_axes (ap);

              redraw (true);
            }
        }
    }
}

void Canvas::canvasPaintEvent (void)
{
  if (! m_redrawBlocked)
    {
      gh_manager::auto_lock lock;

      draw (m_handle);

      if (m_mouseMode == ZoomInMode && m_mouseAxes.ok ())
        drawZoomBox (m_mouseAnchor, m_mouseCurrent);
    }
}

static bool
pan_enabled (const graphics_object figObj)
{
  // Getting pan mode property:
  octave_value ov_pm
    = Utils::properties<figure> (figObj).get___pan_mode__ ();

  octave_scalar_map pm = ov_pm.scalar_map_value ();

  return pm.contents ("Enable").string_value () == "on";
}

static std::string
pan_mode (const graphics_object figObj)
{
  // Getting pan mode property:
  octave_value ov_pm
    = Utils::properties<figure> (figObj).get___pan_mode__ ();

  octave_scalar_map pm = ov_pm.scalar_map_value ();

  return pm.contents ("Motion").string_value ();
}

static bool
rotate_enabled (const graphics_object figObj)
{
  // Getting rotate mode property:
  octave_value ov_rm
    = Utils::properties<figure> (figObj).get___rotate_mode__ ();

  octave_scalar_map rm = ov_rm.scalar_map_value ();

  return rm.contents ("Enable").string_value () == "on";
}

static bool
zoom_enabled (const graphics_object figObj)
{
  // Getting zoom mode property:
  octave_value ov_zm
    = Utils::properties<figure> (figObj).get___zoom_mode__ ();

  octave_scalar_map zm = ov_zm.scalar_map_value ();

  return zm.contents ("Enable").string_value () == "on";
}

static std::string
zoom_mode (const graphics_object figObj)
{
  // Getting zoom mode property:
  octave_value ov_zm
    = Utils::properties<figure> (figObj).get___zoom_mode__ ();

  octave_scalar_map zm = ov_zm.scalar_map_value ();

  return zm.contents ("Motion").string_value ();
}

static std::string
zoom_direction (const graphics_object figObj)
{
  // Getting zoom mode property:
  octave_value ov_zm
    = Utils::properties<figure> (figObj).get___zoom_mode__ ();

  octave_scalar_map zm = ov_zm.scalar_map_value ();

  return zm.contents ("Direction").string_value ();
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
            ap.rotate3d (m_mouseCurrent.x (), event->x (),
                         m_mouseCurrent.y (), event->y ());

            // Update current mouse position
            m_mouseCurrent = event->pos ();

            // Force immediate redraw
            redraw (true);
          }
          break;

        case ZoomInMode:
        case ZoomOutMode:
          m_mouseCurrent = event->pos ();
          redraw (true);
          break;

        case PanMode:
          {
            graphics_object figObj (ax.get_ancestor ("figure"));

            std::string mode = pan_mode (figObj);

            ColumnVector p0 = ap.pixel2coord (m_mouseCurrent.x (),
                                              m_mouseCurrent.y ());
            ColumnVector p1 = ap.pixel2coord (event->x (),
                                              event->y ());

            ap.translate_view (mode, p0(0), p1(0), p0(1), p1(1));

            // Update current mouse position
            m_mouseCurrent = event->pos ();

            // Force immediate redraw
            redraw (true);
          }

        default:
          break;
        }
    }
  else if (m_mouseMode == NoMode)
    {
      graphics_object obj = gh_manager::get_object (m_handle);

      if (obj.valid_object ())
        {
          graphics_object figObj (obj.get_ancestor ("figure"));

          updateCurrentPoint (figObj, obj, event);
          gh_manager::post_callback (figObj.get_handle (),
                                     "windowbuttonmotionfcn");
        }
    }
}

void Canvas::canvasMouseDoubleClickEvent (QMouseEvent* event)
{
  if (event->buttons () != Qt::LeftButton)
    return;

  gh_manager::auto_lock lock;
  graphics_object obj = gh_manager::get_object (m_handle);

  if (obj.valid_object ())
    {
      graphics_object axesObj;

      Matrix children = obj.get_properties ().get_children ();
      octave_idx_type num_children = children.numel ();

      for (int i = 0; i < num_children; i++)
        {
          graphics_object childObj (gh_manager::get_object (children(i)));

          if (childObj.isa ("axes"))
            {
              graphics_object go = selectFromAxes (childObj, event->pos ());

              if (go)
                {
                  axesObj = childObj;
                  break;
                }
            }
        }

      bool redrawFigure = true;

      if (axesObj)
        {
          graphics_object figObj (obj.get_ancestor ("figure"));

          if (axesObj.get_properties ().handlevisibility_is ("on"))
            {
              Utils::properties<figure> (figObj)
                .set_currentaxes (axesObj.get_handle ().as_octave_value ());

              if (pan_enabled (figObj) || rotate_enabled (figObj)
                  || zoom_enabled (figObj))
                {
                  axes::properties& ap =
                    Utils::properties<axes> (axesObj);

                  autoscale_axes (ap);
                }
            }

          if (redrawFigure)
            redraw (false);
        }
    }
}

static double
button_number (QMouseEvent *event)
{
  double retval = 0;

  switch (event->button ())
    {
    case Qt::LeftButton:
      retval = 1;
      break;

    case Qt::MidButton:
      retval = 2;
      break;

    case Qt::RightButton:
      retval = 3;
      break;

    default:
      break;
    }

  return retval;
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

          updateCurrentPoint (figObj, obj, event);

          gh_manager::post_callback (figObj.get_handle (),
                                     "windowbuttondownfcn");

          gh_manager::post_callback (currentObj.get_handle (),
                                     "buttondownfcn", button_number (event));

          if (event->button () == Qt::RightButton)
            ContextMenu::executeAt (currentObj.get_properties (),
                                    event->globalPos ());
          break;

        case TextMode:
          // Handle text insertion here.
          break;

        case PanMode:
        case RotateMode:
        case ZoomInMode:
        case ZoomOutMode:
          if (axesObj)
            {
              bool redraw_figure = true;

              if (event->modifiers () == Qt::NoModifier)
                {
                  switch (event->buttons ())
                    {
                    case Qt::LeftButton:
                      m_mouseAnchor = m_mouseCurrent = event->pos ();
                      m_mouseAxes = axesObj.get_handle ();
                      m_mouseMode = newMouseMode;
                      break;

                    case Qt::RightButton:
                      Utils::properties<axes> (axesObj).unzoom ();
                      break;

                    case Qt::MidButton:
                        {
                          axes::properties& ap =
                            Utils::properties<axes> (axesObj);

                          autoscale_axes (ap);
                        }
                      break;

                    default:
                      redraw_figure = false;
                      break;
                    }
                }
              else if (event->modifiers () == Qt::ShiftModifier)
                {
                  switch (event->buttons ())
                    {
                    case Qt::LeftButton:
                      Utils::properties<axes> (axesObj).unzoom ();
                      break;

                    default:
                      redraw_figure = false;
                      break;
                    }
                }

              if (redraw_figure)
                redraw (false);
            }
          break;

        default:
          break;
        }
    }
}

void Canvas::canvasMouseReleaseEvent (QMouseEvent* event)
{
  if ((m_mouseMode == ZoomInMode || m_mouseMode == ZoomOutMode)
      && m_mouseAxes.ok ())
    {
      gh_manager::auto_lock lock;
      graphics_object ax = gh_manager::get_object (m_mouseAxes);

      if (ax.valid_object ())
        {
          axes::properties& ap = Utils::properties<axes> (ax);

          graphics_object obj = gh_manager::get_object (m_handle);

          graphics_object figObj (obj.get_ancestor ("figure"));

          std::string zm = zoom_mode (figObj);

          if (m_mouseAnchor == event->pos ())
            {
              double factor = m_mouseMode == ZoomInMode ? 2.0 : 0.5;

              ColumnVector p1 = ap.pixel2coord (event->x (), event->y ());

              ap.zoom_about_point (zm, p1(0), p1(1), factor);
            }
          else
            {
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

              ap.zoom (zm, xl, yl);
            }

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

          updateCurrentPoint (figObj, obj, event);
          gh_manager::post_callback (figObj.get_handle (),
                                     "windowbuttonupfcn");
        }
    }

  m_mouseAxes = graphics_handle ();
  m_mouseMode = NoMode;
}

void Canvas::canvasWheelEvent (QWheelEvent* event)
{
  gh_manager::auto_lock lock;
  graphics_object obj = gh_manager::get_object (m_handle);

  if (obj.valid_object ())
    {
      std::string mode;

      graphics_object axesObj;

      Matrix children = obj.get_properties ().get_children ();
      octave_idx_type num_children = children.numel ();

      for (int i = 0; i < num_children; i++)
        {
          graphics_object childObj (gh_manager::get_object (children(i)));

          if (childObj.isa ("axes"))
            {
              graphics_object go = selectFromAxes (childObj, event->pos ());

              if (go)
                {
                  axesObj = childObj;
                  break;
                }
            }
        }

      if (axesObj)
        {
          MouseMode newMouseMode = NoMode;

          graphics_object figObj (obj.get_ancestor ("figure"));

          Figure* fig = dynamic_cast<Figure*> (Backend::toolkitObject (figObj));

          if (fig)
            newMouseMode = fig->mouseMode ();

          if (axesObj.get_properties ().handlevisibility_is ("on"))
            {
              Utils::properties<figure> (figObj)
                .set_currentaxes (axesObj.get_handle ().as_octave_value ());

              if (zoom_enabled (figObj))
                {
                  if (event->delta () > 0)
                    newMouseMode = ZoomInMode;
                  else
                    newMouseMode = ZoomOutMode;

                  mode = zoom_mode (figObj);
                }
              else if (pan_enabled (figObj))
                {
                  newMouseMode = PanMode;

                  mode = pan_mode (figObj);
                }
            }

          bool redrawFigure = true;

          switch (newMouseMode)
            {
            case ZoomInMode:
            case ZoomOutMode:
              {
                axes::properties& ap = Utils::properties<axes> (axesObj);

                // Control how fast to zoom when using scroll wheel.
                double wheel_zoom_speed = ap.get_mousewheelzoom ();

                // Determine if we're zooming in or out.
                double factor = (newMouseMode == ZoomInMode
                                 ? 1 / (1.0 - wheel_zoom_speed)
                                 : 1.0 - wheel_zoom_speed);

                // FIXME: should we zoom about point for 2D plots?

                ap.zoom (mode, factor);
              }
              break;

            case PanMode:
              {
                axes::properties& ap = Utils::properties<axes> (axesObj);

                double factor = event->delta () > 0 ? 0.1 : -0.1;

                ap.pan (mode, factor);
              }
              break;

            default:
              redrawFigure = false;
              break;
            }

          if (redrawFigure)
            redraw (false);
        }
    }
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
