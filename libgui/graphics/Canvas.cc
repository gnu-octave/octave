/*

Copyright (C) 2011-2018 Michael Goffioul

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QApplication>
#include <QBitmap>
#include <QCursor>
#include <QInputDialog>
#include <QList>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QRectF>

#include "Backend.h"
#include "Canvas.h"
#include "ContextMenu.h"
#include "GLCanvas.h"
#include "QtHandlesUtils.h"

#include "annotation-dialog.h"

#include "oct-opengl.h"
#include "octave-qt-link.h"

#include "builtin-defun-decls.h"

namespace QtHandles
{

  void
  Canvas::redraw (bool sync)
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

  void
  Canvas::blockRedraw (bool block)
  {
    m_redrawBlocked = block;
  }

  void
  Canvas::setCursor (MouseMode mode)
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
            w->setCursor (QPixmap (":/images/zoom-in.png"));
            break;

          case ZoomOutMode:
            w->setCursor (QPixmap (":/images/zoom-out.png"));
            break;

          default:
            w->setCursor (origCursor);
            break;
          }
      }
  }

  /*
     Two updateCurrentPoint() routines are required:
     1) Used for QMouseEvents where cursor position data is in callback from Qt.
     2) Used for QKeyEvents where cursor position must be determined.
  */
  void
  Canvas::updateCurrentPoint (const graphics_object& fig,
                              const graphics_object& obj, QMouseEvent *event)
  {
    gh_manager::auto_lock lock;

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

  void
  Canvas::updateCurrentPoint (const graphics_object& fig,
                              const graphics_object& obj)
  {
    gh_manager::auto_lock lock;

    gh_manager::post_set (fig.get_handle (), "currentpoint",
                          Utils::figureCurrentPoint (fig), false);

    Matrix children = obj.get_properties ().get_children ();
    octave_idx_type num_children = children.numel ();

    for (int i = 0; i < num_children; i++)
      {
        graphics_object childObj (gh_manager::get_object (children(i)));

        if (childObj.isa ("axes"))
          {
            // FIXME: QCursor::pos() may give inaccurate results with
            //        asynchronous window systems like X11 over ssh.
            QWidget *w = qWidget ();
            QPoint p = w->mapFromGlobal (QCursor::pos ());
            axes::properties& ap = Utils::properties<axes> (childObj);
            Matrix x_zlim = ap.get_transform_zlim ();
            graphics_xform x_form = ap.get_transform ();

            ColumnVector p1 = x_form.untransform (p.x (), p.y (), x_zlim(0));
            ColumnVector p2 = x_form.untransform (p.x (), p.y (), x_zlim(1));

            Matrix cp (2, 3, 0.0);

            cp(0,0) = p1(0); cp(0,1) = p1(1); cp(0,2) = p1(2);
            cp(1,0) = p2(0); cp(1,1) = p2(1); cp(1,2) = p2(2);

            gh_manager::post_set (childObj.get_handle (), "currentpoint", cp,
                                  false);
          }
      }
  }

  void
  Canvas::annotation_callback (const octave_value_list& args)
  {
    Ffeval (ovl ("annotation").append (args));

    redraw ();
  }

  void
  Canvas::canvasToggleAxes (const graphics_handle& handle)
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

  void
  Canvas::canvasToggleGrid (const graphics_handle& handle)
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

  static void
  autoscale_axes (axes::properties& ap)
  {
    ap.clear_zoom_stack ();

    ap.set_xlimmode ("auto");
    ap.set_ylimmode ("auto");
    ap.set_zlimmode ("auto");
  }

  void
  Canvas::canvasAutoAxes (const graphics_handle& handle)
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

            autoscale_axes (ap);

            redraw (true);
          }
      }
  }

  void
  Canvas::canvasPaintEvent (void)
  {
    if (! m_redrawBlocked)
      {
        gh_manager::auto_lock lock;

        draw (m_handle);

        if ((m_mouseMode == ZoomInMode && m_mouseAxes.ok ()) || m_rectMode)
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

  void
  Canvas::select_object (graphics_object obj, QMouseEvent *event,
                         graphics_object& currentObj, graphics_object& axesObj,
                         bool axes_only, std::vector<std::string> omit)
  {
    QList<graphics_object> axesList;
    Matrix children = obj.get_properties ().get_all_children ();
    octave_idx_type num_children = children.numel ();

    for (int i = 0; i < num_children; i++)
      {
        graphics_object childObj (gh_manager::get_object (children(i)));

        if (childObj.isa ("axes"))
          {
            auto p = omit.begin ();
            bool omitfound = false;
            while (p != omit.end () && ! omitfound)
              {
                omitfound = (childObj.get ("tag").string_value () == *p);
                p++;
              }
            if (! omitfound)
              axesList.append (childObj);
          }
        else if (childObj.isa ("uicontrol") || childObj.isa ("uipanel")
                 || childObj.isa ("uibuttongroup"))
          {
            Matrix bb = childObj.get_properties ().get_boundingbox (false);
            QRectF r (bb(0), bb(1), bb(2), bb(3));

            r.adjust (-5, -5, 5, 5);

#if defined (HAVE_QMOUSEEVENT_LOCALPOS)
            bool rect_contains_pos = r.contains (event->localPos ());
#else
            bool rect_contains_pos = r.contains (event->posF ());
#endif
            if (rect_contains_pos)
              {
                currentObj = childObj;
                break;
              }
          }
      }

    if (axes_only)
      {
        QPoint pt = event->pos ();

        for (QList<graphics_object>::ConstIterator it = axesList.begin ();
             it != axesList.end (); ++it)
          {
            const axes::properties& ap =
              dynamic_cast<const axes::properties&> ((*it).get_properties ());

            ColumnVector p0 = ap.pixel2coord (pt.x (), pt.y ());
            Matrix xlim = ap.get_xlim ().matrix_value ();
            Matrix ylim = ap.get_ylim ().matrix_value ();

            if (xlim(0) < p0(0) && xlim(1) > p0(0)
                && ylim(0) < p0(1) && ylim(1) > p0(1))
              {
                axesObj = *it;
                return;
              }
          }
      }
    else if (! currentObj)
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

#if defined (HAVE_QMOUSEEVENT_LOCALPOS)
                bool rect_contains_pos = r.contains (event->localPos ());
#else
                bool rect_contains_pos = r.contains (event->posF ());
#endif
                if (rect_contains_pos)
                  axesObj = *it;
              }

            if (axesObj && currentObj)
              break;
          }
      }
  }

  void
  Canvas::canvasMouseMoveEvent (QMouseEvent *event)
  {
    gh_manager::auto_lock lock;
    graphics_object ax = gh_manager::get_object (m_mouseAxes);

    if (m_mouseMode != NoMode && (ax.valid_object () || m_mouseMode == TextMode))
      {
        switch (m_mouseMode)
          {
          case RotateMode:
            {
              axes::properties& ap = Utils::properties<axes> (ax);

              ap.rotate3d (m_mouseCurrent.x (), event->x (),
                           m_mouseCurrent.y (), event->y ());

              // Update current mouse position
              m_mouseCurrent = event->pos ();

              // Force immediate redraw
              redraw (true);
            }
            break;
          case TextMode:
          case ZoomInMode:
          case ZoomOutMode:
            m_mouseCurrent = event->pos ();
            redraw (true);
            break;

          case PanMode:
            {
              axes::properties& ap = Utils::properties<axes> (ax);

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

            if (figObj.valid_object () &&
                ! figObj.get ("windowbuttonmotionfcn").isempty ())
              {
                updateCurrentPoint (figObj, obj, event);
                gh_manager::post_callback (figObj.get_handle (),
                                           "windowbuttonmotionfcn");
              }
          }
      }

    // Update mouse coordinates in the figure window status bar
    graphics_object obj = gh_manager::get_object (m_handle);
    graphics_object figObj = obj.get_ancestor ("figure");

    if (figObj.valid_object () && obj.valid_object ())
      {
        graphics_object currentObj, axesObj;
        std::vector<std::string> omit = {"legend", "colorbar", "scribeoverlay"};
        select_object (obj, event, currentObj, axesObj, true, omit);

        if (axesObj.valid_object ())
          {
            // FIXME: should we use signal/slot mechanism instead of
            //        directly calling parent fig methods
            Figure *fig =
              dynamic_cast<Figure *> (Backend::toolkitObject (figObj));
            axes::properties& ap = Utils::properties<axes> (axesObj);

            if (fig)
              fig->updateStatusBar (ap.pixel2coord (event->x (), event->y ()));
          }
      }
  }

  void
  Canvas::canvasMouseDoubleClickEvent (QMouseEvent *event)
  {
    // same processing as normal click, but event type is MouseButtonDblClick
    canvasMousePressEvent (event);
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

  void
  Canvas::canvasMousePressEvent (QMouseEvent *event)
  {
    gh_manager::auto_lock lock;
    graphics_object obj = gh_manager::get_object (m_handle);

    bool isdblclick = (event->type () == QEvent::MouseButtonDblClick);

    if (obj.valid_object ())
      {
        graphics_object figObj (obj.get_ancestor ("figure"));

        // Any click in a figure canvas makes it current
        if (figObj)
          {
            graphics_object root = gh_manager::get_object (0);
            Utils::properties<root_figure> (root)
              .set_currentfigure (figObj.get_handle ().as_octave_value ());
          }

        graphics_object currentObj, axesObj;

        // Retrieve selected object.
        select_object (obj, event, currentObj, axesObj);

        // currentObj may be invalid if, e.g., all objects under the mouse
        // click had "hittest" -> "off" or "pickableparts" -> "none".  In that
        // case, replace with underlying figObj which always accepts mouse
        // clicks.
        if (! currentObj.valid_object ())
          currentObj = figObj;
        else if (! currentObj.get_properties ().is_hittest ())
          {
            // Objects with "hittest"->"off" pass the mouse event to their
            // parent and so on.
            graphics_object tmpgo;
            tmpgo = gh_manager::get_object (currentObj.get_parent ());
            while (tmpgo && ! tmpgo.get_properties ().is_hittest ())
              tmpgo = gh_manager::get_object (tmpgo.get_parent ());

            if (tmpgo && tmpgo.get_handle () != 0.0)
              currentObj = tmpgo;
            else
              currentObj = graphics_object ();
          }

        if (axesObj)
          {
            if (axesObj.get_properties ().handlevisibility_is ("on")
                && axesObj.get_properties ().get_tag () != "legend"
                && axesObj.get_properties ().get_tag () != "colorbar")
              Utils::properties<figure> (figObj)
              .set_currentaxes (axesObj.get_handle ().as_octave_value ());
          }

        Figure *fig = dynamic_cast<Figure *> (Backend::toolkitObject (figObj));

        MouseMode newMouseMode = NoMode;

        if (fig)
          newMouseMode = fig->mouseMode ();

        switch (newMouseMode)
          {
          case NoMode:
            {
              // Update the figure "currentobject"
              auto& fprop = Utils::properties<figure> (figObj);

              if (currentObj
                  && currentObj.get_properties ().handlevisibility_is ("on"))
                fprop.set_currentobject (currentObj.get_handle ()
                                         .as_octave_value ());
              else
                fprop.set_currentobject (Matrix ());

              // Update figure "selectiontype" and "currentpoint"
              gh_manager::post_set (
                                    figObj.get_handle (), "selectiontype",
                                    Utils::figureSelectionType (event, isdblclick), false);

              updateCurrentPoint (figObj, obj, event);

              gh_manager::post_callback (figObj.get_handle (),
                                         "windowbuttondownfcn",
                                         button_number (event));

              // Execute the "buttondownfcn" of the selected object. If the
              // latter is empty then execute the figure "buttondownfcn"
              if (currentObj && ! currentObj.get ("buttondownfcn").isempty ())
                gh_manager::post_callback (currentObj.get_handle (),
                                           "buttondownfcn",
                                           button_number (event));
              else if (figObj && ! figObj.get ("buttondownfcn").isempty ())
                gh_manager::post_callback (figObj.get_handle (),
                                           "buttondownfcn",
                                           button_number (event));

              // Show context menu of the selected object
              if (currentObj && event->button () == Qt::RightButton)
                ContextMenu::executeAt (currentObj.get_properties (),
                                        event->globalPos ());
            }
            break;

          case TextMode:
            {
              if (event->modifiers () == Qt::NoModifier)
                {
                  switch (event->buttons ())
                    {
                    case Qt::LeftButton:
                      m_mouseAnchor = m_mouseCurrent = event->pos ();
                      m_mouseMode = newMouseMode;
                      m_rectMode = true;
                    }
                }
              redraw (false);
            }
            break;

          case PanMode:
          case RotateMode:
          case ZoomInMode:
          case ZoomOutMode:
            if (axesObj && axesObj.get_properties ().handlevisibility_is ("on"))
              {
                bool redraw_figure = true;

                if (isdblclick)
                  {
                    if (event->button () == Qt::LeftButton)
                      {
                        axes::properties& ap = Utils::properties<axes> (axesObj);

                        autoscale_axes (ap);
                      }
                    else
                      {
                        redraw_figure = false;
                      }
                  }
                else if (event->modifiers () == Qt::NoModifier)
                  {
                    switch (event->buttons ())
                      {
                      case Qt::LeftButton:
                        m_mouseAnchor = m_mouseCurrent = event->pos ();
                        m_mouseAxes = axesObj.get_handle ();
                        m_mouseMode = newMouseMode;
                        m_clickMode = newMouseMode == ZoomInMode;
                        break;

                      case Qt::RightButton:
                        if (newMouseMode == ZoomInMode)
                          {
                            m_mouseAnchor = m_mouseCurrent = event->pos ();
                            m_mouseAxes = axesObj.get_handle ();
                            m_mouseMode = newMouseMode;
                            m_clickMode = false;
                          }

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
                        if (newMouseMode == ZoomInMode)
                          {
                            m_mouseAnchor = m_mouseCurrent = event->pos ();
                            m_mouseAxes = axesObj.get_handle ();
                            m_mouseMode = newMouseMode;
                            m_clickMode = false;
                          }
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

  void
  Canvas::canvasMouseReleaseEvent (QMouseEvent *event)
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
                double factor = (m_clickMode ? 2.0 : 0.5);

                ColumnVector p1 = ap.pixel2coord (event->x (), event->y ());

                ap.zoom_about_point (zm, p1(0), p1(1), factor);
              }
            else if (m_mouseMode == ZoomInMode)
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
    else if (m_mouseMode == TextMode)
      {
        gh_manager::auto_lock lock;

        graphics_object figObj =
          gh_manager::get_object (m_handle).get_ancestor ("figure");
        if (figObj.valid_object ())
          {
            QWidget *w = qWidget ();
            if (w)
              {
                Matrix bb = figObj.get ("position").matrix_value ();
                bb(0) = m_mouseAnchor.x () / bb(2);
                bb(1) = 1.0 - (m_mouseAnchor.y () / bb(3));
                bb(2) = (event->x () - m_mouseAnchor.x ()) / bb(2);
                bb(3) = (m_mouseAnchor.y () - event->y ()) / bb(3);

                octave_value_list props = ovl ("textbox", bb);

                annotation_dialog anno_dlg (w, props);

                if (anno_dlg.exec () == QDialog::Accepted)
                  {
                    props = anno_dlg.get_properties ();
                    props.prepend (figObj.get_handle ().as_octave_value ());

                    octave_link::post_event (this, &Canvas::annotation_callback,
                                             props);
                  }
              }
          }
      }
    m_rectMode = false;
    m_mouseAxes = graphics_handle ();
    m_mouseMode = NoMode;
  }

  void
  Canvas::canvasWheelEvent (QWheelEvent *event)
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

            Figure *fig = dynamic_cast<Figure *> (Backend::toolkitObject (figObj));

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

                  double factor = (event->delta () > 0 ? 0.1 : -0.1);

                  if (event->modifiers () == Qt::NoModifier
                      && mode != "horizontal")
                    ap.pan ("vertical", factor);
                  else if (event->modifiers () == Qt::ShiftModifier
                      && mode != "vertical")
                    ap.pan ("horizontal", factor);
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

  bool
  Canvas::canvasKeyPressEvent (QKeyEvent *event)
  {
    if (m_eventMask & KeyPress)
      {
        gh_manager::auto_lock lock;
        graphics_object obj = gh_manager::get_object (m_handle);

        if (obj.valid_object ())
          {
            graphics_object figObj (obj.get_ancestor ("figure"));

            updateCurrentPoint (figObj, obj);
          }

        octave_scalar_map eventData = Utils::makeKeyEventStruct (event);

        gh_manager::post_set (m_handle, "currentcharacter",
                              eventData.getfield ("Character"), false);
        gh_manager::post_callback (m_handle, "keypressfcn", eventData);

        return true;
      }

    return false;
  }

  bool
  Canvas::canvasKeyReleaseEvent (QKeyEvent *event)
  {
    if (! event->isAutoRepeat () && (m_eventMask & KeyRelease))
      {
        gh_manager::post_callback (m_handle, "keyreleasefcn",
                                   Utils::makeKeyEventStruct (event));

        return true;
      }

    return false;
  }

  Canvas*
  Canvas::create (const std::string& /* name */, QWidget *parent,
                  const graphics_handle& handle)
  {
    // Only OpenGL
    return new GLCanvas (parent, handle);
  }

}
