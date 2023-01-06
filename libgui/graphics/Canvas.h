////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_Canvas_h)
#define octave_Canvas_h 1

#include <QObject>
#include <QPoint>

#include "event-manager.h"
#include "graphics.h"

#include "Figure.h"

class QKeyEvent;
class QMouseEvent;
class QWheelEvent;
class QWidget;

class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

class Canvas : public QObject
{
  Q_OBJECT

public:
  enum EventMask
    {
      KeyPress   = 0x01,
      KeyRelease = 0x02
    };

public:
  virtual ~Canvas (void) = default;

  void redraw (bool sync = false);
  void blockRedraw (bool block = true);

  void print (const QString& file_cmd, const QString& term)
  {
    do_print (file_cmd, term, m_handle);
  }

  void addEventMask (int m) { m_eventMask |= m; }
  void clearEventMask (int m) { m_eventMask &= (~m); }
  void setEventMask (int m) { m_eventMask = m; }

  void setCursor (MouseMode mode, std::string fallback,
                  QImage cdata, Matrix hotspot);

  virtual QWidget * qWidget (void) = 0;

  static Canvas *
  create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_handle& handle, QWidget *parent,
          const std::string& name);

  virtual uint8NDArray getPixels (void) { return do_getPixels (m_handle); };

signals:

  void interpreter_event (const octave::fcn_callback& fcn);
  void interpreter_event (const octave::meth_callback& meth);

  void gh_callback_event (const graphics_handle& h, const std::string& name);

  void gh_callback_event (const graphics_handle& h, const std::string& name,
                          const octave_value& data);

  void gh_set_event (const graphics_handle& h, const std::string& name,
                     const octave_value& value);

  void gh_set_event (const graphics_handle& h, const std::string& name,
                     const octave_value& value, bool notify_toolkit);

  void gh_set_event (const graphics_handle& h, const std::string& name,
                     const octave_value& value, bool notify_toolkit,
                     bool redraw_figure);
protected:
  virtual void draw (const graphics_handle& handle) = 0;
  virtual void drawZoomBox (const QPoint& p1, const QPoint& p2) = 0;
  virtual void resize (int x, int y, int width, int height) = 0;
  virtual graphics_object selectFromAxes (const graphics_object& ax,
                                          const QPoint& pt) = 0;
  virtual uint8NDArray do_getPixels (const graphics_handle& handle) = 0;
  virtual void do_print (const QString& file_cmd, const QString& term,
                         const graphics_handle& handle) = 0;

protected:
  Canvas (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_handle& handle)
    : m_octave_qobj (oct_qobj),
      m_interpreter (interp),
      m_handle (handle),
      m_redrawBlocked (false),
      m_mouseMode (NoMode),
      m_clickMode (false),
      m_eventMask (0),
      m_rectMode (false)
  { }

  void canvasToggleAxes (const graphics_handle& handle);
  void canvasToggleGrid (const graphics_handle& handle);
  void canvasAutoAxes (const graphics_handle& handle);
  void canvasPaintEvent (void);
  void canvasMouseDoubleClickEvent (QMouseEvent *event);
  void canvasMouseMoveEvent (QMouseEvent *event);
  void canvasMousePressEvent (QMouseEvent *event);
  void canvasMouseReleaseEvent (QMouseEvent *event);
  void canvasWheelEvent (QWheelEvent *event);
  bool canvasKeyPressEvent (QKeyEvent *event);
  bool canvasKeyReleaseEvent (QKeyEvent *event);

  void updateCurrentPoint (const graphics_object& fig,
                           const graphics_object& obj, QMouseEvent *event);
  void updateCurrentPoint (const graphics_object& fig,
                           const graphics_object& obj);

  void select_object (graphics_object obj, QMouseEvent *event,
                      graphics_object& currentObj, graphics_object& axesObj,
                      bool axes_only = false,
                      std::vector<std::string> omit = std::vector<std::string> ());

protected:
  octave::base_qobject& m_octave_qobj;
  octave::interpreter& m_interpreter;

private:

  QCursor make_cursor (const QString& name, int hot_x  = -1, int hot_y = -1);

  graphics_handle m_handle;
  bool m_redrawBlocked;
  MouseMode m_mouseMode;
  bool m_clickMode;              // True: ZoomIn, False: ZoomOut
  QPoint m_mouseAnchor;
  QPoint m_mouseCurrent;
  graphics_handle m_mouseAxes;
  int m_eventMask;
  bool m_rectMode;
};

OCTAVE_END_NAMESPACE(octave)

#endif
