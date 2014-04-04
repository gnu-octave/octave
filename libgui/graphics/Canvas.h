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

#ifndef __QtHandles_Canvas__
#define __QtHandles_Canvas__ 1

#include <QPoint>

#include "graphics.h"

#include "Figure.h"

class QKeyEvent;
class QMouseEvent;
class QWidget;

namespace QtHandles
{

class Canvas
{
public:
  enum EventMask
    {
      KeyPress   = 0x01,
      KeyRelease = 0x02
    };

public:
  virtual ~Canvas (void) { }

  void redraw (bool sync = false);
  void blockRedraw (bool block = true);

  void addEventMask (int m) { m_eventMask |= m; }
  void clearEventMask (int m) { m_eventMask &= (~m); }
  void setEventMask (int m) { m_eventMask = m; }

  virtual QWidget* qWidget (void) = 0;

  static Canvas* create (const std::string& name, QWidget* parent,
			 const graphics_handle& handle);

protected:
  virtual void draw (const graphics_handle& handle) = 0;
  virtual void drawZoomBox (const QPoint& p1, const QPoint& p2) = 0;
  virtual void resize (int x, int y, int width, int height) = 0;
  virtual graphics_object selectFromAxes (const graphics_object& ax,
                                          const QPoint& pt) = 0;

protected:
  Canvas (const graphics_handle& handle)
    : m_handle (handle),
      m_redrawBlocked (false),
      m_mouseMode (NoMode),
      m_eventMask (0)
    { }

  void canvasPaintEvent (void);
  void canvasMouseMoveEvent (QMouseEvent* event);
  void canvasMousePressEvent (QMouseEvent* event);
  void canvasMouseReleaseEvent (QMouseEvent* event);
  bool canvasKeyPressEvent (QKeyEvent* event);
  bool canvasKeyReleaseEvent (QKeyEvent* event);

  void updateCurrentPoint (const graphics_object& fig,
                           const graphics_object& obj, QMouseEvent *event);

private:
  graphics_handle m_handle;
  bool m_redrawBlocked;
  MouseMode m_mouseMode;
  QPoint m_mouseAnchor;
  QPoint m_mouseCurrent;
  graphics_handle m_mouseAxes;
  int m_eventMask;
};

}; // namespace QtHandles

#endif
