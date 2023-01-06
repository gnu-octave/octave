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

#if ! defined (octave_Figure_h)
#define octave_Figure_h 1

#include <QRect>
#include <QStatusBar>

#include "GenericEventNotify.h"
#include "MenuContainer.h"
#include "Object.h"

class QMainWindow;
class QToolBar;
class QScreen;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

enum MouseMode
  {
    NoMode,
    RotateMode,
    ZoomInMode,
    ZoomOutMode,
    PanMode,
    SelectMode,
    TextMode
  };

class Container;
class FigureWindow;
class MenuBar;
class ToolBar;

class Figure :
  public Object,
  public MenuContainer,
  public GenericEventNotifyReceiver
{
  Q_OBJECT

  friend class ToolBar;

public:
  Figure (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_object& go, FigureWindow *win);
  ~Figure (void);

  static Figure *
  create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_object& go);

  QString fileName (void);
  void setFileName (const QString& name);

  MouseMode mouseMode (void);

  Container * innerContainer (void);
  QWidget * menu (void);
  void updateStatusBar (ColumnVector pt);

  void do_connections (const QObject *receiver,
                       const QObject *emitter = nullptr);

  bool eventNotifyBefore (QObject *watched, QEvent *event);
  void eventNotifyAfter (QObject *watched, QEvent *event);

protected:
  enum UpdateBoundingBoxFlag
    {
      UpdateBoundingBoxPosition = 0x1,
      UpdateBoundingBoxSize     = 0x2,
      UpdateBoundingBoxAll      = 0x3
    };

protected:
  void redraw (void);
  void show (void);
  void print (const QString& file_cmd, const QString& term);
  void update (int pId);
  void updateBoundingBox (bool internal = false, int flags = 0);
  void beingDeleted (void);

private:
  void showFigureStatusBar (bool visible);
  void addCustomToolBar (QToolBar *bar, bool visible, bool isdefault);
  void showCustomToolBar (QToolBar *bar, bool visible);
  void set_geometry (QRect r);

  void enableMouseTracking (void);

private slots:
  void updateFigureHeight (int delta_h);
  void updateContainer (void);
  void figureWindowShown ();
  void screenChanged (QScreen *);

public slots:
  uint8NDArray slotGetPixels (void);

signals:
  void asyncUpdate (void);
  void interpreter_event (const octave::fcn_callback& fcn);
  void interpreter_event (const octave::meth_callback& meth);

private:
  Container *m_container;
  bool m_blockUpdates;
  QToolBar *m_figureToolBar;
  MenuBar *m_menuBar;
  QStatusBar *m_statusBar;
  QRect m_innerRect;
  QRect m_outerRect;
  QImage m_pointer_cdata;
  int m_previousHeight;
  bool m_resizable;
};

OCTAVE_END_NAMESPACE(octave)

#endif
