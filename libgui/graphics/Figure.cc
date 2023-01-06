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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QAction>
#include <QActionEvent>
#include <QApplication>
#include <QClipboard>
#include <QEvent>
#include <QFileDialog>
#include <QFileInfo>
#include <QFrame>
#include <QImage>
#include <QMainWindow>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QtDebug>
#include <QTimer>
#include <QToolBar>
#if defined (HAVE_QSCREEN_DEVICEPIXELRATIO)
#  include <QWindow>
#  include <QScreen>
#endif

#include "Canvas.h"
#include "Container.h"
#include "Figure.h"
#include "FigureWindow.h"
#include "QtHandlesUtils.h"

#include "gui-preferences-global.h"
#include "qt-interpreter-events.h"

#include "file-ops.h"
#include "unwind-prot.h"
#include "utils.h"
#include "version.h"

#include "builtin-defun-decls.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DECLARE_GENERICEVENTNOTIFY_SENDER(MenuBar, QMenuBar);

static QRect
boundingBoxToRect (const Matrix& bb)
{
  QRect r;

  if (bb.numel () == 4)
    {
      r = QRect (octave::math::round (bb(0)), octave::math::round (bb(1)),
                 octave::math::round (bb(2)), octave::math::round (bb(3)));
      if (! r.isValid ())
        r = QRect ();
    }

  return r;
}

static QImage
pointer_to_qimage (const Matrix& cdata)
{
  QImage retval (cdata.rows (), cdata.columns (), QImage::Format_ARGB32);
  QColor tmp ("White");
  QColor black ("Black");
  QColor white ("White");
  for (octave_idx_type ii = 0; ii < cdata.rows (); ii++)
    for (octave_idx_type jj = 0; jj < cdata.columns (); jj++)
      {
        if (cdata(ii, jj) == 1.0)
          tmp = black;
        else if (cdata(ii, jj) == 2.0)
          tmp = white;
        else
          tmp.setAlpha (0);

        retval.setPixel (jj, ii, tmp.rgba ());
      }

  return retval;
}

Figure *
Figure::create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
                const graphics_object& go)
{
  return new Figure (oct_qobj, interp, go, new FigureWindow ());
}

Figure::Figure (octave::base_qobject& oct_qobj, octave::interpreter& interp,
                const graphics_object& go, FigureWindow *win)
  : Object (oct_qobj, interp, go, win), m_blockUpdates (false),
    m_figureToolBar (nullptr), m_menuBar (nullptr), m_innerRect (),
    m_outerRect (), m_previousHeight (0), m_resizable (true)
{
  m_container = new Container (win, oct_qobj, interp);
  win->setCentralWidget (m_container);

  connect (m_container, QOverload<const octave::fcn_callback&>::of (&Container::interpreter_event),
           this, QOverload<const octave::fcn_callback&>::of (&Figure::interpreter_event));

  connect (m_container, QOverload<const octave::meth_callback&>::of (&Container::interpreter_event),
           this, QOverload<const octave::meth_callback&>::of (&Figure::interpreter_event));

  figure::properties& fp = properties<figure> ();

  // Adjust figure position
  m_innerRect = boundingBoxToRect (fp.get_boundingbox (true));
  m_outerRect = boundingBoxToRect (fp.get_boundingbox (false));

  set_geometry (m_innerRect);

  // Menubar
  m_menuBar = new MenuBar (win);
  win->setMenuBar (m_menuBar);
  m_menuBar->addReceiver (this);
  m_menuBar->setStyleSheet (m_menuBar->styleSheet () + global_menubar_style);

  // Status bar
  m_statusBar = win->statusBar ();
  m_statusBar->setVisible (false);

  if (fp.toolbar_is ("figure")
      || (fp.toolbar_is ("auto") && fp.menubar_is ("figure")))
    showFigureStatusBar (true);

  // Enable mouse tracking unconditionally
  enableMouseTracking ();

  // When this constructor gets called all properties are already
  // set, even non default.  We force "update" here to get things right.

  // Figure title
  update (figure::properties::ID_NUMBERTITLE);

  // Decide what keyboard events we listen to
  m_container->canvas (m_handle)->setEventMask (0);
  update (figure::properties::ID_KEYPRESSFCN);
  update (figure::properties::ID_KEYRELEASEFCN);

  // modal style
  update (figure::properties::ID_WINDOWSTYLE);

  // Handle resizing constraints
  update (figure::properties::ID_RESIZE);

  // Custom pointer data
  update (figure::properties::ID_POINTERSHAPECDATA);

  // Visibility
  update (figure::properties::ID_VISIBLE);

  connect (this, &Figure::asyncUpdate, this, &Figure::updateContainer);

  // Register for the signal that indicates when a window has moved
  // to a different screen
  connect (win, &FigureWindow::figureWindowShown,
           this, &Figure::figureWindowShown);

  win->addReceiver (this);
  m_container->addReceiver (this);
}

Figure::~Figure (void)
{ }

QString
Figure::fileName (void)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  const figure::properties& fp = properties<figure> ();

  std::string name = fp.get_filename ();

  return QString::fromStdString (name);
}

void
Figure::setFileName (const QString& name)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  figure::properties& fp = properties<figure> ();

  fp.set_filename (name.toStdString ());
}

MouseMode
Figure::mouseMode (void)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  const figure::properties& fp = properties<figure> ();

  std::string mode = fp.get___mouse_mode__ ();

  if (mode == "zoom")
    {
      octave_scalar_map zm = fp.get___zoom_mode__ ().scalar_map_value ();

      std::string direction = zm.getfield ("Direction").string_value ();

      mode += ' ' + direction;
    }

  if (mode == "rotate")
    return RotateMode;
  else if (mode == "zoom in")
    return ZoomInMode;
  else if (mode == "zoom out")
    return ZoomOutMode;
  else if (mode == "pan")
    return PanMode;
  else if (mode == "text")
    return TextMode;

  return NoMode;
}

void
Figure::set_geometry (QRect r)
{
  QMainWindow *win = qWidget<QMainWindow> ();

  if (! m_resizable)
    {
      win->setSizePolicy (QSizePolicy::Preferred, QSizePolicy::Preferred);
      win->setFixedSize (QSize( QWIDGETSIZE_MAX, QWIDGETSIZE_MAX));
    }

  // Unlock window if it is maximized or full-screen
  int state = win->windowState ();
  if (state == Qt::WindowFullScreen || state == Qt::WindowMaximized)
    win->setWindowState (Qt::WindowNoState);

  win->setGeometry (r);

  if (! m_resizable)
    {
      win->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
      win->setFixedSize(win->size ());
    }
}

Container *
Figure::innerContainer (void)
{
  return m_container;
}

void
Figure::redraw (void)
{
  Canvas *canvas = m_container->canvas (m_handle);

  if (canvas)
    canvas->redraw ();

  for (auto *qobj : qWidget<QWidget> ()->findChildren<QObject *> ())
    {
      if (qobj->objectName () == "UIPanel"
          || qobj->objectName () == "UIButtonGroup"
          || qobj->objectName () == "UIControl"
          || qobj->objectName () == "UITable")
        {
          Object *obj = Object::fromQObject (qobj);

          if (obj)
            obj->slotRedraw ();
        }
    }
}

void
Figure::show (void)
{
  QWidget *win = qWidget<QWidget> ();

  win->activateWindow ();
  win->raise ();
}

void
Figure::print (const QString& file_cmd, const QString& term)
{
  Canvas *canvas = m_container->canvas (m_handle);

  if (canvas)
    canvas->print (file_cmd, term);
}

uint8NDArray
Figure::slotGetPixels (void)
{
  uint8NDArray retval;
  Canvas *canvas = m_container->canvas (m_handle);

  if (canvas)
    {
      gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

      gh_mgr.process_events ();
      octave::autolock guard (gh_mgr.graphics_lock ());
      retval = canvas->getPixels ();
    }

  return retval;
}

void
Figure::beingDeleted (void)
{
  Canvas *canvas = m_container->canvas (m_handle.value (), false);

  if (canvas)
    canvas->blockRedraw (true);

  m_container->removeReceiver (this);
  qWidget<FigureWindow> ()->removeReceiver (this);
}

void
Figure::update (int pId)
{
  if (m_blockUpdates)
    return;

  figure::properties& fp = properties<figure> ();

  if (fp.is___printing__ ())
    return;

  QMainWindow *win = qWidget<QMainWindow> ();

  // If the window doesn't exist, there's nothing we can do.
  if (! win)
    return;

  m_blockUpdates = true;

  switch (pId)
    {
    case figure::properties::ID_POSITION:
      {
        m_innerRect = boundingBoxToRect (fp.get_boundingbox (true));
        int toffset = 0;
        int boffset = 0;

        for (auto *tb : win->findChildren<QToolBar *> ())
          if (! tb->isHidden ())
            toffset += tb->sizeHint ().height ();

        if (! m_menuBar->isHidden ())
          toffset += m_menuBar->sizeHint ().height ();

        if (! m_statusBar->isHidden ())
          boffset += m_statusBar->sizeHint ().height ();

        set_geometry (m_innerRect.adjusted (0, -toffset, 0, boffset));
      }
      break;

    case figure::properties::ID_NAME:
    case figure::properties::ID_NUMBERTITLE:
      win->setWindowTitle (Utils::fromStdString (fp.get_title ()));
      break;

    case figure::properties::ID_VISIBLE:
      if (fp.is_visible ())
        {
          QTimer::singleShot (0, win, &QMainWindow::show);
          if (! fp.is___gl_window__ ())
            {
              gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

              octave::autolock guard (gh_mgr.graphics_lock ());
              fp.set ("__gl_window__", "on");
            }
        }
      else
        win->hide ();
      break;

    case figure::properties::ID_RESIZE:
      if (fp.is_resize ())
        {
          win->setSizePolicy (QSizePolicy::Preferred, QSizePolicy::Preferred);
          win->setFixedSize (QSize( QWIDGETSIZE_MAX, QWIDGETSIZE_MAX));
          m_resizable = true;
        }
      else
        {
          win->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
          win->setFixedSize(win->size ());
          m_resizable = false;
        }
      break;

    case figure::properties::ID_MENUBAR:
    case figure::properties::ID_TOOLBAR:
      if (fp.toolbar_is ("none"))
        showFigureStatusBar (false);
      else if (fp.toolbar_is ("figure"))
        showFigureStatusBar (true);
      else  // "auto"
        showFigureStatusBar (fp.menubar_is ("figure"));
      break;

    case figure::properties::ID_KEYPRESSFCN:
      if (fp.get_keypressfcn ().isempty ())
        m_container->canvas (m_handle)->clearEventMask (Canvas::KeyPress);
      else
        m_container->canvas (m_handle)->addEventMask (Canvas::KeyPress);
      // Signal the change to uipanels as well
      for (auto *qobj : qWidget<QWidget> ()->findChildren<QObject *> ())
        {
          if (qobj->objectName () == "UIPanel")
            {
              Object *obj = Object::fromQObject (qobj);

              if (obj)
                {
                  if (fp.get_keypressfcn ().isempty ())
                    obj->innerContainer ()->canvas (m_handle)->
                      clearEventMask (Canvas::KeyPress);
                  else
                    obj->innerContainer ()->canvas (m_handle)->
                      addEventMask (Canvas::KeyPress);
                }
            }
        }
      break;

    case figure::properties::ID_KEYRELEASEFCN:
      if (fp.get_keyreleasefcn ().isempty ())
        m_container->canvas (m_handle)->clearEventMask (Canvas::KeyRelease);
      else
        m_container->canvas (m_handle)->addEventMask (Canvas::KeyRelease);
      break;
      // Signal the change to uipanels as well
      for (auto *qobj : qWidget<QWidget> ()->findChildren<QObject *> ())
        {
          if (qobj->objectName () == "UIPanel")
            {
              Object *obj = Object::fromQObject (qobj);

              if (obj)
                {
                  if (fp.get_keypressfcn ().isempty ())
                    obj->innerContainer ()->canvas (m_handle)->
                      clearEventMask (Canvas::KeyRelease);
                  else
                    obj->innerContainer ()->canvas (m_handle)->
                      addEventMask (Canvas::KeyRelease);
                }
            }
        }
      break;

    case figure::properties::ID_WINDOWSTYLE:
      if (fp.windowstyle_is ("modal"))
        {
          bool is_visible = win->isVisible ();

          // if window is already visible, need to hide and reshow it in order to
          // make it use the modal settings
          if (is_visible)
            win->setVisible (false);

          win->setWindowModality (Qt::ApplicationModal);
          win->setVisible (is_visible);
        }
      else
        win->setWindowModality (Qt::NonModal);

      break;

    case figure::properties::ID_POINTERSHAPECDATA:
      m_pointer_cdata =
        pointer_to_qimage (fp.get_pointershapecdata ().matrix_value ());
      if (fp.get_pointer () != "custom")
        break;
      OCTAVE_FALLTHROUGH;

    case figure::properties::ID_POINTER:
    case figure::properties::ID_POINTERSHAPEHOTSPOT:
    case figure::properties::ID___MOUSE_MODE__:
    case figure::properties::ID___ZOOM_MODE__:
      m_container->canvas (m_handle)->setCursor (mouseMode (),
                                                 fp.get_pointer (),
                                                 m_pointer_cdata,
                                                 fp.get_pointershapehotspot ()
                                                 .matrix_value());
      break;

    default:
      break;
    }

  m_blockUpdates = false;
}

void
Figure::showFigureStatusBar (bool visible)
{
  if (m_statusBar
      && (! m_statusBar->isHidden ()) != visible)
    {
      int dy = m_statusBar->sizeHint ().height ();
      QRect r = qWidget<QWidget> ()->geometry ();

      if (! visible)
        r.adjust (0, 0, 0, -dy);
      else
        r.adjust (0, 0, 0, dy);

      m_blockUpdates = true;
      set_geometry (r);
      m_statusBar->setVisible (visible);
      m_blockUpdates = false;

      updateBoundingBox (false);
    }
}

void
Figure::updateFigureHeight (int dh)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());
  graphics_object go = object ();

  if (go.valid_object () && dh != 0)
    {
      QRect r = qWidget<QWidget> ()->geometry ();

      r.adjust (0, dh, 0, 0);

      m_blockUpdates = true;
      set_geometry (r);
      m_blockUpdates = false;

      updateBoundingBox (false);
    }
}

void
Figure::updateStatusBar (ColumnVector pt)
{
  if (! m_statusBar->isHidden ())
    m_statusBar->showMessage (QString ("(%1, %2)")
                              .arg (pt(0), 0, 'g', 5)
                              .arg (pt(1), 0, 'g', 5));
}

void
Figure::do_connections (const QObject *receiver, const QObject * /* emitter */)
{
  Object::do_connections (receiver);
  Object::do_connections (receiver, m_container->canvas (m_handle));
}

QWidget *
Figure::menu (void)
{
  return qWidget<QMainWindow> ()->menuBar ();
}

void
Figure::updateBoundingBox (bool internal, int flags)
{
  QWidget *win = qWidget<QWidget> ();
  Matrix bb (1, 4);
  std::string prop;

  if (internal)
    {
      prop = "position";
      QRect r = m_innerRect;

      if (flags & UpdateBoundingBoxPosition)
        r.moveTopLeft (win->mapToGlobal (m_container->pos ()));
      if (flags & UpdateBoundingBoxSize)
        r.setSize (m_container->size ());

      if (r.isValid () && r != m_innerRect)
        {
          m_innerRect = r;

          bb(0) = r.x ();
          bb(1) = r.y ();
          bb(2) = r.width ();
          bb(3) = r.height ();
        }
      else
        return;
    }
  else
    {
      prop = "outerposition";
      QRect r = m_outerRect;

      if (flags & UpdateBoundingBoxPosition)
        r.moveTopLeft (win->pos ());
      if (flags & UpdateBoundingBoxSize)
        r.setSize (win->frameGeometry ().size ());

      if (r.isValid () && r != m_outerRect)
        {
          m_outerRect = r;

          bb(0) = r.x ();
          bb(1) = r.y ();
          bb(2) = r.width ();
          bb(3) = r.height ();
        }
      else
        return;
    }

  figure::properties& fp = properties<figure> ();

  emit gh_set_event (m_handle, prop, fp.bbox2position (bb), false,
                     prop == "position");
}

bool
Figure::eventNotifyBefore (QObject *obj, QEvent *xevent)
{
  if (! m_blockUpdates)
    {
      // Clicking the toolbar or the menubar makes this figure current
      if (xevent->type () == QEvent::MouseButtonPress)
        {
          figure::properties& fp = properties<figure> ();

          gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

          graphics_object root = gh_mgr.get_object (0);

          if (fp.get_handlevisibility () == "on")
            root.set ("currentfigure",
                      fp.get___myhandle__ ().as_octave_value ());
        }

      if (obj == m_container)
        {
          // Do nothing...
        }
      else if (obj == m_menuBar)
        {
          switch (xevent->type ())
            {
            case QEvent::ActionAdded:
            case QEvent::ActionChanged:
            case QEvent::ActionRemoved:
              m_previousHeight = m_menuBar->sizeHint ().height ();

            default:
              break;
            }
        }
      else
        {
          switch (xevent->type ())
            {
            case QEvent::Close:
              xevent->ignore ();
              emit gh_callback_event (m_handle, "closerequestfcn");
              return true;

            default:
              break;
            }
        }
    }

  return false;
}

void
Figure::eventNotifyAfter (QObject *watched, QEvent *xevent)
{
  if (! m_blockUpdates)
    {
      if (watched == m_container)
        {
          gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

          switch (xevent->type ())
            {
            case QEvent::Resize:
              updateBoundingBox (true, UpdateBoundingBoxSize);
              break;

            case QEvent::ChildAdded:
              if (dynamic_cast<QChildEvent *> (xevent)->child
                  ()->isWidgetType())
                {
                  octave::autolock guard (gh_mgr.graphics_lock ());
                  update (figure::properties::ID_TOOLBAR);

                  enableMouseTracking ();
                }
              break;

            case QEvent::ChildRemoved:
              if (dynamic_cast<QChildEvent *> (xevent)->child
                  ()->isWidgetType())
                {
                  octave::autolock guard (gh_mgr.graphics_lock ());
                  update (figure::properties::ID_TOOLBAR);
                }
              break;

            default:
              break;
            }
        }
      else if (watched == m_menuBar)
        {
          switch (xevent->type ())
            {
            case QEvent::ActionAdded:
            case QEvent::ActionChanged:
            case QEvent::ActionRemoved:
              // The menubar may have been resized if no action is visible
              {
                QAction *a = dynamic_cast<QActionEvent *> (xevent)->action ();
                int currentHeight = m_menuBar->sizeHint ().height ();
                if (currentHeight != m_previousHeight
                    && ! a->isSeparator ())
                  updateFigureHeight (m_previousHeight - currentHeight);
              }
              break;

            default:
              break;
            }
        }
      else
        {
          switch (xevent->type ())
            {
            case QEvent::Move:
              updateBoundingBox (false, UpdateBoundingBoxPosition);
              updateBoundingBox (true, UpdateBoundingBoxPosition);
              break;

            case QEvent::Resize:
              updateBoundingBox (false, UpdateBoundingBoxSize);
              break;

            default:
              break;
            }
        }
    }
}

void
Figure::addCustomToolBar (QToolBar *bar, bool visible, bool isdefault)
{
  QMainWindow *win = qWidget<QMainWindow> ();

  if (isdefault)
    m_figureToolBar = bar;

  if (! visible)
    win->addToolBar (bar);
  else
    {
      QSize sz = bar->sizeHint ();
      QRect r = win->geometry ();

      r.adjust (0, -sz.height (), 0, 0);

      m_blockUpdates = true;
      set_geometry (r);
      win->addToolBarBreak ();
      win->addToolBar (bar);
      m_blockUpdates = false;

      updateBoundingBox (false);
    }
}

void
Figure::showCustomToolBar (QToolBar *bar, bool visible)
{
  QMainWindow *win = qWidget<QMainWindow> ();

  if ((! bar->isHidden ()) != visible)
    {
      QSize sz = bar->sizeHint ();
      QRect r = win->geometry ();

      if (visible)
        r.adjust (0, -sz.height (), 0, 0);
      else
        r.adjust (0, sz.height (), 0, 0);

      m_blockUpdates = true;
      set_geometry (r);
      bar->setVisible (visible);
      m_blockUpdates = false;

      updateBoundingBox (false);
    }
}

void
Figure::updateContainer (void)
{
  redraw ();
}

void
Figure::figureWindowShown ()
{
#if defined (HAVE_QSCREEN_DEVICEPIXELRATIO)
  QWindow *window = qWidget<QMainWindow> ()->windowHandle ();
  QScreen *screen = window->screen ();

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  figure::properties& fp = properties<figure> ();
  fp.set___device_pixel_ratio__ (screen->devicePixelRatio ());

  connect (window, &QWindow::screenChanged, this, &Figure::screenChanged);
#endif
}

void
Figure::screenChanged (QScreen *screen)
{
#if defined (HAVE_QSCREEN_DEVICEPIXELRATIO)
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  figure::properties& fp = properties<figure> ();
  double old_dpr = fp.get___device_pixel_ratio__ ();
  double new_dpr = screen->devicePixelRatio ();
  if (old_dpr != new_dpr)
    {
      fp.set___device_pixel_ratio__ (new_dpr);

      // For some obscure reason, changing the __device_pixel_ratio__ property
      // from the GUI thread does not necessarily trigger a redraw.  Force it.
      redraw ();
    }
#else
  octave_unused_parameter (screen);
#endif
}

void
Figure::enableMouseTracking (void)
{
  // Enable mouse tracking on every widgets
  m_container->setMouseTracking (true);
  m_container->canvas (m_handle)->qWidget ()->setMouseTracking (true);
  for (auto *w : m_container->findChildren<QWidget *> ())
    w->setMouseTracking (true);
}

OCTAVE_END_NAMESPACE(octave)
