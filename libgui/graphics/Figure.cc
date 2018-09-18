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

#include <QAction>
#include <QActionEvent>
#include <QActionGroup>
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
#include "MouseModeActionGroup.h"
#include "QtHandlesUtils.h"

#include "file-ops.h"
#include "unwind-prot.h"
#include "utils.h"
#include "version.h"

#include "octave-qt-link.h"

#include "builtin-defun-decls.h"

namespace QtHandles
{

  DECLARE_GENERICEVENTNOTIFY_SENDER(MenuBar, QMenuBar);

  static bool
  hasUiControlChildren (const figure::properties& fp)
  {
    gh_manager::auto_lock lock;

    Matrix kids = fp.get_all_children ();

    for (int i = 0; i < kids.numel (); i++)
      {
        graphics_object go (gh_manager::get_object (kids(i)));

        if (go && (go.isa ("uicontrol") || go.isa ("uipanel")
                   || go.isa ("uibuttongroup")))
          return true;
      }

    return false;
  }

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

  Figure*
  Figure::create (const graphics_object& go)
  {
    return new Figure (go, new FigureWindow ());
  }

  Figure::Figure (const graphics_object& go, FigureWindow *win)
    : Object (go, win), m_blockUpdates (false), m_figureToolBar (nullptr),
      m_menuBar (nullptr), m_innerRect (), m_outerRect (),
      m_mouseModeGroup (nullptr)
  {
    m_container = new Container (win);
    win->setCentralWidget (m_container);

    figure::properties& fp = properties<figure> ();

    // Register for the signal that indicates when a window has moved
    // to a different screen
    connect (win, SIGNAL (figureWindowShown ()),
             this, SLOT (figureWindowShown ()));

    // Status bar
    m_statusBar = win->statusBar ();
    int boffset = 0;

    // Toolbar and menubar
    createFigureToolBarAndMenuBar ();
    int toffset = 0;

    if (fp.toolbar_is ("figure") ||
        (fp.toolbar_is ("auto") && fp.menubar_is ("figure") &&
         ! hasUiControlChildren (fp)))
      {
        toffset += m_figureToolBar->sizeHint ().height ();
        boffset += m_statusBar->sizeHint ().height ();
      }
    else
      {
        m_figureToolBar->hide ();
        m_statusBar->hide ();
      }

    m_innerRect = boundingBoxToRect (fp.get_boundingbox (true));
    m_outerRect = boundingBoxToRect (fp.get_boundingbox (false));

    win->setGeometry (m_innerRect.adjusted (0, -toffset, 0, boffset));

    // Enable mouse tracking unconditionally
    enableMouseTracking ();

    // When this constructor gets called all properties are already
    // set, even non default. We force "update" here to get things right.

    // Figure title
    update (figure::properties::ID_NUMBERTITLE);

    // Decide what keyboard events we listen to
    m_container->canvas (m_handle)->setEventMask (0);
    update (figure::properties::ID_KEYPRESSFCN);
    update (figure::properties::ID_KEYRELEASEFCN);

    // modal style
    update (figure::properties::ID_WINDOWSTYLE);

    // Visibility
    update (figure::properties::ID_VISIBLE);

    connect (this, SIGNAL (asyncUpdate (void)),
             this, SLOT (updateContainer (void)));

    win->addReceiver (this);
    m_container->addReceiver (this);
  }

  Figure::~Figure (void)
  { }

  static std::string
  mouse_mode_to_string (MouseMode mode)
  {
    switch (mode)
      {
      case NoMode:
        return "none";

      case RotateMode:
        return "rotate";

      case ZoomInMode:
        return "zoom in";

      case ZoomOutMode:
        return "zoom out";

      case PanMode:
        return "pan";

      case TextMode:
        return "text";

      case SelectMode:
        return "select";

      default:
        break;
      }

    return "none";
  }

  static MouseMode
  mouse_mode_from_string (const std::string& mode)
  {
    if (mode == "none")
      return NoMode;
    else if (mode == "rotate")
      return RotateMode;
    else if (mode == "zoom in")
      return ZoomInMode;
    else if (mode == "zoom out")
      return ZoomOutMode;
    else if (mode == "pan")
      return PanMode;
    else if (mode == "text")
      return TextMode;
    else if (mode == "select")
      return SelectMode;
    else
      return NoMode;
  }

  QString
  Figure::fileName (void)
  {
    gh_manager::auto_lock lock;

    const figure::properties& fp = properties<figure> ();

    std::string name = fp.get_filename ();

    return QString::fromStdString (name);
  }

  void
  Figure::setFileName (const QString& name)
  {
    gh_manager::auto_lock lock;

    figure::properties& fp = properties<figure> ();

    fp.set_filename (name.toStdString ());
  }

  MouseMode
  Figure::mouseMode (void)
  {
    gh_manager::auto_lock lock;

    const figure::properties& fp = properties<figure> ();

    std::string mode = fp.get___mouse_mode__ ();

    if (mode == "zoom")
      {
        octave_scalar_map zm = fp.get___zoom_mode__ ().scalar_map_value ();

        std::string direction = zm.getfield ("Direction").string_value ();

        mode += ' ' + direction;
      }

    return mouse_mode_from_string (mode);
  }

  void
  Figure::createFigureToolBarAndMenuBar (void)
  {
    QMainWindow *win = qWidget<QMainWindow> ();

    m_figureToolBar = win->addToolBar (tr ("Figure ToolBar"));
    m_figureToolBar->setMovable (false);
    m_figureToolBar->setFloatable (false);

    m_mouseModeGroup = new MouseModeActionGroup (win);
    connect (m_mouseModeGroup, SIGNAL (modeChanged (MouseMode)),
             SLOT (setMouseMode (MouseMode)));
    m_figureToolBar->addActions (m_mouseModeGroup->actions ());

    QAction *toggle_axes = m_figureToolBar->addAction (tr ("Axes"));
    connect (toggle_axes, SIGNAL (triggered (void)),
             this, SLOT (toggleAxes (void)));

    QAction *toggle_grid = m_figureToolBar->addAction (tr ("Grid"));
    connect (toggle_grid, SIGNAL (triggered (void)),
             this, SLOT (toggleGrid (void)));

    QAction *auto_axes = m_figureToolBar->addAction (tr ("Autoscale"));
    connect (auto_axes, SIGNAL (triggered (void)),
             this, SLOT (autoAxes (void)));

    m_menuBar = new MenuBar (win);
    win->setMenuBar (m_menuBar);
    m_menuBar->addReceiver (this);
  }

  void
  Figure::updateFigureToolBarAndMenuBar (void)
  {
    if (m_mouseModeGroup)
      {
        m_blockUpdates = true;
        m_mouseModeGroup->setMode (mouseMode ());
        m_blockUpdates = false;
      }
  }

  Container*
  Figure::innerContainer (void)
  {
    return m_container;
  }

  void
  Figure::redraw (void)
  {
    Canvas *canvas = m_container->canvas (m_handle);

    if (canvas)
      {
        canvas->redraw ();
        //canvas->setMouseMode (RotateMode);
      }

    foreach (QFrame *frame,
             qWidget<QWidget> ()->findChildren<QFrame*> ())
      {
        if (frame->objectName () == "UIPanel"
            || frame->objectName () == "UIButtonGroup")
          {
            Object *obj = Object::fromQObject (frame);

            if (obj)
              obj->slotRedraw ();
          }
      }

    updateFigureToolBarAndMenuBar ();
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
        gh_manager::process_events ();
        gh_manager::auto_lock lock;
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

    m_menuBar->removeReceiver (this);
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

          foreach (QToolBar *tb, win->findChildren<QToolBar*> ())
            if (! tb->isHidden ())
              toffset += tb->sizeHint ().height ();

          if (! m_menuBar->isHidden ())
            toffset += m_menuBar->sizeHint ().height ();

          if (! m_statusBar->isHidden ())
            boffset += m_statusBar->sizeHint ().height ();

          win->setGeometry (m_innerRect.adjusted (0, -toffset, 0, boffset));
        }
        break;

      case figure::properties::ID_NAME:
      case figure::properties::ID_NUMBERTITLE:
        win->setWindowTitle (Utils::fromStdString (fp.get_title ()));
        break;

      case figure::properties::ID_VISIBLE:
        if (fp.is_visible ())
          {
            QTimer::singleShot (0, win, SLOT (show ()));
            if (! fp.is___gl_window__ ())
              {
                gh_manager::auto_lock lock;
                fp.set ("__gl_window__", "on");
              }
          }
        else
          win->hide ();
        break;

      case figure::properties::ID_TOOLBAR:
        if (fp.toolbar_is ("none"))
          showFigureToolBar (false);
        else if (fp.toolbar_is ("figure"))
          showFigureToolBar (true);
        else // "auto"
          showFigureToolBar (! hasUiControlChildren (fp) &&
                             fp.menubar_is ("figure"));
        break;

      case figure::properties::ID_MENUBAR:
        if (fp.toolbar_is ("auto"))
          showFigureToolBar (fp.menubar_is ("figure"));
        break;

      case figure::properties::ID_KEYPRESSFCN:
        if (fp.get_keypressfcn ().isempty ())
          m_container->canvas (m_handle)->clearEventMask (Canvas::KeyPress);
        else
          m_container->canvas (m_handle)->addEventMask (Canvas::KeyPress);
        break;

      case figure::properties::ID_KEYRELEASEFCN:
        if (fp.get_keyreleasefcn ().isempty ())
          m_container->canvas (m_handle)->clearEventMask (Canvas::KeyRelease);
        else
          m_container->canvas (m_handle)->addEventMask (Canvas::KeyRelease);
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
        
      case figure::properties::ID___MOUSE_MODE__:
        m_container->canvas (m_handle)->setCursor (mouseMode ());
        break;

      default:
        break;
      }

    m_blockUpdates = false;
  }

  void
  Figure::showFigureToolBar (bool visible)
  {
    if ((! m_figureToolBar->isHidden ()) != visible)
      {
        int dy1 = m_figureToolBar->sizeHint ().height ();
        int dy2 = m_statusBar->sizeHint ().height ();
        QRect r = qWidget<QWidget> ()->geometry ();

        if (! visible)
          r.adjust (0, dy1, 0, -dy2);
        else
          r.adjust (0, -dy1, 0, dy2);

        m_blockUpdates = true;
        qWidget<QWidget> ()->setGeometry (r);
        m_figureToolBar->setVisible (visible);
        m_statusBar->setVisible (visible);
        m_blockUpdates = false;

        updateBoundingBox (false);
      }
  }

  void
  Figure::updateFigureHeight (int dh)
  {
    gh_manager::auto_lock lock;
    graphics_object go = object ();

    if (go.valid_object () && dh != 0)
      {
        QRect r = qWidget<QWidget> ()->geometry ();

        r.adjust (0, dh, 0, 0);

        m_blockUpdates = true;
        qWidget<QWidget> ()->setGeometry (r);
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

  QWidget*
  Figure::menu (void)
  {
    return qWidget<QMainWindow> ()->menuBar ();
  }

  struct UpdateBoundingBoxData
  {
    Matrix m_bbox;
    bool m_internal;
    graphics_handle m_handle;
    Figure *m_figure;
  };

  void
  Figure::updateBoundingBoxHelper (void *data)
  {
    gh_manager::auto_lock lock;

    UpdateBoundingBoxData *d = reinterpret_cast<UpdateBoundingBoxData *> (data);
    graphics_object go = gh_manager::get_object (d->m_handle);

    if (go.valid_object ())
      {
        figure::properties& fp = Utils::properties<figure> (go);

        fp.set_boundingbox (d->m_bbox, d->m_internal, false);
      }

    delete d;
  }

  void
  Figure::updateBoundingBox (bool internal, int flags)
  {
    QWidget *win = qWidget<QWidget> ();
    Matrix bb (1, 4);

    if (internal)
      {
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

    UpdateBoundingBoxData *d = new UpdateBoundingBoxData ();

    d->m_bbox = bb;
    d->m_internal = internal;
    d->m_handle = m_handle;
    d->m_figure = this;

    gh_manager::post_function (Figure::updateBoundingBoxHelper, d);
  }

  void
  Figure::close_figure_callback (void)
  {
    figure::properties& fp = properties<figure> ();
    octave_value fnum = fp.get___myhandle__ ().as_octave_value ();

    Ffeval (ovl ("close", fnum));
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
            graphics_object root = gh_manager::get_object (0);
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
                octave_link::post_event (this, &Figure::close_figure_callback);
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
            switch (xevent->type ())
              {
              case QEvent::Resize:
                updateBoundingBox (true, UpdateBoundingBoxSize);
                break;

              case QEvent::ChildAdded:
                if (dynamic_cast<QChildEvent *> (xevent)->child
                    ()->isWidgetType())
                  {
                    gh_manager::auto_lock lock;
                    update (figure::properties::ID_TOOLBAR);

                    enableMouseTracking ();
                  }
                break;

              case QEvent::ChildRemoved:
                if (dynamic_cast<QChildEvent *> (xevent)->child
                    ()->isWidgetType())
                  {
                    gh_manager::auto_lock lock;
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
  Figure::setMouseMode (MouseMode mode)
  {
    if (m_blockUpdates)
      return;

    gh_manager::auto_lock lock;

    figure::properties& fp = properties<figure> ();

    fp.set___mouse_mode__ (mouse_mode_to_string (mode));

    Canvas *canvas = m_container->canvas (m_handle);

    if (canvas)
      canvas->setCursor (mode);
  }

  void
  Figure::addCustomToolBar (QToolBar *bar, bool visible)
  {
    QMainWindow *win = qWidget<QMainWindow> ();

    if (! visible)
      win->addToolBar (bar);
    else
      {
        QSize sz = bar->sizeHint ();
        QRect r = win->geometry ();
        //qDebug () << "Figure::addCustomToolBar:" << r;

        r.adjust (0, -sz.height (), 0, 0);

        m_blockUpdates = true;
        win->setGeometry (r);
        win->addToolBarBreak ();
        win->addToolBar (bar);
        m_blockUpdates = false;

        //qDebug () << "Figure::addCustomToolBar:" << win->geometry ();
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
        win->setGeometry (r);
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
  Figure::toggleAxes (void)
  {
    Canvas *canvas = m_container->canvas (m_handle);

    if (canvas)
      canvas->toggleAxes (m_handle);
  }

  void
  Figure::toggleGrid (void)
  {
    Canvas *canvas = m_container->canvas (m_handle);

    if (canvas)
      canvas->toggleGrid (m_handle);
  }

  void
  Figure::autoAxes (void)
  {
    Canvas *canvas = m_container->canvas (m_handle);

    if (canvas)
      canvas->autoAxes (m_handle);
  }

  void
  Figure::figureWindowShown ()
  {
#if defined (HAVE_QSCREEN_DEVICEPIXELRATIO)
    QWindow* window = qWidget<QMainWindow> ()->windowHandle ();
    QScreen* screen = window->screen ();
    
    gh_manager::auto_lock lock;
    
    figure::properties& fp = properties<figure> ();
    fp.set___device_pixel_ratio__ (screen->devicePixelRatio ());

    connect (window, SIGNAL (screenChanged (QScreen*)),
             this, SLOT (screenChanged (QScreen*)));
#endif
  }

  void
  Figure::screenChanged (QScreen* screen)
  {
#if defined (HAVE_QSCREEN_DEVICEPIXELRATIO)
    gh_manager::auto_lock lock;
    
    figure::properties& fp = properties<figure> ();
    fp.set___device_pixel_ratio__ (screen->devicePixelRatio ());
#endif
  }

  void
  Figure::enableMouseTracking (void)
  {
    // Enable mouse tracking on every widgets
    m_container->setMouseTracking (true);
    m_container->canvas (m_handle)->qWidget ()->setMouseTracking (true);
    foreach (QWidget *w, m_container->findChildren<QWidget*> ())
      w->setMouseTracking (true);
  }

}
