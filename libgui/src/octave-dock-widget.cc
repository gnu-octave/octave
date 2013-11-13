/*

Copyright (C) 2012-2013 Richard Crozier
Copyright (C) 2013 Torsten <ttl@justmail.de>

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
#include <QToolBar>
#include <QToolButton>
#include <QAction>
#include <QHBoxLayout>
#include <QLabel>
#include <QSettings>

#include "resource-manager.h"
#include "octave-dock-widget.h"


octave_dock_widget::octave_dock_widget (QWidget *p)
  : QDockWidget (p)
{

  _parent = static_cast<QMainWindow *> (p);     // store main window
  _floating = false;

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility_changed (bool)));

  connect (p, SIGNAL (settings_changed (const QSettings*)),
           this, SLOT (notice_settings (const QSettings*)));

#if defined (Q_OS_WIN32)
  // windows: add an extra title bar that persists when floating

  setFeatures (QDockWidget::DockWidgetMovable); // not floatable or closeable

  // the custom (extra) title bar of the widget
  _dock_action = new QAction
                   (QIcon (":/actions/icons/widget-undock.png"), "", this);
  _dock_action-> setToolTip (tr ("Undock widget"));
  connect (_dock_action, SIGNAL (triggered (bool)),
           this, SLOT (change_floating (bool)));
  QToolButton *dock_button = new QToolButton (this);
  dock_button->setDefaultAction (_dock_action);
  dock_button->setFocusPolicy (Qt::NoFocus);
  dock_button->setIconSize (QSize (12,12));

  QAction *close_action = new QAction
                   (QIcon (":/actions/icons/widget-close.png"), "", this );
  close_action-> setToolTip (tr ("Hide widget"));
  connect (close_action, SIGNAL (triggered (bool)),
           this, SLOT (change_visibility (bool)));
  QToolButton *close_button = new QToolButton (this);
  close_button->setDefaultAction (close_action);
  close_button->setFocusPolicy (Qt::NoFocus);
  close_button->setIconSize (QSize (12,12));

  QHBoxLayout *h_layout = new QHBoxLayout ();
  h_layout->addStretch (100);
  h_layout->addWidget (dock_button);
  h_layout->addWidget (close_button);
  h_layout->setSpacing (0);
  h_layout->setContentsMargins (6,0,0,0);

  QWidget *title_widget = new QWidget ();
  title_widget->setLayout (h_layout);
  setTitleBarWidget (title_widget);

#else

  // non windows: qt takes control of floating widgets
  setFeatures (QDockWidget::DockWidgetMovable |
               QDockWidget::DockWidgetClosable |
               QDockWidget::DockWidgetFloatable); // floatable and closeable

  connect (this, SIGNAL (topLevelChanged (bool)),
           this, SLOT (change_floating (bool)));

#endif

  // copy & paste handling
  connect (p, SIGNAL (copyClipboard_signal ()),
           this, SLOT (copyClipboard ()));
  connect (p, SIGNAL (pasteClipboard_signal ()),
           this, SLOT (pasteClipboard ()));
}

octave_dock_widget::~octave_dock_widget ()
{
  // save state of this dock-widget
  QString name = objectName ();
  QSettings *settings = resource_manager::get_settings ();

  settings->beginGroup ("DockWidgets");

#if defined (Q_OS_WIN32)
  if (_floating) // widget is floating (windows), save actual floating geometry
    settings->setValue (name+"_floating_geometry", geometry ());
  else           // not floating save docked (normal) geometry
#endif
    settings->setValue (name, saveGeometry ());

  settings->setValue (name+"Visible", isVisible ()); // store visibility
  settings->setValue (name+"Floating", _floating);    // store visibility

  settings->endGroup ();
  settings->sync ();
}

// connect signal visibility changed to related slot (called from main-window)
void
octave_dock_widget::connect_visibility_changed (void)
{
  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility (bool)));
  emit active_changed (isVisible ());  // emit once for init of window menu
}


// set the title in the dockwidgets title bar
void
octave_dock_widget::set_title (const QString& title)
{
#if defined (Q_OS_WIN32)
  QHBoxLayout* h_layout =
    static_cast<QHBoxLayout *> (titleBarWidget ()->layout ());
  QLabel *label = new QLabel (title);
  h_layout->insertWidget (0,label);
#endif
  setWindowTitle (title);
}

// make the widget floating
void
octave_dock_widget::make_window ()
{
#if defined (Q_OS_WIN32)

  // windows: the widget has to be reparented (parent = 0)

  QSettings *settings = resource_manager::get_settings ();

  // save the docking area and geometry for later redocking
  // FIXME: dockWidgetArea always returns 2
  settings->setValue ("DockWidgets/" + objectName () + "_dock_area",
                      _parent->dockWidgetArea (this));
  settings->setValue ("DockWidgets/" + objectName (), saveGeometry ());
  settings->sync ();

  // remove parent and adjust the (un)dock icon
  setParent (0, Qt::Window);
  _dock_action->setIcon (QIcon (":/actions/icons/widget-dock.png"));
  _dock_action->setToolTip (tr ("Dock widget"));

  // restore the last geometry( when floating
  setGeometry (settings->value ("DockWidgets/" + objectName ()
                       + "_floating_geometry",QRect(50,100,480,480)).toRect ());

#else

  // non windows: Just set the appripriate window flag
  setWindowFlags (Qt::Window);

#endif

  _floating = true;
}

// dock the widget
void
octave_dock_widget::make_widget (bool dock)
{
#if defined (Q_OS_WIN32)

  // windows: Since floating widget has no parent, we have to readd it

  QSettings *settings = resource_manager::get_settings ();

  // save last floating geometry if widget really was floating
  if (_floating)
    settings->setValue ("DockWidgets/" + objectName () + "_floating_geometry",
                        geometry ());
  settings->sync ();

  if (dock)
    {
      // add widget to last saved docking area (dock=true is default)
      int area = settings->value ("DockWidgets/" + objectName () + "_dock_area",
                                  Qt::TopDockWidgetArea).toInt ();
      _parent->addDockWidget (static_cast<Qt::DockWidgetArea> (area), this);

      // FIXME: restoreGeometry is ignored for docked widgets
      //        and its child widget
      restoreGeometry (settings->value
             ("DockWidgets/" + objectName ()).toByteArray ());
    }
  else  // only reparent, no docking
    setParent (_parent);

  // adjust the (un)dock icon
  _dock_action->setIcon (QIcon (":/actions/icons/widget-undock.png"));
  _dock_action->setToolTip (tr ("Undock widget"));

#else

  // non windows: just say we are a docked widget again
  setWindowFlags (Qt::Widget);

#endif

  _floating = false;
}

// slot for (un)dock action
void
octave_dock_widget::change_floating (bool)
{
  if (_floating)
    make_widget ();
  else
    {
      make_window ();
      focus ();
    }
}

// slot for hiding the widget
void
octave_dock_widget::change_visibility (bool)
{
  setVisible (false);
  emit active_changed (false);
}

// get focus widget
QWidget *
octave_dock_widget::focusWidget ()
{
  QWidget * w = QApplication::focusWidget ();
  if (w && w->focusProxy ()) w = w->focusProxy ();
  return w;
}
