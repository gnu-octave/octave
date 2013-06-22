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
  setFeatures (QDockWidget::DockWidgetMovable); // not floatable or cloasable

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility_changed (bool)));

  connect (p, SIGNAL (settings_changed (const QSettings*)),
           this, SLOT (notice_settings (const QSettings*)));

  // the custom (extra) title bar of the widget
  _dock_action = new QAction
                   (QIcon (":/actions/icons/widget-undock.png"), "", this);
  _dock_action-> setToolTip (tr ("Undock widget"));
  connect (_dock_action, SIGNAL (triggered (bool)),
           this, SLOT (change_floating (bool)));
  QToolButton *dock_button = new QToolButton (this);
  dock_button->setDefaultAction (_dock_action);
  dock_button->setFocusPolicy(Qt::NoFocus);

  QAction *close_action = new QAction
                   (QIcon (":/actions/icons/widget-close.png"), "", this );
  close_action-> setToolTip (tr ("Hide widget"));
  connect (close_action, SIGNAL (triggered (bool)),
           this, SLOT (change_visibility (bool)));
  QToolButton *close_button = new QToolButton (this);
  close_button->setDefaultAction (close_action);
  close_button->setFocusPolicy(Qt::NoFocus);

  QHBoxLayout *h_layout = new QHBoxLayout ();
  h_layout->addStretch (100);
  h_layout->addWidget (dock_button);
  h_layout->addWidget (close_button);
  h_layout->setSpacing (0);
  h_layout->setContentsMargins (6,0,0,0);

  QWidget *title_widget = new QWidget ();
  title_widget->setLayout (h_layout);
  setTitleBarWidget (title_widget);

}

octave_dock_widget::~octave_dock_widget ()
{
  // save state of this dock-widget
  bool floating = false;
  bool visible;
  QString name = objectName ();
  QSettings *settings = resource_manager::get_settings ();

  settings->beginGroup ("DockWidgets");

  if (!parent ())
    { // widget is floating, save actual floating geometry
      floating = true;
      settings->setValue (name+"_floating_geometry", saveGeometry ());
    }
  else  // not floating save docked (normal) geometry
    settings->setValue (name, saveGeometry ());

  visible = isVisible ();
  settings->setValue (name+"Floating", floating);  // store floating state
  settings->setValue (name+"Visible", visible);    // store visibility

  settings->endGroup ();
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
  QHBoxLayout* h_layout =
      static_cast<QHBoxLayout *> (titleBarWidget ()->layout ());
  QLabel *label = new QLabel (title);
  h_layout->insertWidget (0,label);
  setWindowTitle (title);
}

// make the widget floating
void
octave_dock_widget::make_window (bool visible)
{
  QSettings *settings = resource_manager::get_settings ();

  // save the docking area for later redocking
  // FIXME: dockWidgetArea always returns 2
  settings->setValue ("DockWidgets/" + objectName () + "_dock_area",
                      _parent->dockWidgetArea (this));

  // remove parent and adjust the (un)dock icon
  setParent (0, Qt::Window);
  _dock_action->setIcon (QIcon (":/actions/icons/widget-dock.png"));
  _dock_action->setToolTip (tr ("Dock widget"));

  // restore the last geometry when floating
  restoreGeometry (settings->value
          ("DockWidgets/" + objectName ()+"_floating_geometry").toByteArray ());

  if (visible)
    show ();  // make visible if desired
}

// dock the widget
void
octave_dock_widget::make_widget (bool visible)
{
  QSettings *settings = resource_manager::get_settings ();

  // save last floating geometry
  settings->setValue ("DockWidgets/" + objectName () + "_floating_geometry",
                      saveGeometry ());

  // add widget to last saved docking area
  int area = settings->value ("DockWidgets/" + objectName () + "_dock_area",
                               Qt::TopDockWidgetArea).toInt ();
  _parent->addDockWidget (static_cast<Qt::DockWidgetArea> (area), this);

  // FIXME: restoreGeometry is ignored for docked widgets and its child widget
  // restoreGeometry (settings->value
  //        ("DockWidgets/" + objectName ()).toByteArray ());

  // adjust the (un)dock icon
  _dock_action->setIcon (QIcon (":/actions/icons/widget-undock.png"));
  _dock_action->setToolTip (tr ("Unock widget"));

  setVisible (visible);
}

// slot for (un)dock action
void
octave_dock_widget::change_floating (bool)
{
  if (parent())
    {
      make_window (true);
      focus ();
    }
  else
    make_widget (true);
}

// slot for hiding the widget
void
octave_dock_widget::change_visibility (bool)
{
  setVisible (false);
  emit active_changed (false);
}

