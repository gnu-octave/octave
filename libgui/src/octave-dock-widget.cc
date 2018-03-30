/*

Copyright (C) 2012-2018 Richard Crozier
Copyright (C) 2013-2018 Torsten <ttl@justmail.de>

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
#include <QToolBar>
#include <QAction>
#include <QHBoxLayout>
#include <QLabel>
#include <QSettings>
#include <QStyle>

#include "resource-manager.h"
#include "octave-dock-widget.h"

namespace octave
{
  label_dock_widget::label_dock_widget (QWidget *p)
    : QDockWidget (p)
  {
    QStyle *st = style ();
    m_icon_size = 0.75*st->pixelMetric (QStyle::PM_SmallIconSize);

    // the custom (extra) title bar of the widget
    m_title_widget = new QWidget ();

    m_dock_action = new QAction
      (QIcon (":/actions/icons/widget-undock.png"), "", this);
    m_dock_action->setToolTip (tr ("Undock widget"));
    m_dock_button = new QToolButton (m_title_widget);
    m_dock_button->setDefaultAction (m_dock_action);
    m_dock_button->setFocusPolicy (Qt::NoFocus);
    m_dock_button->setIconSize (QSize (m_icon_size,m_icon_size));

    m_close_action = new QAction
      (QIcon (":/actions/icons/widget-close.png"), "", this);
    m_close_action->setToolTip (tr ("Close widget"));
    m_close_button = new QToolButton (m_title_widget);
    m_close_button->setDefaultAction (m_close_action);
    m_close_button->setFocusPolicy (Qt::NoFocus);
    m_close_button->setIconSize (QSize (m_icon_size,m_icon_size));

    QString css_button = QString ("QToolButton {background: transparent; border: 0px;}");
    m_dock_button->setStyleSheet (css_button);
    m_close_button->setStyleSheet (css_button);

    QHBoxLayout *h_layout = new QHBoxLayout ();
    h_layout->addStretch (100);
    h_layout->addWidget (m_dock_button);
    h_layout->addWidget (m_close_button);
    h_layout->setSpacing (0);
    h_layout->setContentsMargins (5,2,2,2);

    m_title_widget->setLayout (h_layout);
    setTitleBarWidget (m_title_widget);

    // copy & paste handling
    connect (p, SIGNAL (copyClipboard_signal ()),
             this, SLOT (copyClipboard ()));
    connect (p, SIGNAL (pasteClipboard_signal ()),
             this, SLOT (pasteClipboard ()));
    connect (p, SIGNAL (selectAll_signal ()),
             this, SLOT (selectAll ()));
    // undo handling
    connect (p, SIGNAL (undo_signal ()), this, SLOT (do_undo ()));
  }

  // set the title in the dockwidgets title bar
  void
  label_dock_widget::set_title (const QString& title)
  {
    QHBoxLayout *h_layout
      = static_cast<QHBoxLayout *> (titleBarWidget ()->layout ());
    QLabel *label = new QLabel (title, titleBarWidget ());
    label->setStyleSheet ("background: transparent;");
    h_layout->insertWidget (0,label);
    setWindowTitle (title);
  }


  octave_dock_widget::octave_dock_widget (QWidget *p)
    : label_dock_widget (p)
  {
    m_parent = static_cast<QMainWindow *> (p);     // store main window
    m_floating = false;
    m_predecessor_widget = nullptr;

    connect (this, SIGNAL (visibilityChanged (bool)),
             this, SLOT (handle_visibility_changed (bool)));

    connect (p, SIGNAL (settings_changed (const QSettings*)),
             this, SLOT (handle_settings (const QSettings*)));

    connect (p, SIGNAL (active_dock_changed (octave_dock_widget*,
                                             octave_dock_widget*)),
             this, SLOT (handle_active_dock_changed (octave_dock_widget*,
                                                     octave_dock_widget*)));

    setFeatures (QDockWidget::DockWidgetMovable); // not floatable or closeable

    connect (m_dock_action, SIGNAL (triggered (bool)),
             this, SLOT (change_floating (bool)));
    connect (m_close_action, SIGNAL (triggered (bool)),
             this, SLOT (change_visibility (bool)));

    m_close_action->setToolTip (tr ("Hide widget"));

    m_icon_color = "";
    m_title_3d = 50;

    installEventFilter (this);

    setFocusPolicy (Qt::StrongFocus);
  }

  // connect signal visibility changed to related slot (called from main-window)
  void
  octave_dock_widget::connect_visibility_changed (void)
  {
    connect (this, SIGNAL (visibilityChanged (bool)),
             this, SLOT (handle_visibility (bool)));
    emit active_changed (isVisible ());  // emit once for init of window menu
  }

  // make the widget floating
  void
  octave_dock_widget::make_window (void)
  {
    // the widget has to be reparented (parent = 0)

    QSettings *settings = resource_manager::get_settings ();

    // save the docking area and geometry for later redocking
    settings->setValue ("DockWidgets/" + objectName () + "_dock_area",
                        m_parent->dockWidgetArea (this));
    settings->setValue ("DockWidgets/" + objectName (), saveGeometry ());
    settings->sync ();

    // remove parent and adjust the (un)dock icon
    setTitleBarWidget (0);
    setParent (0, Qt::Window | Qt::CustomizeWindowHint | Qt::WindowTitleHint |
               Qt::WindowMinMaxButtonsHint | Qt::WindowCloseButtonHint);
    setTitleBarWidget (m_title_widget);
    setParent (0, Qt::Window | Qt::CustomizeWindowHint | Qt::WindowTitleHint |
               Qt::WindowMinMaxButtonsHint | Qt::WindowCloseButtonHint);

#if defined (Q_OS_UNIX)
    m_title_widget->setToolTip (
      tr ("Use <Alt> + <Left Mouse Button> for moving the window"));
#endif

    m_dock_action->setIcon (QIcon (":/actions/icons/widget-dock"
                                   + m_icon_color + ".png"));
    m_dock_action->setToolTip (tr ("Dock widget"));

    // restore the last geometry when floating
    setGeometry (settings->value ("DockWidgets/" + objectName ()
                                  + "_floating_geometry",
                                  QRect (50,100,480,480)).toRect ());

    m_floating = true;

    set_focus_predecessor ();  // set focus previously active widget if tabbed
  }

  // dock the widget
  void
  octave_dock_widget::make_widget (bool dock)
  {
    // Since floating widget has no parent, we have to read it

    QSettings *settings = resource_manager::get_settings ();

    // save last floating geometry if widget really was floating
    if (m_floating)
      settings->setValue ("DockWidgets/" + objectName () + "_floating_geometry",
                          geometry ());
    settings->sync ();

    if (dock)
      {
        settings->setValue ("MainWindow/windowState", m_parent->saveState ());
        m_parent->addDockWidget (Qt::TopDockWidgetArea, this);
        // recover old window states, hide and re-show new added widget
        m_parent->restoreState (settings->value ("MainWindow/windowState").toByteArray ());
        focus ();
        QApplication::setActiveWindow (this);
        m_title_widget->setToolTip ("");
      }
    else  // only reparent, no docking
      setParent (m_parent);

    // adjust the (un)dock icon
    m_dock_action->setIcon (QIcon (":/actions/icons/widget-undock"
                                   + m_icon_color + ".png"));
    m_dock_action->setToolTip (tr ("Undock widget"));

    m_floating = false;
  }

  // set the widget which previously had focus when tabified
  void
  octave_dock_widget::set_predecessor_widget (octave_dock_widget *prev_widget)
  {
    m_predecessor_widget = prev_widget;
  }

  // close event
  void
  octave_dock_widget::closeEvent (QCloseEvent *e)
  {
    emit active_changed (false);
    set_focus_predecessor ();
    QDockWidget::closeEvent (e);
  }

  // get focus widget
  QWidget *
  octave_dock_widget::focusWidget (void)
  {
    QWidget *w = QApplication::focusWidget ();
    if (w && w->focusProxy ()) w = w->focusProxy ();
    return w;
  }

  void
  octave_dock_widget::handle_settings (const QSettings *settings)
  {
    m_custom_style
      = settings->value ("DockWidgets/widget_title_custom_style",false).toBool ();

    m_title_3d
      = settings->value ("DockWidgets/widget_title_3d",50).toInt ();

    QColor default_var = QColor (0,0,0);
    m_fg_color = settings->value ("DockWidgets/title_fg_color",
                                  default_var).value<QColor> ();
    default_var = QColor (0,0,0);
    m_fg_color_active = settings->value ("DockWidgets/title_fg_color_active",
                                         default_var).value<QColor> ();

    default_var = QColor (255,255,255);
    m_bg_color = settings->value ("DockWidgets/title_bg_color",
                                  default_var).value<QColor> ();
    default_var = QColor (192,192,192);
    m_bg_color_active = settings->value ("DockWidgets/title_bg_color_active",
                                         default_var).value<QColor> ();

    QColor bcol (m_bg_color);
    QColor bcola (m_bg_color_active);

    if (! m_custom_style)
      {
        bcol = QWidget::palette ().color (m_title_widget->backgroundRole());
        bcola = bcol;
      }

    int r, g, b;
    bcol.getRgb (&r, &g, &b);
    if (r+g+b < 400)
      m_icon_color = "-light";
    else
      m_icon_color = "";

    bcola.getRgb (&r, &g, &b);
    if (r+g+b < 400)
      m_icon_color_active = "-light";
    else
      m_icon_color_active = "";

    notice_settings (settings);  // call individual handler

    set_style (false);
  }

  void
  octave_dock_widget::handle_active_dock_changed (octave_dock_widget *w_old,
                                                  octave_dock_widget *w_new)
  {
    if (m_custom_style && this == w_old)
      {
        set_style (false);
        update ();
      }

    if (m_custom_style && this == w_new)
      {
        set_style (true);
        update ();
      }
  }

  void
  octave_dock_widget::save_settings (void)
  {
    // save state of this dock-widget
    QString name = objectName ();
    QSettings *settings = resource_manager::get_settings ();

    if (! settings)
      return;

    settings->beginGroup ("DockWidgets");

    if (m_floating) // widget is floating (windows), save actual floating geometry
      settings->setValue (name+"_floating_geometry", geometry ());
    else           // not floating save docked (normal) geometry
      settings->setValue (name, saveGeometry ());

    settings->setValue (name+"Visible", isVisible ()); // store visibility
    settings->setValue (name+"Floating", m_floating);    // store visibility
    settings->setValue (name+"_minimized", isMinimized ()); // store minimized

    settings->endGroup ();
    settings->sync ();
  }

  bool octave_dock_widget::eventFilter (QObject *obj, QEvent *e)
  {
    if (e->type () == QEvent::NonClientAreaMouseButtonDblClick)
      {
        e->ignore (); // ignore double clicks into window decoration elements
        return true;
      }

    return QDockWidget::eventFilter (obj,e);
  }

  // slot for (un)dock action
  void
  octave_dock_widget::change_floating (bool)
  {
    if (m_floating)
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

  void
  octave_dock_widget::set_style (bool active)
  {
    QString css;
    QString css_button;
    QString dock_icon;

    QString icon_col = m_icon_color;

    if (m_floating)
      dock_icon = "widget-dock";
    else
      dock_icon = "widget-undock";

#if defined (Q_OS_MAC)
    QString alignment = "center";
#else
    QString alignment = "center left";
#endif
    if (m_custom_style)
      {

        QColor bg_col, fg_col;

        if (active)
          {
            bg_col = m_bg_color_active;
            fg_col = m_fg_color_active;
            icon_col = m_icon_color_active;
          }
        else
          {
            bg_col = m_bg_color;
            fg_col = m_fg_color;
            icon_col = m_icon_color;
          }

        QColor bg_col_top, bg_col_bottom;
        if (m_title_3d > 0)
          {
            bg_col_top = bg_col.lighter (100 + m_title_3d);
            bg_col_bottom = bg_col.darker (100 + m_title_3d);
          }
        else
          {
            bg_col_top = bg_col.darker (100 - m_title_3d);
            bg_col_bottom = bg_col.lighter (100 - m_title_3d);
          }

        QString background =
          QString ("background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,"
                   "            stop: 0 %1, stop: 0.60 %2, stop: 0.95 %2 stop: 1.0 %3);").
          arg (bg_col_top.name ()).
          arg (bg_col.name ()).
          arg (bg_col_bottom.name ());

        css = background + QString (" color: %1 ;").arg (fg_col.name ());
      }
    else
      {
        css = QString ("");
      }

    m_title_widget->setStyleSheet (css);
    css_button = QString ("QToolButton {background: transparent; border: 0px;}");
    m_dock_button->setStyleSheet (css_button);
    m_close_button->setStyleSheet (css_button);
    m_dock_action->setIcon (QIcon (":/actions/icons/" + dock_icon + icon_col +
                                   ".png"));
    m_close_action->setIcon (QIcon (":/actions/icons/widget-close" + icon_col +
                                    ".png"));
  }

  // set focus to previously active widget in tabbed widget stack
  void
  octave_dock_widget::set_focus_predecessor (void)
  {
    if (m_predecessor_widget)    // only != 0 if widget was tabbed
      m_predecessor_widget->focus ();

    m_predecessor_widget = nullptr;
    // FIXME: Until cset bda0c5b38bda, the wrong keys "Dockwidget/..." were used
    // here.  This had no effect in Qt4, but does in Qt5.  In the following, the
    // four incorrect keys are updated if still present in the settings files.
    // The keys are also used in the settings dialog, but
    // octave_dock_widget::handle_settings is already called at program start.
    // These tests can be removed in a future version of Octave (version 4.8).
    resource_manager::update_settings_key ("Dockwidgets/title_bg_color",
                                           "DockWidgets/title_bg_color");
    resource_manager::update_settings_key ("Dockwidgets/title_bg_color_active",
                                           "DockWidgets/title_bg_color_active");
    resource_manager::update_settings_key ("Dockwidgets/title_fg_color",
                                           "DockWidgets/title_fg_color");
    resource_manager::update_settings_key ("Dockwidgets/title_fg_color_active",
                                           "DockWidgets/title_fg_color_active");
  }
}
