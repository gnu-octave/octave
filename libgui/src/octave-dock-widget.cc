////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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
#include <QApplication>
#include <QHBoxLayout>
#include <QLabel>
#include <QScreen>
#include <QStyle>
#include <QToolBar>
#include <QMenuBar>
#include <QWindow>

#include "gui-preferences-dw.h"
#include "gui-preferences-global.h"
#include "gui-preferences-mw.h"
#include "gui-preferences-sc.h"
#include "gui-settings.h"
#include "gui-utils.h"
#include "main-window.h"
#include "octave-dock-widget.h"
#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

label_dock_widget::label_dock_widget (QWidget *p, base_qobject& oct_qobj)
: QDockWidget (p), m_octave_qobj (oct_qobj),
  m_default_float_button (nullptr), m_default_close_button (nullptr)
{
  QStyle *st = style ();
  m_icon_size = 0.75*st->pixelMetric (QStyle::PM_SmallIconSize);

  // keep track of the original buttons on the default title bar,
  // the button further left is considered "float"
  QList<QAbstractButton *> buttonlist = findChildren<QAbstractButton *> ();
  if (buttonlist.size () == 2)
    {
      if (buttonlist.at (0)->x () < buttonlist.at (1)->x ())
        {
          m_default_float_button = buttonlist.at (0);
          m_default_close_button = buttonlist.at (1);
        }
      else
        {
          m_default_float_button = buttonlist.at (1);
          m_default_close_button = buttonlist.at (0);
        }
    }

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  // the custom (extra) title bar of the widget
  m_title_widget = new QWidget ();

  m_dock_action = new QAction
    (rmgr.icon ("widget-undock", true), "", this);
  m_dock_action->setToolTip (tr ("Undock widget"));
  m_dock_button = new QToolButton (m_title_widget);
  m_dock_button->setDefaultAction (m_dock_action);
  m_dock_button->setFocusPolicy (Qt::NoFocus);
  m_dock_button->setIconSize (QSize (m_icon_size, m_icon_size));

  m_close_action = new QAction
    (rmgr.icon ("widget-close", true), "", this);
  m_close_action->setToolTip (tr ("Close widget"));
  m_close_button = new QToolButton (m_title_widget);
  m_close_button->setDefaultAction (m_close_action);
  m_close_button->setFocusPolicy (Qt::NoFocus);
  m_close_button->setIconSize (QSize (m_icon_size, m_icon_size));

  QString css_button = QString ("QToolButton {background: transparent; border: 0px;}");
  m_dock_button->setStyleSheet (css_button);
  m_close_button->setStyleSheet (css_button);

  QHBoxLayout *h_layout = new QHBoxLayout ();
  h_layout->addStretch (100);
  h_layout->addWidget (m_dock_button);
  h_layout->addWidget (m_close_button);
  h_layout->setSpacing (10);
  h_layout->setContentsMargins (5, 2, 2, 2);

  m_title_widget->setLayout (h_layout);

  if (p && (p->objectName () == gui_obj_name_main_window))
    {
      // Only connect the when a parent (main window) is given
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
}

// set the title in the dockwidgets title bar
void
label_dock_widget::set_title (const QString& title)
{
  QHBoxLayout *h_layout
    = static_cast<QHBoxLayout *> (m_title_widget->layout ());
  QLabel *label = new QLabel (title, m_title_widget);
  label->setStyleSheet ("background-color: transparent;");
  h_layout->insertWidget (0, label);
  setTitleBarWidget (m_title_widget);
  setWindowTitle (title);
}

static QString
qdockwidget_css (const QString& close_icon, const QString& close_tooltip,
                 const QString& float_icon, const QString& float_tooltip,
                 int icon_size, const QString& titlebar_foreground,
                 const QString& titlebar_background)
{
  return QString ("QDockWidget\n"
                  "{\n"
                  "%6"
                  "  border: none;\n"
                  "  titlebar-close-icon: url(%1);\n"
                  "  titlebar-normal-icon: url(%2);\n"
                  "}\n"
                  "\n"
                  "QDockWidget::close-button, QDockWidget::float-button\n"
                  "{\n"
                  "  border: none;\n"
                  "  icon-size: %3px;\n"
                  "}\n"
                  "\n"
                  "QAbstractButton#qt_dockwidget_closebutton\n"
                  "{\n"
                  "  qproperty-toolTip: \"%4\";\n"
                  "}\n"
                  "\n"
                  "QAbstractButton#qt_dockwidget_floatbutton\n"
                  "{\n"
                  "  qproperty-toolTip: \"%5\";\n"
                  "}\n"
                  "\n"
                  "QDockWidget::title {\n"
                  "  text-align: left;\n"
                  "%7"
                  "  padding-left: 1px;\n"
                  "}\n"
                  "\n"
                  "QDockWidget::close-button\n"
                  "{\n"
                  "  right: %8px;\n"
                  "  top: 3px;\n"
                  "}\n"
                  "\n"
                  "QDockWidget::float-button\n"
                  "{\n"
                  "  right: %9px;\n"
                  "  top: 3px;\n"
                  "}\n"
                  ).arg (close_icon).arg (float_icon).arg (icon_size)
    .arg (close_tooltip).arg (float_tooltip)
    .arg (titlebar_foreground).arg (titlebar_background)
    .arg ((icon_size*2)/3).arg((icon_size*7)/3);
}

octave_dock_widget::octave_dock_widget (const QString& obj_name, QWidget *p,
                                        base_qobject& oct_qobj)
  : label_dock_widget (p, oct_qobj), m_adopted (false),
    m_custom_style (false), m_focus_follows_mouse (false),
    m_recent_float_geom (), m_recent_dock_geom (),
    m_waiting_for_mouse_button_release (false)
{
  setObjectName (obj_name);

  // FIXME: Can we avoid the cast here?
  m_main_window = dynamic_cast<main_window *> (p);

  m_predecessor_widget = nullptr;

  connect (this, &octave_dock_widget::topLevelChanged,
           this, &octave_dock_widget::toplevel_change);
  connect (this, &octave_dock_widget::visibilityChanged,
           this, &octave_dock_widget::handle_visibility);

  if (m_default_float_button != nullptr)
    {
      disconnect (m_default_float_button, 0, 0, 0);
      connect (m_default_float_button, &QAbstractButton::clicked,
               this, &octave_dock_widget::make_window);
    }
  connect (this, &octave_dock_widget::queue_make_window,
           this, &octave_dock_widget::make_window, Qt::QueuedConnection);
  connect (this, &octave_dock_widget::queue_make_widget,
           this, [=] () { make_widget (); }, Qt::QueuedConnection);

  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();
  scmgr.set_shortcut (m_dock_action, sc_dock_widget_dock);
  m_dock_action->setShortcutContext (Qt::WidgetWithChildrenShortcut);
  addAction (m_dock_action);
  connect (m_dock_action, &QAction::triggered,
           this, &octave_dock_widget::make_window);

  scmgr.set_shortcut (m_close_action, sc_dock_widget_close);
  m_close_action->setShortcutContext (Qt::WidgetWithChildrenShortcut);
  addAction (m_close_action);
  connect (m_close_action, &QAction::triggered,
           this, &octave_dock_widget::change_visibility);

  m_close_action->setToolTip (tr ("Hide widget"));

  setStyleSheet (qdockwidget_css (
                                  global_icon_paths.at (ICON_THEME_OCTAVE) + "widget-close.png",
                                  QString ("Close widget"),
                                  global_icon_paths.at (ICON_THEME_OCTAVE) + "widget-undock.png",
                                  QString ("Undock widget"),
                                  m_icon_size,
                                  QString (""),
                                  QString ("")));

  if (widget ())
    widget ()->setToolTip (QString (""));

  m_icon_color = "";
  m_title_3d = 50;

  installEventFilter (this);

  setFocusPolicy (Qt::StrongFocus);

  setFeatures (QDockWidget::DockWidgetClosable
               | QDockWidget::DockWidgetMovable
               | QDockWidget::DockWidgetFloatable);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  handle_settings (rmgr.get_settings ());
}

void
octave_dock_widget::init_window_menu_entry (void)
{
  emit active_changed (isVisible ());  // emit once for init of window menu
}

// make the widget floating
void
octave_dock_widget::make_window (bool widget_was_dragged)
{
  bool vis = isVisible ();

  // prevent follow-up calls by clearing state variable
  m_waiting_for_mouse_button_release = false;

  set_focus_predecessor ();  // set focus previously active widget if tabbed

  // Before unparenting, get current geometry for restoring if dragged
  QRect geom = geometry ();

  // the widget has to be reparented (parent = 0), preferably
  // from a non-toplevel widget otherwise may not have full
  // decorations, e.g., no taskbar icon and always in front
  if (isFloating ())
    setFloating (false);

  if (m_main_window)
    {
      // Before making it a separate (no more parent) floating widget,
      // remove the dock widget from the main window. This ensures
      // that tabbed widgets keep their focus when it is re-docked
      // later
      m_main_window->removeDockWidget (this);
    }

  setParent (0, Qt::CustomizeWindowHint | Qt::WindowTitleHint |
             Qt::WindowMinMaxButtonsHint | Qt::WindowCloseButtonHint | Qt::Window);

  // restore the last geometry when floating only if we have not dragged
  // the window outside the main window
  if (! widget_was_dragged)
    geom = m_recent_float_geom.isNull () ? QRect (50,100,480,480)
      : m_recent_float_geom;
  setGeometry (geom);

  // adjust the (un)dock action
  disconnect (m_dock_action, 0, this, 0);
  connect (m_dock_action, &QAction::triggered,
           this, &octave_dock_widget::make_widget);

  // adjust the (un)dock icon
  if (titleBarWidget ())
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      m_dock_action->setIcon (rmgr.icon ("widget-dock" + m_icon_color, true));
      m_dock_action->setToolTip (tr ("Dock widget"));
    }
  else
    {
      disconnect (m_default_float_button, 0, this, 0);
      connect (m_default_float_button, &QAbstractButton::clicked,
               this, &octave_dock_widget::make_widget);
    }

  raise ();
  activateWindow ();

  if (vis)
    {
      show ();
      setFocus ();
      set_style (true);
    }

  emit topLevelChanged (true);  // Be sure signal is emitted
}

// dock the widget
void
octave_dock_widget::make_widget (bool)
{
  bool vis = isVisible ();

  // Since floating widget has no parent, we have to read it
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (m_main_window)
    {
      settings->setValue (mw_state.key, m_main_window->saveState ());

      // Stay window, otherwise will bounce back to window by default
      // because there is no layout information for this widget in the
      // saved settings.
      setParent (m_main_window, Qt::Window);
      m_main_window->addDockWidget (Qt::BottomDockWidgetArea, this);
      m_adopted = false;
      // recover old window states, hide and re-show new added widget
      m_main_window->restoreState (settings->value (mw_state.key).toByteArray ());
      setFloating (false);
      // restore size using setGeometry instead of restoreGeometry
      // following this post:
      // https://forum.qt.io/topic/79326/qdockwidget-restoregeometry-not-working-correctly-when-qmainwindow-is-maximized/5
      setGeometry (m_recent_dock_geom);
    }

  // adjust the (un)dock icon
  disconnect (m_dock_action, 0, this, 0);
  connect (m_dock_action, &QAction::triggered,
           this, &octave_dock_widget::make_window);
  if (titleBarWidget ())
    {
      m_dock_action->setIcon (rmgr.icon ("widget-undock" + m_icon_color, true));
      m_dock_action->setToolTip (tr ("Undock widget"));
    }
  else
    {
      disconnect (m_default_float_button, 0, this, 0);
      connect (m_default_float_button, &QAbstractButton::clicked,
               this, &octave_dock_widget::make_window);
    }

  raise ();
  QApplication::setActiveWindow (this);

  if (vis)
    {
      show ();
      setFocus ();
      set_style (true);
    }
}

// dock the widget
void
octave_dock_widget::default_dock (bool)
{
  setFloating (false);
}

// set the widget which previously had focus when tabified
void
octave_dock_widget::set_predecessor_widget (octave_dock_widget *prev_widget)
{
  m_predecessor_widget = prev_widget;
}

void
octave_dock_widget::set_main_window (main_window *mw)
{
  m_main_window = mw;

  if (m_main_window)
    {
      connect (m_main_window, &main_window::copyClipboard_signal,
               this, &octave_dock_widget::copyClipboard);

      connect (m_main_window, &main_window::pasteClipboard_signal,
               this, &octave_dock_widget::pasteClipboard);

      connect (m_main_window, &main_window::selectAll_signal,
               this, &octave_dock_widget::selectAll);

      connect (m_main_window, &main_window::undo_signal,
               this, &octave_dock_widget::do_undo);
    }
}

// close event
void
octave_dock_widget::closeEvent (QCloseEvent *e)
{
  emit active_changed (false);
  set_focus_predecessor ();
  save_settings ();

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

bool
octave_dock_widget::event (QEvent *event)
{
  // low-level check of whether docked-widget became a window via
  // double-click or via drag-and-drop
  if ( (event->type () == QEvent::MouseButtonDblClick && ! isFloating ())
       || (event->type () == QEvent::ActivationChange
           && m_waiting_for_mouse_button_release))
    {
      bool retval = QDockWidget::event (event);
      if (isFloating () && parent () != 0)
        {
          m_waiting_for_mouse_button_release = false;
          emit queue_make_window (event->type () != QEvent::MouseButtonDblClick);
        }
      return retval;
    }

  return QDockWidget::event (event);
}

void
octave_dock_widget::handle_settings (const gui_settings *settings)
{
  if (! settings)
    return;

  m_focus_follows_mouse = settings->value (dw_focus_follows_mouse).toBool ();

  m_custom_style
    = settings->value (dw_title_custom_style).toBool ();

  m_title_3d = settings->value (dw_title_3d.key, dw_title_3d.def).toInt ();

  m_fg_color
    = settings->value (dw_title_fg_color).value<QColor> ();

  m_fg_color_active
    = settings->value (dw_title_fg_color_active).value<QColor> ();

  m_bg_color = settings->value (dw_title_bg_color).value<QColor> ();

  m_bg_color_active
    = settings->value (dw_title_bg_color_active).value<QColor> ();

  QColor bcol (m_bg_color);
  QColor bcola (m_bg_color_active);

  if (! m_custom_style)
    {
      bcol = QWidget::palette ().color (m_title_widget->backgroundRole ());
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

  int x, y, w, h;
  QGuiApplication::primaryScreen ()->availableGeometry ().getRect (&x, &y, &w, &h);
  QRect default_floating_size = QRect (x+16, y+32, w/3, h/2);

  QRect default_dock_size;
  if (m_main_window)
    {
      // We have a main window, dock size depends on size of main window
      m_main_window->geometry ().getRect (&x, &y, &w, &h);
      default_dock_size = QRect (x+16, y+32, w/3, h/3);
    }
  else
    {
      // No main window, default dock size should never be used
      default_dock_size = QRect (0, 0, w/10, h/10);
    }

  m_recent_float_geom
    = settings->value (dw_float_geometry.key.arg (objectName ()),
                       default_floating_size).toRect ();

  adjust_to_screen (m_recent_float_geom, default_floating_size);

  // The following is required for ensure smooth transition from old
  // saveGeomety to new QRect setting (see comment for restoring size
  // of docked widgets)
  QVariant dock_geom
    = settings->value (dw_dock_geometry.key.arg (objectName ()),
                       default_dock_size);
  if (dock_geom.canConvert (QMetaType::QRect))
    m_recent_dock_geom = dock_geom.toRect ();
  else
    m_recent_dock_geom = dw_dock_geometry.def.toRect ();

  notice_settings (settings);  // call individual handler

  set_style (false);

  // Compacter design
  QToolBar *toolbar = findChild <QToolBar *> ();
  if (toolbar)
    toolbar->setStyleSheet (toolbar->styleSheet () + global_toolbar_style);

  QMenuBar *menubar = findChild <QMenuBar *> ();
  if (menubar)
    menubar->setStyleSheet (menubar->styleSheet () + global_menubar_style);

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
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (! settings)
    return;

  store_geometry ();

  // conditional needed?
  if (! m_recent_float_geom.isNull ())
    settings->setValue (dw_float_geometry.key.arg (name), m_recent_float_geom);

  if (! m_recent_dock_geom.isEmpty ())
    settings->setValue (dw_dock_geometry.key.arg (name), m_recent_dock_geom);
  settings->setValue (dw_is_visible.key.arg (name), isVisible ()); // store visibility
  settings->setValue (dw_is_floating.key.arg (name), isFloating ()); // store floating
  settings->setValue (dw_is_minimized.key.arg (name), isMinimized ()); // store minimized

  settings->sync ();
}

bool octave_dock_widget::eventFilter (QObject *obj, QEvent *e)
{
  // Ignore double clicks into window decoration elements
  if (e->type () == QEvent::NonClientAreaMouseButtonDblClick)
    {
      e->ignore ();
      return true;
    }

  // Detect mouse enter events if "focus follows mouse" is desired
  // for widgets docked to the main window (non floating) and activate
  // the widget currently under the mouse
  if (m_focus_follows_mouse && ! isFloating () && (e->type () == QEvent::Enter))
    setFocus ();

  return QDockWidget::eventFilter (obj, e);
}

void
octave_dock_widget::store_geometry (void)
{
  if (isFloating ())
    {
      if (! parent ())
        m_recent_float_geom = geometry ();
    }
  else
    {
      m_recent_dock_geom = geometry ();
    }
}

void
octave_dock_widget::moveEvent (QMoveEvent *event)
{
  store_geometry ();

  QDockWidget::moveEvent (event);
}

void
octave_dock_widget::resizeEvent (QResizeEvent *event)
{
  store_geometry ();

  QDockWidget::resizeEvent (event);
}

// slot for hiding the widget
void
octave_dock_widget::change_visibility (bool)
{
  setVisible (false);
  emit active_changed (false);
}

void octave_dock_widget::activate (void)
{
  if (! isVisible ())
    setVisible (true);

  setFocus ();
  activateWindow ();
  raise ();
}

void octave_dock_widget::handle_visibility (bool visible)
{
  if (visible)
    {
      emit active_changed (true);
      if (! isFloating ())
        setFocus ();
    }
}

void
octave_dock_widget::toplevel_change (bool toplevel)
{
  QObject *dockobj;
  const char *docksig;

  if (titleBarWidget ())
    {
      dockobj = m_dock_action;
      docksig = SIGNAL (triggered (bool));
    }
  else
    {
      dockobj = m_default_float_button;
      docksig = SIGNAL (clicked (bool));
    }

  if (toplevel)
    {
      // This is a fallback in case the attempt to create a floated
      // top-level window fails and the QDockWidget remains a child
      // of the QMainWindow.
      connect (dockobj, docksig, this, SLOT (default_dock (bool)));

      // Could be dragging window, so must wait until there is a
      // change in focus.
      if (parent () != 0)
        m_waiting_for_mouse_button_release = true;
    }
  else
    {
      // If a drag-and-drop within the QMainWindow occurred, want to remain a widget.
      m_waiting_for_mouse_button_release = false;

      // Making into a widget immediately will mangle the double-click
      // status and cause problems on followup button clicks.
      if (parent () == 0)
        emit queue_make_widget ();
    }
}

void
octave_dock_widget::set_style (bool active)
{
  QString css_foreground;
  QString css_background;
  QString css_button;
  QString dock_icon;

  QString icon_col = m_icon_color;

  QString close_tooltip = "Close widget";
  QString dock_tooltip;

  if (isFloating ())
    {
      dock_icon = "widget-dock";
      dock_tooltip = "Dock widget";
    }
  else
    {
      dock_icon = "widget-undock";
      dock_tooltip = "Undock widget";
    }

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

      css_foreground = QString ("  color: %1;\n").arg (fg_col.name ());

      css_background =
        QString ("  background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,"
                 " stop: 0 %1, stop: 0.60 %2, stop: 0.95 %2 stop: 1.0 %3);\n").
        arg (bg_col_top.name ()).
        arg (bg_col.name ()).
        arg (bg_col_bottom.name ());
    }
  else
    {
      css_foreground = QString ("");
      css_background = QString ("");
    }

  QString full_dock_icon = dock_icon + icon_col;
  QString full_close_icon = "widget-close" + icon_col;
  if (titleBarWidget ())
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      titleBarWidget ()->setStyleSheet (css_foreground + css_background);
      css_button = QString ("QToolButton {background: transparent; border: 0px;}");
      m_dock_button->setStyleSheet (css_button);
      m_close_button->setStyleSheet (css_button);
      m_dock_action->setIcon (rmgr.icon (full_dock_icon, true));
      m_close_action->setIcon (rmgr.icon (full_close_icon, true));
    }
  else
    {
      setStyleSheet (qdockwidget_css (global_icon_paths.at (ICON_THEME_OCTAVE) + full_close_icon + ".png",
                                      close_tooltip,
                                      global_icon_paths.at (ICON_THEME_OCTAVE) + full_dock_icon + ".png",
                                      dock_tooltip,
                                      m_icon_size,
                                      css_foreground,
                                      css_background));
    }
}

// set focus to previously active widget in tabbed widget stack
void
octave_dock_widget::set_focus_predecessor (void)
{
  // only != 0 if widget was tabbed
  if (m_predecessor_widget && m_predecessor_widget->isVisible ())
    m_predecessor_widget->setFocus ();

  m_predecessor_widget = nullptr;

  // FIXME: Until cset bda0c5b38bda, the wrong keys "Dockwidget/..." were used
  // here.  This had no effect in Qt4, but does in Qt5.  In the following, the
  // four incorrect keys are updated if still present in the settings files.
  // The keys are also used in the settings dialog, but
  // octave_dock_widget::handle_settings is already called at program start.
  // These tests can be removed in a future version of Octave (version 6).

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  rmgr.update_settings_key ("Dockwidgets/title_bg_color",
                            dw_title_bg_color.key);

  rmgr.update_settings_key ("Dockwidgets/title_bg_color_active",
                            dw_title_bg_color_active.key);

  rmgr.update_settings_key ("Dockwidgets/title_fg_color",
                            dw_title_fg_color.key);

  rmgr.update_settings_key ("Dockwidgets/title_fg_color_active",
                            dw_title_fg_color_active.key);
}

OCTAVE_END_NAMESPACE(octave)
