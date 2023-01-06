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

#if ! defined (octave_octave_dock_widget_h)
#define octave_octave_dock_widget_h 1

#include <QDockWidget>
#include <QIcon>
#include <QMouseEvent>
#include <QToolButton>

#include "gui-settings.h"
#include "qt-interpreter-events.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class main_window;

// The few decoration items common to both main window and variable editor.

class label_dock_widget : public QDockWidget
{
  Q_OBJECT

public:

  label_dock_widget (QWidget *p, base_qobject& oct_qobj);

  ~label_dock_widget (void) = default;

  // set_title() uses the custom title bar while setWindowTitle() uses
  // the default title bar (with style sheets)
  void set_title (const QString&);

protected slots:

  //! Slots to handle copy & paste.
  //!@{
  virtual void copyClipboard (void) { }
  virtual void pasteClipboard (void) { }
  virtual void selectAll (void) { }
  //!@}

  //! Slot to handle undo.

  virtual void do_undo (void) { }

protected:

  base_qobject& m_octave_qobj;

  int m_icon_size;
  QWidget *m_title_widget;
  QToolButton *m_dock_button;
  QToolButton *m_close_button;
  QAction *m_dock_action;
  QAction *m_close_action;

  QAbstractButton *m_default_float_button;
  QAbstractButton *m_default_close_button;
};

class octave_dock_widget : public label_dock_widget
{
  Q_OBJECT

public:

  octave_dock_widget (const QString& obj_name, QWidget *p,
                      base_qobject& oct_qobj);

  ~octave_dock_widget (void) = default;

  void set_predecessor_widget (octave_dock_widget *prev_widget);

  void set_main_window (main_window *mw);

  void set_adopted (bool adopted = true) { m_adopted = adopted; }
  bool adopted (void) const { return m_adopted; }

signals:

  //! Custom signal that tells whether a user has clicked away that dock
  //! widget, i.e. the active dock widget has changed.

  void active_changed (bool active);

  void queue_make_window (bool widget_was_dragged);

  void queue_make_widget (void);

protected:

  virtual void closeEvent (QCloseEvent *e);

  QWidget * focusWidget (void);

  bool event (QEvent *event);

public slots:

  virtual void activate (void);

  virtual void handle_visibility (bool visible);

  virtual void notice_settings (const gui_settings *) { }

  virtual void save_settings (void);

  void init_window_menu_entry (void);

  void handle_settings (const gui_settings *);

  void handle_active_dock_changed (octave_dock_widget *, octave_dock_widget *);

  void moveEvent (QMoveEvent *event);

  void resizeEvent (QResizeEvent *event);

  void make_window (bool widget_was_dragged = false);

  void make_widget (bool not_used = false);

  void default_dock (bool not_used = false);

protected slots:

  virtual void toplevel_change (bool);

  //! Event filter for double clicks into the window decoration elements.

  bool eventFilter (QObject *obj, QEvent *e);

private slots:

  void change_visibility (bool);

private:

  void set_style (bool active);
  void set_focus_predecessor (void);
  void store_geometry (void);

  //! Stores the parent, since we are reparenting to 0.

  main_window *m_main_window;

  bool m_adopted;
  bool m_custom_style;
  bool m_focus_follows_mouse;
  int m_title_3d;
  QColor m_bg_color;
  QColor m_bg_color_active;
  QColor m_fg_color;
  QColor m_fg_color_active;
  QString m_icon_color;
  QString m_icon_color_active;
  octave_dock_widget *m_predecessor_widget;
  QRect m_recent_float_geom;
  QRect m_recent_dock_geom;
  bool m_waiting_for_mouse_button_release;
};

OCTAVE_END_NAMESPACE(octave)

#endif
