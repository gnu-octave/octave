////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#if ! defined (octave_dw_main_window_h)
#define octave_dw_main_window_h 1

#include <QMainWindow>

#include "gui-settings.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class dw_main_window : public QMainWindow
{
  Q_OBJECT

public:

  dw_main_window (base_qobject& oct_qboj, QWidget *parent = nullptr);

  ~dw_main_window (void) = default;

  // No copying!

  dw_main_window (const dw_main_window&) = delete;

  dw_main_window& operator = (const dw_main_window&) = delete;

public slots:

  void notice_settings (const gui_settings *);

protected slots:

  virtual QMenu * createPopupMenu ();

  virtual bool event (QEvent *ev);

private slots:

  void request_close ();
  void request_close_all ();
  void request_close_other ();

  void request_switch_left ();
  void request_switch_right ();

private:

  void request_switch (int direction);

  QAction * add_action (QMenu *menu, const QIcon& icon, const QString& text,
                        const char *member, QWidget *receiver);

  base_qobject& m_octave_qobj;

  QList<QDockWidget *> m_dw_list;

  QAction *m_close_action;
  QAction *m_close_all_action;
  QAction *m_close_others_action;

  QAction *m_switch_left_action;
  QAction *m_switch_right_action;

  QList<QAction *> m_actions_list;
};

OCTAVE_END_NAMESPACE(octave)

#endif
