////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

// This file implements a tab bar derived from QTabBar with a contextmenu
// and possibility to close a tab via double-left or middle mouse click.

#if ! defined (octave_tab_bar_h)
#define octave_tab_bar_h 1

#include <QMenu>
#include <QMouseEvent>
#include <QSize>
#include <QStyleOptionTab>
#include <QStylePainter>
#include <QTabBar>

OCTAVE_BEGIN_NAMESPACE(octave)

// Subclassed QTabBar for usable tab-bar, rotated tabs and
// reimplemented mouse event

class tab_bar : public QTabBar
{
  Q_OBJECT

public:

  tab_bar (QWidget *p);

  ~tab_bar (void) = default;

  void set_rotated (int rotated);
  QMenu * get_context_menu (void) { return m_context_menu; };
  QSize tabSizeHint (int idx) const;

signals:

  void close_current_tab_signal (bool);

public slots:

  void switch_left_tab (void);
  void switch_right_tab (void);
  void move_tab_left (void);
  void move_tab_right (void);
  void sort_tabs_alph (void);

private slots:

  void ctx_menu_activated (QAction *a);

protected:

  void paintEvent(QPaintEvent *e);
  void mousePressEvent(QMouseEvent *event);

private:

  void switch_tab (int direction, bool movetab = false);

  QMenu *m_context_menu;
  QList <QAction *> m_ctx_actions;
  int m_rotated;
};

OCTAVE_END_NAMESPACE(octave)

#endif
