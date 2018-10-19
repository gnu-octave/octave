/*

Copyright (C) 2018 Torsten <mttl@mailbox.org>

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

// This file implements a tab bar derived from QTabBar with a contextmenu
// and possibility to close a tab via double-left and middle mouse click.

#if ! defined (octave_tab_bar_h)
#define octave_tab_bar_h 1

#include <QMenu>
#include <QTabBar>
#include <QMouseEvent>

namespace octave
{
  // Subclassed QTabBar for usable tab-bar and reimplemented mouse event

  class tab_bar : public QTabBar
  {
    Q_OBJECT

  public:

    tab_bar (QWidget *p);

    ~tab_bar (void);

    QMenu *get_context_menu (void) { return m_context_menu; };

  signals:

    void close_current_tab_signal (bool);

  public slots:

    void switch_left_tab (void);
    void switch_right_tab (void);
    void move_tab_left (void);
    void move_tab_right (void);

  protected:

    void mousePressEvent(QMouseEvent *event);

  private:

    QMenu *m_context_menu;

    void switch_tab (int direction, bool movetab = false);
  };
}

#endif
