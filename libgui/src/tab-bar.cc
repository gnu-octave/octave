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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "tab-bar.h"

namespace octave
{
  tab_bar::tab_bar (QWidget *p)
    : QTabBar (p), m_context_menu (new QMenu (this))
  { }

  tab_bar::~tab_bar (void)
  {
    delete m_context_menu;
  }

  // slots for tab navigation
  void tab_bar::switch_left_tab (void)
  {
    switch_tab (-1);
  }

  void tab_bar::switch_right_tab (void)
  {
    switch_tab (1);
  }

  void tab_bar::move_tab_left (void)
  {
#if defined (HAVE_QTABWIDGET_SETMOVABLE)
    switch_tab (-1, true);
#endif
  }

  void tab_bar::move_tab_right (void)
  {
#if defined (HAVE_QTABWIDGET_SETMOVABLE)
    switch_tab (1, true);
#endif
  }

  void tab_bar::switch_tab (int direction, bool movetab)
  {
    int tabs = count ();

    if (tabs < 2)
      return;

    int old_pos = currentIndex ();
    int new_pos = currentIndex () + direction;

    if (new_pos < 0 || new_pos >= tabs)
      new_pos = new_pos - direction*tabs;

    if (movetab)
      {
#if defined (HAVE_QTABWIDGET_SETMOVABLE)
        moveTab (old_pos, new_pos);
        setCurrentIndex (old_pos);
        setCurrentIndex (new_pos);
#endif
      }
    else
      setCurrentIndex (new_pos);
  }

  // Reimplement mouse event for filtering out the desired mouse clicks
  void tab_bar::mousePressEvent (QMouseEvent *me)
  {
    QPoint click_pos;
    int clicked_idx = -1;

    // detect the tab where the click occured
    for (int i = 0; i < count (); i++)
      {
        click_pos = mapToGlobal (me->pos ());
        if (tabRect (i).contains (mapFromGlobal (click_pos)))
          {
            clicked_idx = i;
            break;
          }
      }

    // If a tab was clicked
    if (clicked_idx >= 0)
      {
        int current_idx = currentIndex ();
        // detect the mouse click
        if ((me->type () == QEvent::MouseButtonDblClick &&
             me->button() == Qt::LeftButton) ||
            (me->type () != QEvent::MouseButtonDblClick &&
             me->button() == Qt::MidButton))
          {
            // Middle click or double click -> close the tab
            // Make the clicked tab the current one and close it
            setCurrentIndex (clicked_idx);
            emit close_current_tab_signal (true);
            // Was the closed tab before or after the previously current tab?
            // According to the result, use previous index or reduce it by one
            if (current_idx - clicked_idx > 0)
              setCurrentIndex (current_idx - 1);
            else if (current_idx - clicked_idx < 0)
              setCurrentIndex (current_idx);
          }
        else if (me->type () != QEvent::MouseButtonDblClick &&
                 me->button() == Qt::RightButton)
          {
            // Right click, show context menu
            setCurrentIndex (clicked_idx);
            if (! m_context_menu->exec (click_pos))
              {
                // No action selected, back to previous tab
                setCurrentIndex (current_idx);
              }
            else
              {
                // Was the possibly only closed tab before or after the
                // previously current tab? According to the result, use previous
                // index or reduce it by one. Also prevent using a too large
                // if other or all files were closed.
                int new_idx = count () - 1;
                if (new_idx > 0)
                  {
                    if (current_idx - clicked_idx > 0)
                      new_idx = current_idx - 1;
                    else if (current_idx - clicked_idx < 0)
                      new_idx = current_idx;
                  }
                if (new_idx >= 0)
                  setCurrentIndex (new_idx);
              }
          }
        else
          {
            // regular handling of the mouse event
            QTabBar::mousePressEvent (me);
          }
      }
    else
      {
        // regular handling of the mouse event
        QTabBar::mousePressEvent (me);
      }
  }
}
