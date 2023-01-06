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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "tab-bar.h"

OCTAVE_BEGIN_NAMESPACE(octave)

//
// Reimplemented QTabbar
//

tab_bar::tab_bar (QWidget *p)
: QTabBar (p), m_context_menu (new QMenu (this))
{ }

void tab_bar::set_rotated (int rotated)
{
  m_rotated = rotated;
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
  switch_tab (-1, true);
}

void tab_bar::move_tab_right (void)
{
  switch_tab (1, true);
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
      moveTab (old_pos, new_pos);
      setCurrentIndex (old_pos);
      setCurrentIndex (new_pos);
    }
  else
    setCurrentIndex (new_pos);
}

void tab_bar::sort_tabs_alph (void)
{
  QString current_title = tabText (currentIndex ());
  int tab_with_focus = 0;

  // Get all tab title and sort
  QStringList tab_texts;

  for (int i = 0; i < count (); i++)
    tab_texts.append (tabText (i));

  tab_texts.sort ();

  // Move tab into the order of the generated string list
  for (int title = 0; title < tab_texts.count (); title++)
    {
      // Target tab is same as place of title in QStringList.
      // Find index of next title in string list, leaving out the
      // tabs (or titles) that were already moved.
      for (int tab = title; tab < count (); tab++)
        {
          if (tabText (tab) == tab_texts.at (title))
            {
              // Index of next tile found, so move tab into next position
              moveTab (tab, title);

              if (tab_texts.at (title) == current_title)
                tab_with_focus = title;

              break;
            }
        }
    }

  setCurrentIndex (tab_with_focus);
}

// The following two functions are reimplemented for allowing rotated
// tabs and are based on this answer on stack overflow:
// https://stackoverflow.com/a/50579369

// Reimplemented size hint allowing rotated tabs
QSize tab_bar::tabSizeHint (int idx) const
{
  QSize s = QTabBar::tabSizeHint (idx);
  if (m_rotated)
    s.transpose();

  return s;
}

// Reimplemented paint event allowing rotated tabs
void tab_bar::paintEvent(QPaintEvent *e)
{
  // Just process the original event if not rotated
  if (! m_rotated)
    return QTabBar::paintEvent (e);

  // Process the event for rotated tabs
  QStylePainter painter (this);
  QStyleOptionTab opt;

  for (int idx = 0; idx < count(); idx++)
    {
      initStyleOption (&opt, idx);
      painter.drawControl (QStyle::CE_TabBarTabShape, opt);
      painter.save ();

      QSize s = opt.rect.size();
      s.transpose();
      QRect rect (QPoint (), s);
      rect.moveCenter (opt.rect.center ());
      opt.rect = rect;

      QPoint p = tabRect (idx).center ();
      painter.translate (p);
      painter.rotate (-m_rotated*90);
      painter.translate (-p);
      painter.drawControl (QStyle::CE_TabBarTabLabel, opt);
      painter.restore ();
    }
}

// Reimplement mouse event for filtering out the desired mouse clicks
void tab_bar::mousePressEvent (QMouseEvent *me)
{
  QPoint click_pos;
  int clicked_idx = -1;

  // detect the tab where the click occurred
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
      int current_count = count ();

      // detect the mouse click
      if ((me->type () == QEvent::MouseButtonDblClick
           && me->button() == Qt::LeftButton)
          || (me->type () != QEvent::MouseButtonDblClick
              && me->button() == Qt::MiddleButton))
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
      else if (me->type () != QEvent::MouseButtonDblClick
               && me->button() == Qt::RightButton)
        {
          // Right click, show context menu
          setCurrentIndex (clicked_idx);

          // Fill context menu with actions for selecting current tabs
          m_ctx_actions = m_context_menu->actions (); // Copy of basic actions
          QMenu ctx_menu;                             // The menu actually used
          connect (&ctx_menu, &QMenu::triggered,
                   this, &tab_bar::ctx_menu_activated);

          for (int i = count () - 1; i >= 0; i--)
            {
              // Prepend an action for each tab
              QAction *a = new QAction (tabIcon (i), tabText (i), &ctx_menu);
              m_ctx_actions.prepend (a);
            }
          // Add all actions to our menu
          ctx_menu.insertActions (nullptr, m_ctx_actions);

          if (! ctx_menu.exec (click_pos))
            {
              // No action selected, back to previous tab
              setCurrentIndex (current_idx);
            }
          else if (count () < current_count)
            {
              // A tab was closed:
              // Was the possibly only closed tab before or after the
              // previously current tab? According to the result, use previous
              // index or reduce it by one.  Also prevent using a too large
              // index if other or all files were closed.
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

// Slot if a menu entry in the context menu is activated
void tab_bar::ctx_menu_activated (QAction *a)
{
  // If the index of the activated action is in the range of
  // the current tabs, set the related current tab. The basic actions
  // are handled by the editor
  int i = m_ctx_actions.indexOf (a);
  if ((i > -1) && (i < count ()))
    setCurrentIndex (i);
}

OCTAVE_END_NAMESPACE(octave)
