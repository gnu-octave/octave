/*

Copyright (C) 2013-2018 Torsten <mttl@mailbox.org>

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

/* This is the main window derived from QMainWindow for being used
   as the main window in dock widgets like the variable editor or
   the file editor
*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QMenu>
#include <QDockWidget>

#include "resource-manager.h"
#include "shortcut-manager.h"
#include "dw-main-window.h"

namespace octave
{

  dw_main_window::dw_main_window (QWidget *p)
    : QMainWindow (p)
  {
    // Adding the actions for closing the dock widgets
    m_close_action
      = add_action (nullptr,
                    resource_manager::icon ("window-close",false),
                    tr ("&Close"),
                    SLOT (request_close ()), this);

    m_close_all_action
      = add_action (nullptr,
                    resource_manager::icon ("window-close",false),
                    tr ("Close &All"),
                    SLOT (request_close_all ()), this);

    m_close_others_action
      = add_action (nullptr,
                    resource_manager::icon ("window-close",false),
                    tr ("Close &Other"),
                    SLOT (request_close_other ()), this);

    m_switch_left_action
      = add_action (nullptr, QIcon (), tr ("Switch to &Left Widget"),
                    SLOT (request_switch_left ()), this);

    m_switch_right_action
      = add_action (nullptr, QIcon (), tr ("Switch to &Right Widget"),
                    SLOT (request_switch_right ()), this);

    // The list of actions for floating widgets
    m_actions_list << m_close_action;
    m_actions_list << m_close_others_action;
    m_actions_list << m_close_all_action;
    m_actions_list << m_switch_left_action;
    m_actions_list << m_switch_right_action;

    notice_settings (resource_manager::get_settings ());
  }


  // Re-implementing the popup menu of the main window
  QMenu *dw_main_window::createPopupMenu ()
  {
    QList<QAction *> new_actions = QList<QAction *> ();
    new_actions.append (m_close_action);
    new_actions.append (m_close_others_action);
    new_actions.append (m_close_all_action);

    QMenu *menu = QMainWindow::createPopupMenu ();
    QList<QAction *> actions = menu->actions();

    if (actions.length () > 0)
      {
        QAction *sep = menu->insertSeparator (actions.at (0));
        menu->insertActions (sep, new_actions);
      }
    else
      menu->addActions (new_actions);

    return menu;
  }


  // Adding an action to the main window
  QAction * dw_main_window::add_action (QMenu *menu, const QIcon& icon,
                                        const QString& text, const char *member,
                                        QWidget *receiver)
  {
    QAction *a;
    QWidget *r = this;

    if (receiver != nullptr)
      r = receiver;

    if (menu)
      a = menu->addAction (icon, text, r, member);
    else
      {
        a = new QAction (icon, text, this);
        a->setEnabled (true);
        connect (a, SIGNAL (triggered ()), r, member);
      }

    addAction (a);  // important for shortcut context
    a->setShortcutContext (Qt::WidgetWithChildrenShortcut);

    return a;
  }

  // Update the settings
  void dw_main_window::notice_settings (const QSettings*)
  {
    shortcut_manager::set_shortcut (m_close_action, "editor_file:close");
    shortcut_manager::set_shortcut (m_close_all_action, "editor_file:close_all");
    shortcut_manager::set_shortcut (m_close_others_action, "editor_file:close_other");

    shortcut_manager::set_shortcut (m_switch_left_action, "editor_tabs:switch_left_tab");
    shortcut_manager::set_shortcut (m_switch_right_action, "editor_tabs:switch_right_tab");
  }


  // Slots for handling actions

  // Close current widget
  void dw_main_window::request_close ()
  {
    for (int i = 0; i < m_dw_list.length (); i++)
      {
        if (m_dw_list.at (i)->hasFocus ())
          {
            m_dw_list.at (i)->close ();
            if (i > 0)
              m_dw_list.at (i-1)->setFocus ();
            break;
          }
      }
  }

  // Close other widgets
  void dw_main_window::request_close_other ()
  {
    for (int i = m_dw_list.length () - 1; i >= 0; i--)
      {
        if (! m_dw_list.at (i)->hasFocus ())
          m_dw_list.at (i)->close ();
      }
  }

  // Close all widgets
  void dw_main_window::request_close_all ()
  {
    for (int i = m_dw_list.length () - 1; i >= 0; i--)
      m_dw_list.at (i)->close ();
  }

  // Switch to left widget
  void dw_main_window::request_switch_left ()
  {
    request_switch (-1);
  }

  // Switch to right widget
  void dw_main_window::request_switch_right ()
  {
    request_switch (1);
  }

  // Switch to left/right widget
  void dw_main_window::request_switch (int direction)
  {
    int active = -1, next;

    for (int i = m_dw_list.length () - 1; i >= 0; i--)
      {
        if (m_dw_list.at (i)->hasFocus ())
          {
            active = i;
            break;
          }
      }

    if (active == -1)
      return;

    if (direction == -1 && active == 0)
      next = m_dw_list.length () - 1;
    else if (direction == 1 && active == m_dw_list.length () - 1)
      next = 0;
    else
      next = active + direction;

    m_dw_list.at (next)->raise ();
    m_dw_list.at (next)->activateWindow ();
    m_dw_list.at (next)->setFocus ();
  }


  // Reimplemented Event
  bool dw_main_window::event (QEvent *ev)
  {
    if (ev->type () == QEvent::ChildAdded ||
        ev->type () == QEvent::ChildRemoved)
      {
        // Adding or Removing a child indicates that a dock widget was
        // created or removed.
        // In all cases, the list of dock widgets has to be updated.
        m_dw_list = findChildren<QDockWidget *>();
      }

    if (ev->type () == QEvent::StyleChange)
      {
        // This might indicate un- or re-docking a widget: Make sure
        // floating widget get a copy of our actions
        for (int i = m_dw_list.length () - 1; i >= 0; i--)
          {
            // First remove possibly existing actions
            for (int j = m_actions_list.length () - 1; j >0; j--)
              m_dw_list.at (i)->removeAction (m_actions_list.at (j));

            // Then add our actions for floating widgets
            if (m_dw_list.at (i)->isFloating ())
              m_dw_list.at (i)->addActions (m_actions_list);
          }
      }

    return QMainWindow::event (ev);
  }

}

