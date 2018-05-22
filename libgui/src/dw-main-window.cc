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
                    SLOT (request_close_file ()), this);

    m_close_all_action
      = add_action (nullptr,
                    resource_manager::icon ("window-close",false),
                    tr ("Close &All"),
                    SLOT (request_close_all_files ()), this);

    m_close_others_action
      = add_action (nullptr,
                    resource_manager::icon ("window-close",false),
                    tr ("Close &Other"),
                    SLOT (request_close_other_files ()), this);

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
  }


  // Slots for handling actions

  // Close current widget
  void dw_main_window::request_close_file ()
  {
    QList<QDockWidget *> list = findChildren<QDockWidget *>();

    for (int i = 0; i < list.length (); i++)
      {
        if (list.at (i)->hasFocus ())
          {
            list.at (i)->close ();
            if (i > 0)
              list.at (i-1)->setFocus ();
            break;
          }
      }
  }

  // Close other widgets
  void dw_main_window::request_close_other_files ()
  {
    QList<QDockWidget *> list = findChildren<QDockWidget *>();

    for (int i = list.length () - 1; i >= 0; i--)
      {
        if (! list.at (i)->hasFocus ())
          list.at (i)->close ();
      }
  }

  // Close all widgets
  void dw_main_window::request_close_all_files ()
  {
    QList<QDockWidget *> list = findChildren<QDockWidget *>();

    for (int i = list.length () - 1; i >= 0; i--)
      list.at (i)->close ();
  }

}


