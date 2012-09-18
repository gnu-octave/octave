/*

Copyright (C) 2011-2012 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QVBoxLayout>

#include "error.h"

#include "cmd-hist.h"

#include "history-dockwidget.h"
#include "octave-link.h"

history_dock_widget::history_dock_widget (QWidget * p)
  : QDockWidget (p)
{
  setObjectName ("HistoryDockWidget");
  construct ();
}

void
history_dock_widget::construct ()
{
  _history_model = new QStringListModel ();
  _sort_filter_proxy_model.setSourceModel (_history_model);
  _history_list_view = new QListView (this);
  _history_list_view->setModel (&_sort_filter_proxy_model);
  _history_list_view->setAlternatingRowColors (true);
  _history_list_view->setEditTriggers (QAbstractItemView::NoEditTriggers);
  _history_list_view->setStatusTip (tr ("Doubleclick a command to transfer it to the terminal."));
  _filter_line_edit = new QLineEdit (this);
  _filter_line_edit->setStatusTip (tr ("Enter text to filter the command history."));
  QVBoxLayout *vbox_layout = new QVBoxLayout ();

  setWindowTitle (tr ("Command History"));
  setWidget (new QWidget ());

  vbox_layout->addWidget (_history_list_view);
  vbox_layout->addWidget (_filter_line_edit);
  vbox_layout->setMargin (2);

  widget ()->setLayout (vbox_layout);

  connect (_filter_line_edit,
           SIGNAL (textEdited (QString)),
           &_sort_filter_proxy_model,
           SLOT (setFilterWildcard (QString)));

  connect (_history_list_view,
           SIGNAL (doubleClicked (QModelIndex)),
           this,
           SLOT (handle_double_click (QModelIndex)));

  connect (this,
           SIGNAL (visibilityChanged (bool)),
           this,
           SLOT (handle_visibility_changed (bool)));

  _update_history_model_timer.setInterval (200);
  _update_history_model_timer.setSingleShot (true);

  connect (&_update_history_model_timer,
           SIGNAL (timeout ()),
           this,
           SLOT (request_history_model_update ()));

  _update_history_model_timer.start ();

  setFocusProxy (_filter_line_edit);
}

void
history_dock_widget::handle_double_click (QModelIndex modelIndex)
{
  emit command_double_clicked (modelIndex.data().toString());
}

void
history_dock_widget::handle_visibility_changed (bool visible)
{
  if (visible)
    emit active_changed (true);
}

void
history_dock_widget::request_history_model_update ()
{
  octave_link::post_event (this, &history_dock_widget::update_history_callback);
}

void
history_dock_widget::reset_model ()
{
  _history_model->setStringList (QStringList ());
}

void
history_dock_widget::closeEvent (QCloseEvent *e)
{
  emit active_changed (false);
  QDockWidget::closeEvent (e);
}

void
history_dock_widget::update_history_callback (void)
{
  static bool scroll_window = false;

  // Determine the client's (our) history length and the one of the server.
  int clientHistoryLength = _history_model->rowCount ();
  int serverHistoryLength = command_history::length ();

  // If were behind the server, iterate through all new entries and add
  // them to our history.
  if (clientHistoryLength < serverHistoryLength)
    {
      int elts_to_add = serverHistoryLength - clientHistoryLength;

      _history_model->insertRows (clientHistoryLength, elts_to_add);

      for (int i = clientHistoryLength; i < serverHistoryLength; i++)
        {
          std::string entry = command_history::get_entry (i);

          _history_model->setData (_history_model->index (i),
                                   QString::fromStdString (entry));
        }

      // FIXME -- does this behavior make sense?  Calling
      // _history_list_view->scrollToBottom () here doesn't seem to
      // have any effect.  Instead, we need to request that action
      // and wait until the next event occurs in which no items
      // are added to the history list.

      scroll_window = true;
    }
  else if (scroll_window)
    {
      scroll_window = false;

      _history_list_view->scrollToBottom ();
    }

  // Post a new update event in a given time. This prevents flooding the
  // event queue.
  _update_history_model_timer.start ();
}
