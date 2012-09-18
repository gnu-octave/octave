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

#ifndef HISTORYDOCKWIDGET_H
#define HISTORYDOCKWIDGET_H

#include <QDockWidget>
#include <QLineEdit>
#include <QListView>
#include <QSortFilterProxyModel>
#include <QStringListModel>
#include <QTimer>

class history_dock_widget : public QDockWidget
{
  Q_OBJECT
  public:
  history_dock_widget (QWidget *parent = 0);

public slots:
  void handle_visibility_changed (bool visible);
  void request_history_model_update ();
  void reset_model ();

signals:
  void information (const QString& message);

  /** Emitted, whenever the user double-clicked a command in the history. */
  void command_double_clicked (const QString& command);

  /** Custom signal that tells if a user has clicked away that dock widget. */
  void active_changed (bool active);
protected:
  void closeEvent (QCloseEvent *event);
private slots:
  void handle_double_click (QModelIndex modelIndex);

private:
  void construct ();
  QListView *_history_list_view;
  QLineEdit *_filter_line_edit;
  QSortFilterProxyModel _sort_filter_proxy_model;

  /** Stores the current history_model. */
  QStringListModel *_history_model;

  QTimer _update_history_model_timer;

  void update_history_callback (void);
};

#endif // HISTORYDOCKWIDGET_H
