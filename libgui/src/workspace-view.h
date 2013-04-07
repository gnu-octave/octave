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

#ifndef WORKSPACEVIEW_H
#define WORKSPACEVIEW_H

#include <QDockWidget>
#include <QTreeView>
#include <QSemaphore>

#include "workspace-model.h"

class workspace_view : public QDockWidget
{
  Q_OBJECT

public:

  workspace_view (QWidget * parent = 0);

  ~workspace_view ();

  void connect_visibility_changed (void);

public:

  void setModel (workspace_model *model) { view->setModel (model); }

public slots:

  void model_changed ();

  /** Slot when floating property changes */
  void top_level_changed (bool floating);

  void focus (void);

  void handle_visibility (bool visible);

signals:
  /** Custom signal that tells if a user has clicke away that dock widget. */
  void active_changed (bool active);
  /** signal that user had requested a command on a variable */
  void command_requested (const QString& cmd);

protected:
  void closeEvent (QCloseEvent *event);

protected slots:
  void collapse_requested (QModelIndex index);
  void expand_requested (QModelIndex index);
  void item_double_clicked (QModelIndex index);
  void contextmenu_requested (const QPoint& pos);
  // context menu slots
  void handle_contextmenu_disp ();
  void handle_contextmenu_plot ();
  void handle_contextmenu_stem ();
private:
  void relay_contextmenu_command (const QString& cmdname);

  QTreeView *view;

  struct
  {
    bool local;
    bool global;
    bool persistent;
  } _explicit_collapse;
};

#endif // WORKSPACEVIEW_H
