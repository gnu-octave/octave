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
#include "octave-link.h"
#include "workspace-model.h"

class workspace_view : public QDockWidget
{
  Q_OBJECT
public:
  workspace_view (QWidget * parent = 0);
  ~workspace_view ();

public slots:
  void handle_visibility_changed (bool visible);
  void model_changed ();

signals:
  /** Custom signal that tells if a user has clicke away that dock widget. */
  void active_changed (bool active);

protected:
  void closeEvent (QCloseEvent *event);

protected slots:
  void collapse_requested (QModelIndex index);
  void expand_requested (QModelIndex index);
  void item_double_clicked (QModelIndex index);

private:
  QTreeView *_workspace_tree_view;

  /** Stores the current workspace model. */
  workspace_model *_workspace_model;

  struct
  {
    bool local;
    bool global;
    bool persistent;
  } _explicit_collapse;
};

#endif // WORKSPACEVIEW_H
