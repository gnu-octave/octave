/*

Copyright (C) 2013 John W. Eaton
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

#if !defined (workspace_view_h)
#define workspace_view_h 1

#include <QItemDelegate>
#include <QTableView>
#include <QSemaphore>

#include "octave-dock-widget.h"
#include "workspace-model.h"

class variable_name_editor : public QItemDelegate
{
  Q_OBJECT

public:

  variable_name_editor (QObject *p = 0) : QItemDelegate (p) { }

  QWidget *createEditor (QWidget *p, const QStyleOptionViewItem& option,
                         const QModelIndex& index) const;

  void setEditorData (QWidget *editor, const QModelIndex& index) const;

  void setModelData (QWidget *editor, QAbstractItemModel *model,
                     const QModelIndex& index) const;

  void updateEditorGeometry (QWidget *editor,
                             const QStyleOptionViewItem& option,
                             const QModelIndex&) const;
};

class workspace_view : public octave_dock_widget
{
  Q_OBJECT

public:

  workspace_view (QWidget *parent = 0);

  ~workspace_view (void);

public:

  void setModel (workspace_model *model) { view->setModel (model); }

signals:

  /** signal that user had requested a command on a variable */
  void command_requested (const QString& cmd);

protected:

  void closeEvent (QCloseEvent *event);

protected slots:

  void contextmenu_requested (const QPoint& pos);

  // context menu slots
  void handle_contextmenu_disp (void);
  void handle_contextmenu_plot (void);
  void handle_contextmenu_stem (void);

private:

  void relay_contextmenu_command (const QString& cmdname);

  QTableView *view;

  variable_name_editor *var_name_editor;
};

#endif
