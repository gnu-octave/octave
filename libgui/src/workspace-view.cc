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

#include "workspace-view.h"
#include "resource-manager.h"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>

workspace_view::workspace_view (QWidget * parent) : QDockWidget
  (parent)
{
  setObjectName ("WorkspaceView");
  setWindowTitle (tr ("Workspace"));

  // Create a new workspace model.
  _workspace_model = new workspace_model ();

  _workspace_tree_view = new QTreeView (this);            // Create a new tree view.
  _workspace_tree_view->setHeaderHidden (false);          // Do not show header columns.
  _workspace_tree_view->setAlternatingRowColors (true);   // Activate alternating row colors.
  _workspace_tree_view->setAnimated (false);              // Deactivate animations because of strange glitches.
  _workspace_tree_view->setTextElideMode (Qt::ElideRight);// Elide text to the right side of the cells.
  _workspace_tree_view->setWordWrap (false);              // No wordwrapping in cells.
  _workspace_tree_view->setModel (_workspace_model);      // Assign model.

  // Set an empty widget, so we can assign a layout to it.
  setWidget (new QWidget (this));

  // Create a new layout and add widgets to it.
  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (_workspace_tree_view);
  layout->setMargin (2);

  // Set the empty widget to have our layout.
  widget ()->setLayout (layout);

  // Initialize collapse/expand state of the workspace subcategories.

  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  _explicit_collapse.local      = settings->value ("workspaceview/local_collapsed", false).toBool ();
  _explicit_collapse.global     = settings->value ("workspaceview/global_collapsed", false).toBool ();;
  _explicit_collapse.persistent = settings->value ("workspaceview/persistent_collapsed", false).toBool ();;
  _explicit_collapse.hidden     = settings->value ("workspaceview/hidden_collapsed", false).toBool ();;

  // Connect signals and slots.
  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT(handle_visibility_changed (bool)));

  connect (_workspace_model, SIGNAL (model_changed ()),
           this, SLOT (model_changed ()));

  connect (_workspace_tree_view, SIGNAL (collapsed (QModelIndex)),
           this, SLOT (collapse_requested (QModelIndex)));
  connect (_workspace_tree_view, SIGNAL (expanded (QModelIndex)),
           this, SLOT (expand_requested (QModelIndex)));

  connect (_workspace_tree_view, SIGNAL (doubleClicked (QModelIndex)),
           this, SLOT (item_double_clicked (QModelIndex)));

}

workspace_view::~workspace_view ()
{
  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  settings->setValue("workspaceview/local_collapsed", _explicit_collapse.local);
  settings->setValue("workspaceview/global_collapsed", _explicit_collapse.global);
  settings->setValue("workspaceview/persistent_collapsed", _explicit_collapse.persistent);
  settings->setValue("workspaceview/hidden_collapsed", _explicit_collapse.hidden);
}

void
workspace_view::handle_visibility_changed (bool visible)
{
  if (visible)
    emit active_changed (true);
}

void
workspace_view::model_changed ()
{
  // This code is very quirky and requires some explanation.
  // Usually, we should not deal with collapsing or expanding ourselves,
  // because the view itself determines (based on the model) whether it
  // is appropriate to collapse or expand items.
  //
  // Now, the logic requires that we update our model item by item, which
  // would make it work correctly, but this is extremely slow and scales
  // very bad (O(n^2)). That's why we throw away our model and rebuild it
  // completely from scratch (O(n)), which is why the view renders all
  // displayed data as invalid.
  //
  // In order to make collapsing/expanding work again, we need to set
  // flags ourselves here.

  QModelIndex local_model_index = _workspace_model->index (0, 0);
  QModelIndex global_model_index = _workspace_model->index (1, 0);
  QModelIndex persistent_model_index = _workspace_model->index (2, 0);
  QModelIndex hidden_model_index = _workspace_model->index (3, 0);

  if (_explicit_collapse.local) {
    _workspace_tree_view->collapse (local_model_index);
  } else {
    _workspace_tree_view->expand (local_model_index);
  }

  if (_explicit_collapse.global) {
    _workspace_tree_view->collapse (global_model_index);
  } else {
    _workspace_tree_view->expand (global_model_index);
  }

  if (_explicit_collapse.persistent) {
    _workspace_tree_view->collapse (persistent_model_index);
  } else {
    _workspace_tree_view->expand (persistent_model_index);
  }

  if (_explicit_collapse.hidden) {
    _workspace_tree_view->collapse (hidden_model_index);
  } else {
    _workspace_tree_view->expand (hidden_model_index);
  }
}

void
workspace_view::collapse_requested (QModelIndex index)
{
  // This code is very quirky and requires some explanation.
  // Usually, we should not deal with collapsing or expanding ourselves,
  // because the view itself determines (based on the model) whether it
  // is appropriate to collapse or expand items.
  //
  // Now, the logic requires that we update our model item by item, which
  // would make it work correctly, but this is extremely slow and scales
  // very bad (O(n^2)). That's why we throw away our model and rebuild it
  // completely from scratch (O(n)), which is why the view renders all
  // displayed data as invalid.
  //
  // In order to make collapsing/expanding work again, we need to set
  // flags ourselves here.
  QMap<int, QVariant> item_data
      = _workspace_model->itemData (index);

  if (item_data[0] == "Local")
    _explicit_collapse.local = true;
  if (item_data[0] == "Global")
    _explicit_collapse.global = true;
  if (item_data[0] == "Persistent")
    _explicit_collapse.persistent = true;
  if (item_data[0] == "Hidden")
    _explicit_collapse.hidden = true;
}

void
workspace_view::expand_requested (QModelIndex index)
{
  // This code is very quirky and requires some explanation.
  // Usually, we should not deal with collapsing or expanding ourselves,
  // because the view itself determines (based on the model) whether it
  // is appropriate to collapse or expand items.
  //
  // Now, the logic requires that we update our model item by item, which
  // would make it work correctly, but this is extremely slow and scales
  // very bad (O(n^2)). That's why we throw away our model and rebuild it
  // completely from scratch (O(n)), which is why the view renders all
  // displayed data as invalid.
  //
  // In order to make collapsing/expanding work again, we need to do set
  // flags ourselves here.
  QMap<int, QVariant> item_data
      = _workspace_model->itemData (index);

  if (item_data[0] == "Local")
    _explicit_collapse.local = false;
  if (item_data[0] == "Global")
    _explicit_collapse.global = false;
  if (item_data[0] == "Persistent")
    _explicit_collapse.persistent = false;
  if (item_data[0] == "Hidden")
    _explicit_collapse.hidden = false;
}

void
workspace_view::item_double_clicked (QModelIndex index)
{
  Q_UNUSED (index);
  // TODO: Implement opening a dialog that allows the user to change a variable in the workspace.
}

void
workspace_view::closeEvent (QCloseEvent *event)
{
  emit active_changed (false);
  QDockWidget::closeEvent (event);
}
