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

#include "workspace-view.h"
#include "resource-manager.h"
#include <QHeaderView>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>

workspace_view::workspace_view (QWidget *p)
  : QDockWidget (p)
{
  setObjectName ("WorkspaceView");
  setWindowIcon (QIcon(":/actions/icons/logo.png"));
  setWindowTitle (tr ("Workspace"));
  setStatusTip (tr ("View the variables in the active workspace."));

  view = new QTreeView (this);            // Create a new tree view.
  view->setHeaderHidden (false);          // Do not show header columns.
  view->setAlternatingRowColors (true);   // Activate alternating row colors.
  view->setAnimated (false);              // Deactivate animations because of strange glitches.
  view->setTextElideMode (Qt::ElideRight);// Elide text to the right side of the cells.
  view->setWordWrap (false);              // No wordwrapping in cells.

  // Set an empty widget, so we can assign a layout to it.
  setWidget (new QWidget (this));

  // Create a new layout and add widgets to it.
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->addWidget (view);
  vbox_layout->setMargin (2);

  // Set the empty widget to have our layout.
  widget ()->setLayout (vbox_layout);

  // Initialize collapse/expand state of the workspace subcategories.

  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  _explicit_collapse.local      = settings->value ("workspaceview/local_collapsed", false).toBool ();
  _explicit_collapse.global     = settings->value ("workspaceview/global_collapsed", false).toBool ();;
  _explicit_collapse.persistent = settings->value ("workspaceview/persistent_collapsed", false).toBool ();;

  // Initialize column order and width of the workspace
  
  view->header ()->restoreState (settings->value("workspaceview/column_state").toByteArray ());

  // Connect signals and slots.
  connect (view, SIGNAL (collapsed (QModelIndex)),
           this, SLOT (collapse_requested (QModelIndex)));

  connect (view, SIGNAL (expanded (QModelIndex)),
           this, SLOT (expand_requested (QModelIndex)));

  connect (view, SIGNAL (doubleClicked (QModelIndex)),
           this, SLOT (item_double_clicked (QModelIndex)));

  // topLevelChanged is emitted when floating property changes (floating = true)
  connect (this, SIGNAL (topLevelChanged(bool)), this, SLOT(top_level_changed(bool)));

}

workspace_view::~workspace_view ()
{
  QSettings *settings = resource_manager::get_settings ();
  settings->setValue("workspaceview/local_collapsed", _explicit_collapse.local);
  settings->setValue("workspaceview/global_collapsed", _explicit_collapse.global);
  settings->setValue("workspaceview/persistent_collapsed", _explicit_collapse.persistent);
  settings->setValue("workspaceview/column_state", view->header ()->saveState ());
  settings->sync ();
}

void
workspace_view::connect_visibility_changed (void)
{
  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility (bool)));
}

void
workspace_view::model_changed ()
{
  QAbstractItemModel *m = view->model ();

  dynamic_cast<workspace_model *> (m)->update_workspace_callback ();

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

  QModelIndex local_model_index = m->index (0, 0);
  QModelIndex global_model_index = m->index (1, 0);
  QModelIndex persistent_model_index = m->index (2, 0);

  if (_explicit_collapse.local) {
    view->collapse (local_model_index);
  } else {
    view->expand (local_model_index);
  }

  if (_explicit_collapse.global) {
    view->collapse (global_model_index);
  } else {
    view->expand (global_model_index);
  }

  if (_explicit_collapse.persistent) {
    view->collapse (persistent_model_index);
  } else {
    view->expand (persistent_model_index);
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
  QAbstractItemModel *m = view->model ();

  QMap<int, QVariant> item_data = m->itemData (index);

  if (item_data[0] == "Local")
    _explicit_collapse.local = true;
  if (item_data[0] == "Global")
    _explicit_collapse.global = true;
  if (item_data[0] == "Persistent")
    _explicit_collapse.persistent = true;
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
  QAbstractItemModel *m = view->model ();

  QMap<int, QVariant> item_data = m->itemData (index);

  if (item_data[0] == "Local")
    _explicit_collapse.local = false;
  if (item_data[0] == "Global")
    _explicit_collapse.global = false;
  if (item_data[0] == "Persistent")
    _explicit_collapse.persistent = false;
}

void
workspace_view::item_double_clicked (QModelIndex)
{
  // TODO: Implement opening a dialog that allows the user to change a variable in the workspace.
}

void
workspace_view::closeEvent (QCloseEvent *e)
{
  emit active_changed (false);
  QDockWidget::closeEvent (e);
}

// slot for signal that is emitted when floating property changes
void
workspace_view::top_level_changed (bool floating)
{
  if(floating)
    {
      setWindowFlags(Qt::Window);  // make a window from the widget when floating
      show();                      // make it visible again since setWindowFlags hides it
    }
}

void
workspace_view::focus (void)
{
  if (! isVisible ())
    setVisible (true);

  setFocus ();
  activateWindow ();
  raise ();
}

void
workspace_view::handle_visibility (bool visible)
{
  // if changed to visible and widget is not floating
  if (visible && ! isFloating ())
    focus ();
}
