/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "workspace-view.h"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>

workspace_view::workspace_view (QWidget * parent) : QDockWidget
  (parent)
{
  setObjectName ("WorkspaceView");
  setWindowTitle (tr ("Workspace"));

  _workspace_tree_view = new QTreeView (this);
  _workspace_tree_view->setHeaderHidden (false);
  _workspace_tree_view->setAlternatingRowColors (true);
  _workspace_tree_view->setAnimated (true);
  _workspace_tree_view->setModel(octave_link::instance()->get_workspace_model());

  setWidget (new QWidget (this));
  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (_workspace_tree_view);
  layout->setMargin (2);
  widget ()->setLayout (layout);

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT(handle_visibility_changed (bool)));

  connect (octave_link::instance()->get_workspace_model(), SIGNAL(expand_request()),
           _workspace_tree_view, SLOT(expandAll()));

  connect(&_update_workspace_model_timer, SIGNAL (timeout ()),
    octave_link::instance()->get_workspace_model(),
          SLOT (update_from_symbol_table ()));

  _update_workspace_model_timer.setInterval (1000);
  _update_workspace_model_timer.setSingleShot (false);
  _update_workspace_model_timer.start ();
}

void
workspace_view::handle_visibility_changed (bool visible)
{
  if (visible)
  emit active_changed (true);
}

void
workspace_view::closeEvent (QCloseEvent *event)
{
  emit active_changed (false);
  QDockWidget::closeEvent (event);
}
