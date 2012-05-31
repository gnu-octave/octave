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

  m_workspaceTreeView = new QTreeView (this);
  m_workspaceTreeView->setHeaderHidden (false);
  m_workspaceTreeView->setAlternatingRowColors (true);
  m_workspaceTreeView->setAnimated (true);
  m_workspaceTreeView->setModel(octave_link::instance()->workspaceModel());

  setWidget (new QWidget (this));
  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (m_workspaceTreeView);
  layout->setMargin (2);
  widget ()->setLayout (layout);

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT(handleVisibilityChanged (bool)));

  connect (octave_link::instance()->workspaceModel(), SIGNAL(expandRequest()),
           m_workspaceTreeView, SLOT(expandAll()));
}

void
workspace_view::handleVisibilityChanged (bool visible)
{
  if (visible)
  emit activeChanged (true);
}

void
workspace_view::closeEvent (QCloseEvent *event)
{
  emit activeChanged (false);
  QDockWidget::closeEvent (event);
}
