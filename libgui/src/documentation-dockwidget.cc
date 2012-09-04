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

#include "documentation-dockwidget.h"

documentation_dock_widget::documentation_dock_widget (QWidget *parent)
  : QDockWidget (parent)
{
  setObjectName ("DocumentationDockWidget");
  setWindowTitle (tr ("Documentation"));

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility_changed (bool)));

  _webinfo = new webinfo (this);
  setWidget (_webinfo);
}

void
documentation_dock_widget::handle_visibility_changed (bool visible)
{
  if (visible)
    emit active_changed (true);
}

void
documentation_dock_widget::closeEvent (QCloseEvent *event)
{
  emit active_changed (false);
  QDockWidget::closeEvent (event);
}
