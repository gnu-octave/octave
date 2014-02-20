/*

Copyright (C) 2011 Michael Goffioul.

This file is part of QtHandles.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QtHandles is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QMenu>

#include "FigureWindow.h"

//////////////////////////////////////////////////////////////////////////////

namespace QtHandles
{

//////////////////////////////////////////////////////////////////////////////

FigureWindow::FigureWindow (QWidget* parent)
  : FigureWindowBase (parent)
{
}

//////////////////////////////////////////////////////////////////////////////

FigureWindow::~FigureWindow (void)
{
}

//////////////////////////////////////////////////////////////////////////////

QMenu* FigureWindow::createPopupMenu (void)
{
  // For the time being, disable menubar/toolbar popup menu
  return 0;
}

//////////////////////////////////////////////////////////////////////////////

}; // namespace QtHandles
