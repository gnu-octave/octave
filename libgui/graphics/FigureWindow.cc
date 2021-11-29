////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2021 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QMenu>

#include "FigureWindow.h"

namespace octave
{

  FigureWindow::FigureWindow (QWidget *xparent)
    : FigureWindowBase (xparent)
  {
    // set icon from application resources
    setWindowIcon (QIcon (":/actions/icons/graphic_logo_Figure.png"));
  }

  FigureWindow::~FigureWindow (void)
  { }

  QMenu *
  FigureWindow::createPopupMenu (void)
  {
    // For the time being, disable menubar/toolbar popup menu
    return nullptr;
  }

  void FigureWindow::showEvent (QShowEvent *ev)
  {
    QMainWindow::showEvent (ev);
    emit figureWindowShown();
  }

}
