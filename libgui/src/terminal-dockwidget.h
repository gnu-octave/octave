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

#ifndef TERMINALDOCKWIDGET_H
#define TERMINALDOCKWIDGET_H

#include <QDockWidget>
#include "QTerminal.h"

class terminal_dock_widget : public QDockWidget
{
  Q_OBJECT
  public:
  terminal_dock_widget (QTerminal *terminal, QWidget *parent = 0);

signals:
  void active_changed (bool active);

public slots:
  void handle_visibility_changed (bool visible)
  {
    if (visible)
      emit active_changed (true);
  }
};

#endif // TERMINALDOCKWIDGET_H
