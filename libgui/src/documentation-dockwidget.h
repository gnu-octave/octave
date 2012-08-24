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

#ifndef DOCUMENTATIONDOCKWIDGET_H
#define DOCUMENTATIONDOCKWIDGET_H

#include <QObject>
#include <QDockWidget>
#include "webinfo.h"

class documentation_dock_widget : public QDockWidget
{
  Q_OBJECT
public:
  documentation_dock_widget (QWidget *parent = 0);

public slots:
  /** Slot to steer changing visibility from outside. */
  void handle_visibility_changed (bool visible);

signals:
  /** Custom signal that tells if a user has clicked away that dock widget. */
  void active_changed (bool active);

protected:
  void closeEvent (QCloseEvent *event);

private:
  webinfo *_webinfo;
};

#endif // DOCUMENTATIONDOCKWIDGET_H
