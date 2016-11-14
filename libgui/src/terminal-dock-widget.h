/*

Copyright (C) 2013-2016 John W. Eaton
Copyright (C) 2011-2016 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_terminal_dock_widget_h)
#define octave_terminal_dock_widget_h 1

#include <QString>

#include "QTerminal.h"
#include "octave-dock-widget.h"

class terminal_dock_widget : public octave_dock_widget
{
  Q_OBJECT

public:

  terminal_dock_widget (QWidget *parent = 0);

  ~terminal_dock_widget (void);

  bool has_focus (void) const;

  void focus (void);

signals:

  void interrupt_signal (void);

protected slots:

  void terminal_interrupt (void);

private:

  QTerminal *terminal;
};

#endif

