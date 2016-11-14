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

#if ! defined (octave_octave_interpreter_h)
#define octave_octave_interpreter_h 1

#include <QObject>

#include "octave.h"

#include "thread-manager.h"

class octave_interpreter : public QObject
{
  Q_OBJECT

public:

  // An object to manage the Octave interpreter.

  octave_interpreter (octave::application *app_context);

  ~octave_interpreter (void) { }

signals:

  void octave_ready_signal ();

public slots:

  // Initialize and execute the octave interpreter.

  void execute (void);

  void interrupt (void);

private:

  octave_thread_manager thread_manager;

  octave::application *m_app_context;

  int m_exit_status;
};

#endif

