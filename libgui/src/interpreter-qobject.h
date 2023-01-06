////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#if ! defined (octave_interpreter_qobject_h)
#define octave_interpreter_qobject_h 1

#include <QObject>

#include "qt-interpreter-events.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class base_qobject;

class interpreter_qobject : public QObject
{
  Q_OBJECT

public:

  interpreter_qobject (base_qobject& oct_qobj);

  ~interpreter_qobject (void) = default;

  qt_interpreter_events * qt_link (void);

  void interpreter_event (const fcn_callback& fcn);

  void interpreter_event (const meth_callback& meth);

  void interrupt (void);

  // Note: PAUSE, STOP, and RESUME are currently only used by the new
  // experimental terminal widget.
  void pause (void);
  void stop (void);
  void resume (void);

signals:

  void ready (void);

  void shutdown_finished (int);

public slots:

  // Programming Note: With the current design of the interpreter,
  // additional signals will not be noticed because the execute
  // function starts the Octave interpreter and it doesn't return
  // until the interpreter exits.  So the Qt event loop running in the
  // thread where the interpreter_qobject object lives never has a
  // chance to see another signal.  Changing the design of the
  // terminal widget as proposed and discussed here:
  //
  //   https://lists.gnu.org/archive/html/octave-maintainers/2019-05/msg00115.html
  //   https://lists.gnu.org/archive/html/octave-maintainers/2019-06/msg00009.html
  //
  // coulld solve that problem.  Briefly, instead of having the Octave
  // interpreter block and wait for keyboard input, the terminal
  // widget would be in charge of accepting keyboard events and use
  // readline in callback mode.  Then the terminal widget and the
  // interpreter will not block except when executing code.  Then we
  // could have the interpreter qobject directly accept signals.

  //! Initialize and execute the octave interpreter.

  void execute (void);

private:

  base_qobject& m_octave_qobj;

  interpreter *m_interpreter;
};

OCTAVE_END_NAMESPACE(octave)

#endif
