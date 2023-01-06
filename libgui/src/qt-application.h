////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#if ! defined (octave_qt_application_h)
#define octave_qt_application_h 1

#include "octave.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Programming Note: This file must not include any Qt headers.  Any
// Qt header files required by the qt_application::execute function
// must be included only in the corresponding .cc file.

//! This class inherits from the pure-virtual base class
//! application and provides an implementation of the
//! application::execute method that starts an interface to Octave
//! that is based on Qt.  It may start a command-line interface that
//! allows Qt graphics to be used or it may start an interface that
//! provides the full GUI experience.

  class OCTGUI_API qt_application  : public application
{
 public:

  qt_application (int argc, char **argv);

  // No copying, at least not yet.

  qt_application (const qt_application&) = delete;

  qt_application& operator = (const qt_application&) = delete;

  ~qt_application (void) = default;

  // Should we start the GUI or fall back to the CLI?
  bool start_gui_p (void) const;

  int execute (void);

  bool gui_running (void) const { return m_gui_running; }
  void gui_running (bool arg) { m_gui_running = arg; }

 private:

  // If TRUE, the GUI should be started.
  bool m_gui_running = false;
};

OCTAVE_END_NAMESPACE(octave)

#endif
