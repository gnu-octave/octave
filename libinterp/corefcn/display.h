////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#if ! defined (octave_display_h)
#define octave_display_h 1

#include "octave-config.h"

#include <string>

class Matrix;

OCTAVE_BEGIN_NAMESPACE(octave)

class display_info
{
public:

  // Create object with default values.  To be useful, you must call
  // initialize to find the actual system parameters for the given
  // display.

  display_info (void)
    : m_rx (72), m_ry (72), m_ht (1), m_wd (1), m_dp (0),
      m_dpy_avail (false), m_msg ()
  { }

  ~display_info (void) = default;

  display_info (const display_info&) = default;

  display_info& operator = (const display_info&) = default;

  void initialize (void);

  double x_dpi (void) const { return m_rx; }

  double y_dpi (void) const { return m_ry; }

  int height (void) const { return m_ht; }

  int width (void) const { return m_wd; }

  int depth (void) const { return m_dp; }

  bool display_available (void) const { return m_dpy_avail; }

  std::string message (void) const { return m_msg; }

private:

  // X- and Y- Resolution of the display in dots (pixels) per inch.
  double m_rx;
  double m_ry;

  // Height, width, and depth of the display.
  int m_ht;
  int m_wd;
  int m_dp;

  bool m_dpy_avail;

  // Message associated with any initiailization failure.  Set if
  // m_dpy_avail is false.
  std::string m_msg;
};

OCTAVE_END_NAMESPACE(octave)

#endif
