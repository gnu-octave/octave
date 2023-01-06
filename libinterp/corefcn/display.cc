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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "singleton-cleanup.h"

#include "cdisplay.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "interpreter.h"
#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void display_info::initialize (void)
{
  int avail = 0;

  const char *msg
    = octave_get_display_info (nullptr, &m_ht, &m_wd, &m_dp,
                               &m_rx, &m_ry, &avail);

  m_dpy_avail = avail;

  if (msg)
    m_msg = msg;
}

DEFMETHOD (have_window_system, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} have_window_system ()
Return true if a window system is available (X11, Windows, or Apple OS X)
and false otherwise.
@seealso{isguirunning}
@end deftypefn */)
{
  display_info& dpy_info = interp.get_display_info ();

  return ovl (dpy_info.display_available ());
}

OCTAVE_END_NAMESPACE(octave)
