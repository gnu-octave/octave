/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_float_fmt_h)
#define octave_float_fmt_h 1

enum floating_point_format
  {
    OCTAVE_IEEE_LITTLE,
    OCTAVE_IEEE_BIG,
    OCTAVE_VAX_D,
    OCTAVE_VAX_G,
    OCTAVE_CRAY,
    OCTAVE_UNKNOWN_FLT_FMT
  };

// The floating point format on this system.
extern floating_point_format native_float_format;

// Initializes the value of native_float_format.  Maybe this should be
// done automatically using a class with a static member.  Hmm...
extern int float_format_init (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
