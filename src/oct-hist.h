/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2005, 2006, 2007, 2008
              John W. Eaton

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

#if !defined (octave_octave_hist_h)
#define octave_octave_hist_h 1

#include <string>

#include "cmd-hist.h"

extern void initialize_history (bool read_history_file = false);

// Write timestamp to history file.
extern void octave_history_write_timestamp (void);

// TRUE means input is coming from temporary history file.
extern bool input_from_tmp_history_file;

// TRUE if we are saving history.
extern bool Vsaving_history;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
