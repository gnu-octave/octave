/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <sstream>
#include <string>

#include "ov-fcn.h"
#include "pt.h"
#include "pt-pr-code.h"

// If true, stop executing at the next possible point.
bool tree::break_next = false;

// The line where dbnext was executed.
int tree::last_line = 0;

// The function where the last breakpoint occurred.
const octave_function *tree::break_function = 0;

// The statement where the last breakpoint occurred.
const tree *tree::break_statement = 0;

// Hide the details of the string buffer so that we are less likely to
// create a memory leak.

std::string
tree::str_print_code (void)
{
  std::ostringstream buf;

  tree_print_code tpc (buf);

  accept (tpc);

  std::string retval = buf.str ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
