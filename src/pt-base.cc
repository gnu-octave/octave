// tree-base.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include <iostream.h>

#include "tree-base.h"

// Current indentation.
int tree_print_code::curr_print_indent_level = 0;

// Nonzero means we are at the beginning of a line.
int tree_print_code::beginning_of_line = 1;

// All print_code() functions should use this to print new lines.

void
tree_print_code::print_code_new_line (ostream& os)
{
  os << "\n";

  beginning_of_line = 1;
}

// Each print_code() function should call this before printing
// anything.
//
// This doesn't need to be fast, but isn't there a better way?

void
tree_print_code::print_code_indent (ostream& os)
{
  assert (curr_print_indent_level >= 0);
 
  if (beginning_of_line)
    {
      os.form ("%*s", curr_print_indent_level, "");
      beginning_of_line = 0;
    }
}

// For ressetting print_code state.

void
tree_print_code::print_code_reset (void)
{
  beginning_of_line = 1;
  curr_print_indent_level = 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
