// gripes.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef __GNUG__
#pragma implementation
#endif

#include "gripes.h"
#include "error.h"

void
gripe_string_invalid (void)
{
  error ("string constant used in invalid context");
}

void
gripe_range_invalid (void)
{
  error ("range constant used in invalid context");
}

void
gripe_nonconformant (void)
{
  error ("nonconformant matrices");
}

void
gripe_empty_arg (const char *name, int is_error)
{
  if (is_error)
    error ("%s: empty matrix is invalid as an argument", name);
  else
    warning ("%s: argument is empty matrix", name);
}

void
gripe_square_matrix_required (const char *name)
{
  error ("%s: argument must be a square matrix", name);
}

void
gripe_user_supplied_eval (const char *name)
{
  error ("%s: evaluation of user-supplied function failed", name);
}

void
gripe_user_returned_invalid (const char *name)
{
  error ("%s: user-supplied function returned invalid value", name);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
