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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <iostream.h>
#include <strstream.h>

#include "error.h"
#include "pager.h"
#include "ov.h"
#include "pt-exp-base.h"

// Expressions.

bool
tree_expression::is_logically_true (const char *warn_for)
{
  bool expr_value = false;

  octave_value t1 = eval (false);

  if (! error_state)
    {
      if (t1.is_defined ())
	return t1.is_true ();
      else
	::error ("%s: undefined value used in conditional expression",
		 warn_for);
    }
  else
    ::error ("%s: error evaluating conditional expression", warn_for);

  return expr_value;
}

void
tree_expression::mark_for_possible_ans_assign (void)
{
  panic_impossible ();
}

octave_value
tree_expression::eval (bool /* print */)
{
  panic ("invalid evaluation of generic expression");
  return octave_value ();
}

string
tree_expression::original_text (void) const
{
  return string ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
