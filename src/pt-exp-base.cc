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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>
#include <strstream.h>

#include "error.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-exp-base.h"
#include "user-prefs.h"

// Expressions.

bool
tree_expression::is_logically_true (const char *warn_for)
{
  bool expr_value = false;

  octave_value t1 = eval (false);

  if (! error_state)
    {
      if (t1.is_defined ())
	{
	  if (t1.rows () == 0 || t1.columns () == 0)
	    {
	      t1 = 0.0;
	      int flag = user_pref.propagate_empty_matrices;
	      if (flag < 0)
		warning ("%s: empty matrix used in conditional expression",
			 warn_for);
	      else if (flag == 0)
		{
		  ::error ("%s: empty matrix used in conditional expression",
			   warn_for);
		  return expr_value;
		}
	    }
	  else if (! t1.is_scalar_type ())
	    {
	      octave_value t2 = t1.all ();
	      if (! error_state)
		t1 = t2.all ();
	    }

	  if (! error_state)
	    {
	      if (t1.is_real_scalar ())
		expr_value = t1.double_value () != 0.0;
	      else if (t1.is_complex_scalar ())
		expr_value = t1.complex_value () != 0.0;
	      else
		panic_impossible ();
	    }
	  else
	    ::error ("%s: invalid type in conditional expression", warn_for);
	}
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
