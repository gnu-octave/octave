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

#include "error.h"
#include "oct-obj.h"
#include "oct-builtin.h"
#include "ov.h"

octave_value
octave_builtin::eval (void)
{
  octave_value retval;

  if (error_state)
    return retval;

  octave_value_list args;

  octave_value_list tmp = (*f) (args, 0);

  if (tmp.length () > 0)
    retval = tmp(0);

  return retval;
}

// Are any of the arguments `:'?

static bool
any_arg_is_magic_colon (const octave_value_list& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_magic_colon ())
	return true;

  return false;
}

octave_value_list
octave_builtin::eval (int nargout, const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (any_arg_is_magic_colon (args))
    ::error ("invalid use of colon in function argument list");
  else
    retval = (*f) (args, nargout);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
