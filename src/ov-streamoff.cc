/*

Copyright (C) 2003 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "so-array.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "ov-streamoff.h"
#include "oct-obj.h"
#include "utils.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"

template class octave_base_matrix<streamoff_array>;

DEFINE_OCTAVE_ALLOCATOR (octave_streamoff);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_streamoff,
				     "streamoff", "streamoff");

std::streamoff
octave_streamoff::streamoff_value (void) const
{
  std::streamoff retval (-1);

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "streamoff array", "scalar streamoff");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("streamoff array", "scalar streamoff");

  return retval;
}

void
octave_streamoff::print (std::ostream& os, bool) const
{
  print_raw (os);
  newline (os);
}

void
octave_streamoff::print_raw (std::ostream& os, bool) const
{
  dim_vector dv = matrix.dims ();
  os << "<" << dv.str () << " streamoff object>";
}

DEFUN (isstreamoff, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isstreamoff (@var{x})\n\
Return true if @var{x} is a streamoff object.  Otherwise, return\n\
false.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_streamoff ();
  else
    print_usage ();

  return retval;
}

DEFUN (streamoff, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} streamoff (@var{x})\n\
@deftypefnx {Built-in Function} {} streamoff (@var{n}, @var{m})\n\
Create a new streamoff array object.  If invoked with a single scalar\n\
argument, @code{streamoff} returns a square streamoff array with\n\
the dimension specified.  If you supply two scalar arguments,\n\
@code{streamoff} takes them to be the number of rows and columns.\n\
If given a vector with two elements, @code{streamoff} uses the values\n\
of the elements as the number of rows and columns, respectively.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector dims;

  switch (nargin)
    {
    case 0:
      dims = dim_vector (0, 0);
      break;

    case 1:
      get_dimensions (args(0), "streamoff", dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).nint_value ();

	    if (error_state)
	      {
		error ("streamoff: expecting scalar arguments");
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      int ndim = dims.length ();

      check_dimensions (dims, "streamoff");

      if (! error_state)
	{
	  switch (ndim)
	    {
	    case 1:
	      retval = Cell (dims(0), dims(0), Matrix ());
	      break;

	    default:
	      retval = Cell (dims, Matrix ());
	      break;
	    }
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
