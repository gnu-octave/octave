/*

Copyright (C) 2003 John W. Eaton

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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "Array.h"
#include "Array.cc"
#include "ArrayN.h"
#include "ArrayN.cc"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "ov-streamoff.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"

INSTANTIATE_ARRAY_AND_ASSIGN (std::streamoff);

template class ArrayN<std::streamoff>;

template class octave_base_matrix<streamoff_array>;

DEFINE_OCTAVE_ALLOCATOR (octave_streamoff);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_streamoff,
				     "streamoff", "streamoff");
boolNDArray
streamoff_array::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR), true);
}

boolNDArray
streamoff_array::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ANY_EVAL (MX_ND_ANY_EXPR), false);
}

#if 0
streamoff_array&
streamoff_array::operator += (const streamoff_array& a)
{
  // XXX FIXME XXX 
  return *this;
}

streamoff_array&
streamoff_array::operator -= (const streamoff_array& a)
{
  // XXX FIXME XXX 
  return *this;
}
#endif

int
streamoff_array::compute_index (Array<int>& ra_idx,
				const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

SND_CMP_OP (mx_el_eq, ==, std::streamoff, , streamoff_array, , FBM)
SND_CMP_OP (mx_el_ne, !=, std::streamoff, , streamoff_array, , TBM)

NDS_CMP_OP (mx_el_eq, ==, streamoff_array, , std::streamoff, , FBM)
NDS_CMP_OP (mx_el_ne, !=, streamoff_array, , std::streamoff, , TBM)

NDND_CMP_OP (mx_el_eq, ==, streamoff_array, , streamoff_array, , FBM, TBM)
NDND_CMP_OP (mx_el_ne, !=, streamoff_array, , streamoff_array, , TBM, FBM)

NDND_BIN_OP (streamoff_array, operator +, streamoff_array, streamoff_array, mx_inline_add)
NDND_BIN_OP (streamoff_array, operator -, streamoff_array, streamoff_array, mx_inline_subtract)

NDS_BIN_OP (streamoff_array, operator +, streamoff_array, std::streamoff, mx_inline_add)
NDS_BIN_OP (streamoff_array, operator -, streamoff_array, std::streamoff, mx_inline_subtract)

SND_BIN_OP (streamoff_array, operator +, std::streamoff, streamoff_array, mx_inline_add)
SND_BIN_OP (streamoff_array, operator -, std::streamoff, streamoff_array, mx_inline_subtract)

std::streamoff
octave_streamoff::streamoff_value (void) const
{
  std::streamoff retval (-1);

  if (numel () > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("streamoff array", "scalar streamoff");

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
    print_usage ("isstreamoff");

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
