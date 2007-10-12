/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2003, 2004, 2005, 2006,
              2007 John W. Eaton

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

#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

// Find at most N_TO_FIND nonzero elements in NDA.  Search forward if
// DIRECTION is 1, backward if it is -1.  NARGOUT is the number of
// output arguments.  If N_TO_FIND is -1, find all nonzero elements.

template <typename T>
octave_value_list
find_nonzero_elem_idx (const T& nda, int nargout, octave_idx_type n_to_find,
		       int direction)
{
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  octave_idx_type count = 0;

  octave_idx_type nel = nda.nelem ();

  // Set the starting element to the correct value based on the
  // direction to search.
  octave_idx_type k = 0;
  if (direction == -1)
    k = nel - 1;

  // Search in the default range.
  octave_idx_type start_el = -1;
  octave_idx_type end_el = -1;

  // Search for the number of elements to return.
  while (k < nel && k > -1 && n_to_find != count)
    {
      OCTAVE_QUIT;

      if (nda(k) != 0.0)
	{
	  end_el = k;
	  if (start_el == -1)
	    start_el = k;
	  count++;
	}
      k = k + direction;
    }

  // Reverse the range if we're looking backward.
  if (direction == -1)
    {
      octave_idx_type tmp_el = start_el;
      start_el = end_el;
      end_el = tmp_el;
    }
  // Fix an off by one error.
  end_el++;

  // If the original argument was a row vector, force a row vector of
  // the overall indices to be returned.  But see below for scalar
  // case...

  octave_idx_type result_nr = count;
  octave_idx_type result_nc = 1;

  bool scalar_arg = false;

  if (nda.ndims () == 2 && nda.rows () == 1)
    {
      result_nr = 1;
      result_nc = count;

      scalar_arg = (nda.columns () == 1);
    }

  Matrix idx (result_nr, result_nc);

  Matrix i_idx (result_nr, result_nc);
  Matrix j_idx (result_nr, result_nc);

  T val (dim_vector (result_nr, result_nc));

  if (count > 0)
    {
      count = 0;

      octave_idx_type nr = nda.rows ();

      octave_idx_type i = 0;

      // Search for elements to return.  Only search the region where
      // there are elements to be found using the count that we want
      // to find.

      // For compatibility, all N-d arrays are handled as if they are
      // 2-d, with the number of columns equal to "prod (dims (2:end))".

      for (k = start_el; k < end_el; k++)
	{
	  OCTAVE_QUIT;

	  if (nda(k) != 0.0)
	    {
	      idx(count) = k + 1;

	      octave_idx_type xr = k % nr;
	      i_idx(count) = xr + 1;
	      j_idx(count) = (k - xr) / nr + 1;

	      val(count) = nda(k);

	      count++;
	    }

	  i++;
	}
    }
  else if (scalar_arg)
    {
      idx.resize (0, 0);

      i_idx.resize (0, 0);
      j_idx.resize (0, 0);

      val.resize (dim_vector (0, 0));
    }

  switch (nargout)
    {
    default:
    case 3:
      retval(2) = val;
      // Fall through!

    case 2:
      retval(1) = j_idx;
      retval(0) = i_idx;
      break;

    case 1:
    case 0:
      retval(0) = idx;
      break;
    }

  return retval;
}

template octave_value_list find_nonzero_elem_idx (const NDArray&, int,
						  octave_idx_type, int);

template octave_value_list find_nonzero_elem_idx (const ComplexNDArray&, int,
						  octave_idx_type, int);

DEFUN_DLD (find, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} find (@var{x})\n\
@deftypefnx {Loadable Function} {} find (@var{x}, @var{n})\n\
@deftypefnx {Loadable Function} {} find (@var{x}, @var{n}, @var{direction})\n\
Return a vector of indices of nonzero elements of a matrix, as a row if\n\
@var{x} is a row or as a column otherwise.  To obtain a single index for\n\
each matrix element, Octave pretends that the columns of a matrix form one\n\
long vector (like Fortran arrays are stored).  For example,\n\
\n\
@example\n\
@group\n\
find (eye (2))\n\
     @result{} [ 1; 4 ]\n\
@end group\n\
@end example\n\
\n\
If two outputs are requested, @code{find} returns the row and column\n\
indices of nonzero elements of a matrix.  For example,\n\
\n\
@example\n\
@group\n\
[i, j] = find (2 * eye (2))\n\
     @result{} i = [ 1; 2 ]\n\
     @result{} j = [ 1; 2 ]\n\
@end group\n\
@end example\n\
\n\
If three outputs are requested, @code{find} also returns a vector\n\
containing the nonzero values.  For example,\n\
\n\
@example\n\
@group\n\
[i, j, v] = find (3 * eye (2))\n\
     @result{} i = [ 1; 2 ]\n\
     @result{} j = [ 1; 2 ]\n\
     @result{} v = [ 3; 3 ]\n\
@end group\n\
@end example\n\
\n\
If two inputs are given, @var{n} indicates the number of elements to\n\
find from the beginning of the matrix or vector.\n\
\n\
If three inputs are given, @var{direction} should be one of \"first\" or\n\
\"last\" indicating that it should start counting found elements from the\n\
first or last element.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 3 || nargin < 1)
    {
      print_usage ();
      return retval;
    }

  // Setup the default options.
  octave_idx_type n_to_find = -1;
  if (nargin > 1)
    {
      n_to_find = args(1).int_value ();
      if (error_state)
	{
	  error ("find: expecting second argument to be an integer");
	  return retval;
	}
    }

  // Direction to do the searching (1 == forward, -1 == reverse).
  int direction = 1;
  if (nargin > 2)
    {
      direction = 0;

      std::string s_arg = args(2).string_value ();

      if (! error_state)
	{
	  if (s_arg == "first")
	    direction = 1;
	  else if (s_arg == "last")
	    direction = -1;
	}

      if (direction == 0)
	{
	  error ("find: expecting third argument to be \"first\" or \"last\"");
	  return retval;
	}
    }

  octave_value arg = args(0);

  if (arg.is_real_type ())
    {
      NDArray nda = arg.array_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (nda, nargout, n_to_find, direction);
    }
  else if (arg.is_complex_type ())
    {
      ComplexNDArray cnda = arg.complex_array_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (cnda, nargout, n_to_find, direction);
    }
  else if (arg.is_string ())
    {
      charNDArray cnda = arg.char_array_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (cnda, nargout, n_to_find, direction);
    }
  else
    {
      gripe_wrong_type_arg ("find", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
