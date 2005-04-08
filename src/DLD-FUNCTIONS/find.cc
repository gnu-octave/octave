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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

inline double real (double x) { return x; }

template <typename T>
octave_value_list
find_nonzero_elem_idx (const T& nda, int nargout)
{
  octave_value_list retval (((nargout == 0) ? 1 : nargout), Matrix ());

  octave_idx_type count = 0;

  octave_idx_type nel = nda.nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;

      if (nda(i) != 0.0)
	count++;
    }

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
      octave_idx_type j = 0;

      for (octave_idx_type k = 0; k < nel; k++)
	{
	  OCTAVE_QUIT;

	  if (nda(k) != 0.0)
	    {
	      idx(count) = k + 1;

	      i_idx(count) = i + 1;
	      j_idx(count) = j + 1;

	      val(count) = nda(k);

	      count++;
	    }

	  i++;

	  if (i == nr)
	    {
	      i = 0;

	      j++;
	    }
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
    case 0:
    case 1:
      retval(0) = idx;
      break;

    case 3:
      retval(2) = val;
      // Fall through!

    case 2:
      retval(1) = j_idx;
      retval(0) = i_idx;
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

template octave_value_list find_nonzero_elem_idx (const NDArray&, int);

template octave_value_list find_nonzero_elem_idx (const ComplexNDArray&, int);

DEFUN_DLD (find, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} find (@var{x})\n\
Return a vector of indices of nonzero elements of a matrix.  To obtain a\n\
single index for each matrix element, Octave pretends that the columns\n\
of a matrix form one long vector (like Fortran arrays are stored).  For\n\
example,\n\
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
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 3)
    {
      print_usage ("find");
      return retval;
    }

  octave_value arg = args(0);

  if (arg.is_real_type ())
    {
      NDArray nda = arg.array_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (nda, nargout);
    }
  else if (arg.is_complex_type ())
    {
      ComplexNDArray cnda = arg.complex_array_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (cnda, nargout);
    }
  else if (arg.is_string ())
    {
      charNDArray cnda = arg.char_array_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (cnda, nargout);
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
