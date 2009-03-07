/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2003, 2004, 2005, 2006,
              2007, 2008, 2009 John W. Eaton

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
find_nonzero_elem_idx (const Array<T>& nda, int nargout, 
		       octave_idx_type n_to_find, int direction)
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

      if (nda(k) != T ())
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

  ArrayN<T> val (dim_vector (result_nr, result_nc));

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

	  if (nda(k) != T ())
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

template octave_value_list find_nonzero_elem_idx (const Array<double>&, int,
						  octave_idx_type, int);

template octave_value_list find_nonzero_elem_idx (const Array<Complex>&, int,
						  octave_idx_type, int);

template octave_value_list find_nonzero_elem_idx (const Array<float>&, int,
						  octave_idx_type, int);

template octave_value_list find_nonzero_elem_idx (const Array<FloatComplex>&,
						  int, octave_idx_type, int);

template <typename T>
octave_value_list
find_nonzero_elem_idx (const Sparse<T>& v, int nargout, 
		       octave_idx_type n_to_find, int direction)
{
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());


  octave_idx_type nc = v.cols();
  octave_idx_type nr = v.rows();
  octave_idx_type nz = v.nnz();

  // Search in the default range.
  octave_idx_type start_nc = -1;
  octave_idx_type end_nc = -1;
  octave_idx_type count;
 
  // Search for the range to search
  if (n_to_find < 0)
    {
      start_nc = 0;
      end_nc = nc;
      n_to_find = nz;
      count = nz;
    }
  else if (direction > 0)
    {
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  OCTAVE_QUIT;
	  if (v.cidx(j) == 0 && v.cidx(j+1) != 0)
	    start_nc = j;
	  if (v.cidx(j+1) >= n_to_find)
	    {
	      end_nc = j + 1;
	      break;
	    }
	}
    }
  else
    {
      for (octave_idx_type j = nc; j > 0; j--)
	{
	  OCTAVE_QUIT;
	  if (v.cidx(j) == nz && v.cidx(j-1) != nz)
	    end_nc = j;
	  if (nz - v.cidx(j-1) >= n_to_find)
	    {
	      start_nc = j - 1;
	      break;
	    }
	}
    }

  count = (n_to_find > v.cidx(end_nc) - v.cidx(start_nc) ? 
	   v.cidx(end_nc) - v.cidx(start_nc) : n_to_find);

  // If the original argument was a row vector, force a row vector of
  // the overall indices to be returned.  But see below for scalar
  // case...

  octave_idx_type result_nr = count;
  octave_idx_type result_nc = 1;

  bool scalar_arg = false;

  if (v.rows () == 1)
    {
      result_nr = 1;
      result_nc = count;

      scalar_arg = (v.columns () == 1);
    }

  Matrix idx (result_nr, result_nc);

  Matrix i_idx (result_nr, result_nc);
  Matrix j_idx (result_nr, result_nc);

  ArrayN<T> val (dim_vector (result_nr, result_nc));

  if (count > 0)
    {
      // Search for elements to return.  Only search the region where
      // there are elements to be found using the count that we want
      // to find.
      for (octave_idx_type j = start_nc, cx = 0; j < end_nc; j++) 
	for (octave_idx_type i = v.cidx(j); i < v.cidx(j+1); i++ ) 
	  {
	    OCTAVE_QUIT;
	    if (direction < 0 && i < nz - count)
	      continue;
	    i_idx(cx) = static_cast<double> (v.ridx(i) + 1);
	    j_idx(cx) = static_cast<double> (j + 1);
	    idx(cx) = j * nr + v.ridx(i) + 1; 
	    val(cx) = v.data(i);
	    cx++;
	    if (cx == count)
	      break;
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

    case 5:
      retval(4) = nc;
      // Fall through

    case 4:
      retval(3) = nr;
      // Fall through

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

template octave_value_list find_nonzero_elem_idx (const Sparse<double>&, int,
						  octave_idx_type, int);

template octave_value_list find_nonzero_elem_idx (const Sparse<Complex>&, int,
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
\n\
Note that this function is particularly useful for sparse matrices, as\n\
it extracts the non-zero elements as vectors, which can then be used to\n\
create the original matrix. For example,\n\
\n\
@example\n\
@group\n\
sz = size(a);\n\
[i, j, v] = find (a);\n\
b = sparse(i, j, v, sz(1), sz(2));\n\
@end group\n\
@end example\n\
@seealso{sparse}\n\
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

  if (arg.is_sparse_type ())
    {
      if (arg.is_real_type ())
	{
	  SparseMatrix v = arg.sparse_matrix_value ();

	  if (! error_state)
	    retval = find_nonzero_elem_idx (v, nargout, 
					    n_to_find, direction);
	}
      else if (arg.is_complex_type ())
	{
	  SparseComplexMatrix v = arg.sparse_complex_matrix_value ();

	  if (! error_state)
	    retval = find_nonzero_elem_idx (v, nargout, 
					    n_to_find, direction);
	}
      else 
	gripe_wrong_type_arg ("find", arg);
    }
  else
    {
      if (arg.is_single_type ())
	{
	  if (arg.is_real_type ())
	    {
	      FloatNDArray nda = arg.float_array_value ();

	      if (! error_state)
		retval = find_nonzero_elem_idx (nda, nargout, 
						n_to_find, direction);
	    }
	  else if (arg.is_complex_type ())
	    {
	      FloatComplexNDArray cnda = arg.float_complex_array_value ();

	      if (! error_state)
		retval = find_nonzero_elem_idx (cnda, nargout, 
						n_to_find, direction);
	    }
	}
      else
	{
	  if (arg.is_real_type ())
	    {
	      NDArray nda = arg.array_value ();

	      if (! error_state)
		retval = find_nonzero_elem_idx (nda, nargout, 
						n_to_find, direction);
	    }
	  else if (arg.is_complex_type ())
	    {
	      ComplexNDArray cnda = arg.complex_array_value ();

	      if (! error_state)
		retval = find_nonzero_elem_idx (cnda, nargout, 
						n_to_find, direction);
	    }
	  else if (arg.is_string ())
	    {
	      charNDArray cnda = arg.char_array_value ();

	      if (! error_state)
		retval = find_nonzero_elem_idx (cnda, nargout, 
						n_to_find, direction);
	    }
	  else
	    {
	      gripe_wrong_type_arg ("find", arg);
	    }
	}
    }

  return retval;
}

/*
%!assert(find ([1, 0, 1, 0, 1]), [1, 3, 5]);
%!assert(find ([1; 0; 3; 0; 1]), [1; 3; 5]);
%!assert(find ([0, 0, 2; 0, 3, 0; -1, 0, 0]), [3; 5; 7]);

%!test
%! [i, j, v] = find ([0, 0, 2; 0, 3, 0; -1, 0, 0]);
%! 
%! assert(i, [3; 2; 1]);
%! assert(j, [1; 2; 3]);
%! assert(v, [-1; 3; 2]);

%!assert(find (single([1, 0, 1, 0, 1])), [1, 3, 5]);
%!assert(find (single([1; 0; 3; 0; 1])), [1; 3; 5]);
%!assert(find (single([0, 0, 2; 0, 3, 0; -1, 0, 0])), [3; 5; 7]);

%!test
%! [i, j, v] = find (single([0, 0, 2; 0, 3, 0; -1, 0, 0]));
%! 
%! assert(i, [3; 2; 1]);
%! assert(j, [1; 2; 3]);
%! assert(v, single([-1; 3; 2]));

%!error <Invalid call to find.*> find ();

 */

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
