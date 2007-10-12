/*

Copyright (C) 2006, 2007 David Bateman

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

#include <cstdlib>
#include <string>

#include "variables.h"
#include "utils.h"
#include "pager.h"
#include "defun-dld.h"
#include "gripes.h"
#include "quit.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

template <typename T, typename M>
octave_value_list
sparse_find_non_zero_elem_idx (const T& v, M& val, int nargout, 
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

  val = M(result_nr, result_nc);

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
	    i_idx (cx) = static_cast<double> (v.ridx(i) + 1);
	    j_idx (cx) = static_cast<double> (j + 1);
	    idx (cx) = j * nr + v.ridx(i) + 1; 
	    val (cx) = v.data(i);
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

      val.resize (0, 0);
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

template octave_value_list sparse_find_non_zero_elem_idx 
  (const SparseMatrix&, Matrix&, int, octave_idx_type, int);

template octave_value_list sparse_find_non_zero_elem_idx 
  (const SparseComplexMatrix&, ComplexMatrix&, int, octave_idx_type, int);


// PKG_ADD: dispatch ("find", "spfind", "sparse matrix");
// PKG_ADD: dispatch ("find", "spfind", "sparse complex matrix");
// PKG_ADD: dispatch ("find", "spfind", "sparse bool matrix");
DEFUN_DLD (spfind, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} spfind (@var{x})\n\
@deftypefnx {Loadable Function} {} spfind (@var{x}, @var{n})\n\
@deftypefnx {Loadable Function} {} spfind (@var{x}, @var{n}, @var{direction})\n\
@deftypefnx {Loadable Function} {[@var{i}, @var{j}, @var{v}} spfind (@dots{})\n\
\n\
A sparse version of the @code{find} function. Please see the @code{find}\n\
for details of its use.\n\
\n\
Note that this function is particularly useful for sparse matrices, as\n\
it extracts the non-zero elements as vectors, which can then be used to\n\
create the original matrix. For example,\n\
\n\
@example\n\
@group\n\
sz = size(a);\n\
[i, j, v] = spfind (a);\n\
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

   if (arg.is_real_type ())
     {
       SparseMatrix v = arg.sparse_matrix_value ();
       Matrix val;

       if (! error_state)
	 retval = sparse_find_non_zero_elem_idx  (v, val, nargout, n_to_find, direction);
     }
   else if (arg.is_complex_type ())
     {
       SparseComplexMatrix v = arg.sparse_complex_matrix_value ();
       ComplexMatrix val;

       if (! error_state)
	 retval = sparse_find_non_zero_elem_idx  (v, val, nargout, n_to_find, direction);
     }
   else 
     gripe_wrong_type_arg ("spfind", arg);

   return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
