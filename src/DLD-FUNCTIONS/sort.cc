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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

// This is algorithm 5.2.4L from Knuth, Volume 3.

// XXX FIXME XXX -- there is way too much duplicated code here given
// that the sort algorithms are all the same, and only the type of the
// data and the comparison changes...
//
// Maybe some cpp abuse will make it better.

static Array<int>
create_index_array (int n)
{
  Array<int> l (n+2);

  l (0) = 1;

  for (int i = 1; i < n - 1; i++)
    l (i) = -(i+2);

  l (n-1) = 0;
  l (n) = 0;
  l (n+1) = 2;

  return l;
}

#define SORT_INIT_PHASE(n) \
  int s = 0; \
  int t = n + 1; \
  int p = l (s); \
  int q = l (t); \
  if (q == 0) \
     break

#define SORT_COMMON_CODE \
  p = -p; \
  q = -q; \
  if (q == 0) \
    { \
      l (s) = (l (s) < 0) \
	? ((p < 0) ? p : -p) \
	  : ((p >= 0) ? p : -p); \
      l (t) = 0; \
      break; \
    } \

#define SORT_REORDER_PHASE_ONE \
  l (s) = (l (s) < 0) \
    ? ((q < 0) ? q : -q) \
      : ((q >= 0) ? q : -q); \
  s = q; \
  q = l (q); \
  if (q <= 0) \
    { \
      l (s) = p; \
      s = t; \
      do \
	{ \
	  t = p; \
	  p = l (p); \
	} \
      while (p > 0); \
      SORT_COMMON_CODE; \
    } \

#define SORT_REORDER_PHASE_TWO \
  l (s) = (l (s) < 0) \
    ? ((p < 0) ? p : -p) \
      : ((p >= 0) ? p : -p); \
  s = p; \
  p = l (p); \
  if (p <= 0) \
    { \
      l (s) = q; \
      s = t; \
      do \
	{ \
	  t = q; \
	  q = l (q); \
	} \
      while (q > 0); \
      SORT_COMMON_CODE; \
    }

#define DO_SORT(n, condition) \
  while (1) \
    { \
      SORT_INIT_PHASE(n); \
      while (1) \
	{ \
	  if (condition) \
	    { \
	      SORT_REORDER_PHASE_ONE; \
	    } \
	  else \
	    { \
	      SORT_REORDER_PHASE_TWO; \
	    } \
	} \
    }

#define VECTOR_CREATE_RETURN_VALUES(vs, v) \
  int k = l (0); \
  idx (0) = k; \
  vs (0) = v (k-1); \
  for (int i = 1; i < n; i++) \
    { \
      k = l (static_cast<int> (idx (i-1))); \
      idx (i) = k; \
      vs (i) = v (k-1); \
    }

#define MATRIX_CREATE_RETURN_VALUES(ms, m) \
  int k = l (0); \
  idx (0, j) = k; \
  ms (0, j) = m (k-1, j); \
  for (int i = 1; i < nr; i++) \
    { \
      k = l (static_cast<int> (idx (i-1, j))); \
      idx (i, j) = k; \
      ms (i, j) = m (k-1, j); \
    }

static octave_value_list
mx_sort (const Matrix& m)
{
  octave_value_list retval;

  int nr = m.rows ();
  int nc = m.columns ();

  Matrix ms (nr, nc);
  Matrix idx (nr, nc);

  if (nr == 1 && nc > 0)
    {
      retval(1) = Matrix (nr, nc, 1.0);
      retval(0) = m;

      return retval;
    }
  else if (nr > 1 && nc > 0)
    {
      for (int j = 0; j < nc; j++)
	{
	  Array<int> l = create_index_array (nr);

	  DO_SORT (nr, (m (p-1, j) > m (q-1, j)));

	  MATRIX_CREATE_RETURN_VALUES (ms, m);
	}
    }

  retval(1) = idx;
  retval(0) = ms;

  return retval;
}

static octave_value_list
mx_sort (const RowVector& v)
{
  octave_value_list retval;

  int n = v.capacity ();

  RowVector vs (n);
  RowVector idx (n);

  if (n == 1)
    {
      retval(1) = RowVector (n, 1.0);
      retval(0) = v;

      return retval;
    }
  else if (n > 1)
    {
      Array<int> l = create_index_array (n);

      DO_SORT (n, (v (p-1) > v (q-1)));

      VECTOR_CREATE_RETURN_VALUES (vs, v);
    }

  retval(1) = idx;
  retval(0) = vs;

  return retval;
}

static octave_value_list
mx_sort (const ComplexMatrix& cm)
{
  octave_value_list retval;

  int nr = cm.rows ();
  int nc = cm.columns ();

  ComplexMatrix cms (nr, nc);
  Matrix idx (nr, nc);

  if (nr == 1 && nc > 0)
    {
      retval(1) = Matrix (nr, nc, 1.0);
      retval(0) = cm;

      return retval;
    }
  else if (nr > 1 && nc > 0)
    {
      for (int j = 0; j < nc; j++)
	{
	  Array<int> l = create_index_array (nr);

	  int all_elts_real = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (cm (i, j)) != 0.0)
	      {
		all_elts_real = 0;
		break;
	      }

	  DO_SORT (nr, ((all_elts_real
			 && real (cm (p-1, j)) > real (cm (q-1, j)))
			|| abs (cm (p-1, j)) > abs (cm (q-1, j))));

	  MATRIX_CREATE_RETURN_VALUES (cms, cm);
	}
    }

  retval(1) = idx;
  retval(0) = cms;

  return retval;
}

static octave_value_list
mx_sort (ComplexRowVector& cv)
{
  octave_value_list retval;

  int n = cv.capacity ();

  ComplexRowVector cvs (n);
  RowVector idx (n);

  if (n == 1)
    {
      retval(1) = RowVector (n, 1.0);
      retval(0) = cv;

      return retval;
    }
  else if (n > 1)
    {
      Array<int> l = create_index_array (n);

      int all_elts_real = 1;
      for (int i = 0; i < n; i++)
	if (imag (cv (i)) != 0.0)
	  {
	    all_elts_real = 0;
	    break;
	  }

      DO_SORT (n, ((all_elts_real
		    && real (cv (p-1)) > real (cv (q-1)))
		   || abs (cv (p-1)) > abs (cv (q-1))));

      VECTOR_CREATE_RETURN_VALUES (cvs, cv);
    }

  retval(1) = idx;
  retval(0) = cvs;

  return retval;
}

DEFUN_DLD (sort, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{s}, @var{i}] =} sort (@var{x})\n\
Return a copy of @var{x} with the elements elements arranged in\n\
increasing order.  For matrices, @code{sort} orders the elements in each\n\
column.\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
sort ([1, 2; 2, 3; 3, 1])\n\
     @result{}  1  1\n\
         2  2\n\
         3  3\n\
@end group\n\
@end example\n\
\n\
The @code{sort} function may also be used to produce a matrix\n\
containing the original row indices of the elements in the sorted\n\
matrix.  For example,\n\
\n\
@example\n\
@group\n\
[s, i] = sort ([1, 2; 2, 3; 3, 1])\n\
     @result{} s = 1  1\n\
            2  2\n\
            3  3\n\
     @result{} i = 1  3\n\
            2  1\n\
            3  2\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("sort");
      return retval;
    }

  int return_idx = nargout > 1;
  if (return_idx)
    retval.resize (2);
  else
    retval.resize (1);

  octave_value arg = args(0);

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  if (m.rows () == 1)
	    {
	      int nc = m.columns ();
	      RowVector v (nc);
	      for (int i = 0; i < nc; i++)
		v (i) = m (0, i);

	      retval = mx_sort (v);
	    }
	  else
	    retval = mx_sort (m);
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix cm = arg.complex_matrix_value ();

      if (! error_state)
	{
	  if (cm.rows () == 1)
	    {
	      int nc = cm.columns ();
	      ComplexRowVector cv (nc);
	      for (int i = 0; i < nc; i++)
		cv (i) = cm (0, i);

	      retval = mx_sort (cv);
	    }
	  else
	    retval = mx_sort (cm);
	}
    }
  else
    gripe_wrong_type_arg ("sort", arg);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
