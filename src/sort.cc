// f-sort.cc                                           -*- C++ -*-
/*

Copyright (C) 1994, 1995 John W. Eaton

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
#include "help.h"
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
  Array<int> l (n+1);

  l.elem (0) = 1;

  for (int i = 1; i < n - 1; i++)
    l.elem (i) = -(i+2);

  l.elem (n-1) = 0;
  l.elem (n) = 0;
  l.elem (n+1) = 2;

  return l;
}

#define SORT_INIT_PHASE(n) \
  int s = 0; \
  int t = n + 1; \
  int p = l.elem (s); \
  int q = l.elem (t); \
  if (q == 0) \
     break

#define SORT_COMMON_CODE \
  p = -p; \
  q = -q; \
  if (q == 0) \
    { \
      l.elem (s) = (l.elem (s) < 0) \
	? ((p < 0) ? p : -p) \
	  : ((p >= 0) ? p : -p); \
      l.elem (t) = 0; \
      break; \
    } \

#define SORT_REORDER_PHASE_ONE \
  l.elem (s) = (l.elem (s) < 0) \
    ? ((q < 0) ? q : -q) \
      : ((q >= 0) ? q : -q); \
  s = q; \
  q = l.elem (q); \
  if (q <= 0) \
    { \
      l.elem (s) = p; \
      s = t; \
      do \
	{ \
	  t = p; \
	  p = l.elem (p); \
	} \
      while (p > 0); \
      SORT_COMMON_CODE; \
    } \

#define SORT_REORDER_PHASE_TWO \
  l.elem (s) = (l.elem (s) < 0) \
    ? ((p < 0) ? p : -p) \
      : ((p >= 0) ? p : -p); \
  s = p; \
  p = l.elem (p); \
  if (p <= 0) \
    { \
      l.elem (s) = q; \
      s = t; \
      do \
	{ \
	  t = q; \
	  q = l.elem (q); \
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
  int k = l.elem (0); \
  idx.elem (0) = k; \
  vs.elem (0) = v.elem (k-1); \
  for (int i = 1; i < n; i++) \
    { \
      k = l.elem ((int) idx.elem (i-1)); \
      idx.elem (i) = k; \
      vs.elem (i) = v.elem (k-1); \
    }

#define MATRIX_CREATE_RETURN_VALUES(ms, m) \
  int k = l.elem (0); \
  idx.elem (0, j) = k; \
  ms.elem (0, j) = m.elem (k-1, j); \
  for (int i = 1; i < nr; i++) \
    { \
      k = l.elem ((int) idx.elem (i-1, j)); \
      idx.elem (i, j) = k; \
      ms.elem (i, j) = m.elem (k-1, j); \
    }

static Octave_object
mx_sort (const Matrix& m)
{
  Octave_object retval;

  int nr = m.rows ();
  int nc = m.columns ();

  Matrix ms (nr, nc);
  Matrix idx (nr, nc);

  if (nr == 1 && nc > 0)
    {
      retval (1) = Matrix (nr, nc, 1.0);
      retval (0) = m;

      return retval;
    }
  else if (nr > 1 && nc > 0)
    {
      for (int j = 0; j < nc; j++)
	{
	  Array<int> l = create_index_array (nr);

	  DO_SORT (nr, (m.elem (p-1, j) > m.elem (q-1, j)));

	  MATRIX_CREATE_RETURN_VALUES (ms, m);
	}
    }

  retval (1) = idx;
  retval (0) = ms;

  return retval;
}

static Octave_object
mx_sort (const RowVector& v)
{
  Octave_object retval;

  int n = v.capacity ();

  RowVector vs (n);
  RowVector idx (n);

  if (n == 1)
    {
      retval (1) = RowVector (n, 1.0);
      retval (0) = v;

      return retval;
    }
  else if (n > 1)
    {
      Array<int> l = create_index_array (n);

      DO_SORT (n, (v.elem (p-1) > v.elem (q-1)));

      VECTOR_CREATE_RETURN_VALUES (vs, v);
    }

  retval (1) = tree_constant (idx, 0);
  retval (0) = tree_constant (vs, 0);

  return retval;
}

static Octave_object
mx_sort (const ComplexMatrix& cm)
{
  Octave_object retval;

  int nr = cm.rows ();
  int nc = cm.columns ();

  ComplexMatrix cms (nr, nc);
  Matrix idx (nr, nc);

  if (nr == 1 && nc > 0)
    {
      retval (1) = Matrix (nr, nc, 1.0);
      retval (0) = cm;

      return retval;
    }
  else if (nr > 1 && nc > 0)
    {
      for (int j = 0; j < nc; j++)
	{
	  Array<int> l = create_index_array (nr);

	  int all_elts_real = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (cm.elem (i, j)) != 0.0)
	      {
		all_elts_real = 0;
		break;
	      }

	  DO_SORT (nr, ((all_elts_real
			 && real (cm.elem (p-1, j)) > real (cm.elem (q-1, j)))
			|| abs (cm.elem (p-1, j)) > abs (cm.elem (q-1, j))));

	  MATRIX_CREATE_RETURN_VALUES (cms, cm);
	}
    }

  retval (1) = idx;
  retval (0) = cms;

  return retval;
}

static Octave_object
mx_sort (ComplexRowVector& cv)
{
  Octave_object retval;

  int n = cv.capacity ();

  ComplexRowVector cvs (n);
  RowVector idx (n);

  if (n == 1)
    {
      retval (1) = RowVector (n, 1.0);
      retval (0) = cv;

      return retval;
    }
  else if (n > 1)
    {
      Array<int> l = create_index_array (n);

      int all_elts_real = 1;
      for (int i = 0; i < n; i++)
	if (imag (cv.elem (i)) != 0.0)
	  {
	    all_elts_real = 0;
	    break;
	  }

      DO_SORT (n, ((all_elts_real
		    && real (cv.elem (p-1)) > real (cv.elem (q-1)))
		   || abs (cv.elem (p-1)) > abs (cv.elem (q-1))));

      VECTOR_CREATE_RETURN_VALUES (cvs, cv);
    }

  retval (1) = tree_constant (idx, 0);
  retval (0) = tree_constant (cvs, 0);

  return retval;
}

DEFUN_DLD_BUILTIN ("sort", Fsort, Ssort, FSsort, 11,
  "[S, I] = sort (X)\n\
\n\
sort the columns of X, optionally return sort index")
{
  Octave_object retval;

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

  tree_constant arg = args(0);

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
		v.elem (i) = m.elem (0, i);

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
		cv.elem (i) = cm.elem (0, i);

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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
