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
#include "tree-const.h"

static void
mx_sort (Matrix& m, Matrix& idx, int return_idx)
{
  int nr = m.rows ();
  int nc = m.columns ();
  idx.resize (nr, nc);
  int i, j;

  if (return_idx)
    {
      for (j = 0; j < nc; j++)
	for (i = 0; i < nr; i++)
	  idx.elem (i, j) = i+1;
    }

  for (j = 0; j < nc; j++)
    {
      for (int gap = nr/2; gap > 0; gap /= 2)
	for (i = gap; i < nr; i++)
	  for (int k = i - gap;
	       k >= 0 && m.elem (k, j) > m.elem (k+gap, j);
	       k -= gap)
	    {
	      double tmp = m.elem (k, j);
	      m.elem (k, j) = m.elem (k+gap, j);
	      m.elem (k+gap, j) = tmp;

	      if (return_idx)
		{
		  double tmp = idx.elem (k, j);
		  idx.elem (k, j) = idx.elem (k+gap, j);
		  idx.elem (k+gap, j) = tmp;
		}
	    }
    }
}

static void
mx_sort (RowVector& v, RowVector& idx, int return_idx)
{
  int n = v.capacity ();
  idx.resize (n);
  int i;

  if (return_idx)
    for (i = 0; i < n; i++)
      idx.elem (i) = i+1;

  for (int gap = n/2; gap > 0; gap /= 2)
    for (i = gap; i < n; i++)
      for (int k = i - gap;
	   k >= 0 && v.elem (k) > v.elem (k+gap);
	   k -= gap)
	{
	  double tmp = v.elem (k);
	  v.elem (k) = v.elem (k+gap);
	  v.elem (k+gap) = tmp;

	  if (return_idx)
	    {
	      double tmp = idx.elem (k);
	      idx.elem (k) = idx.elem (k+gap);
	      idx.elem (k+gap) = tmp;
	    }
	}
}

static void
mx_sort (ComplexMatrix& cm, Matrix& idx, int return_idx)
{
  int nr = cm.rows ();
  int nc = cm.columns ();
  idx.resize (nr, nc);
  int i, j;

  if (return_idx)
    {
      for (j = 0; j < nc; j++)
	for (i = 0; i < nr; i++)
	  idx.elem (i, j) = i+1;
    }

  for (j = 0; j < nc; j++)
    {
      for (int gap = nr/2; gap > 0; gap /= 2)
	for (i = gap; i < nr; i++)
	  for (int k = i - gap;
	       k >= 0 && abs (cm.elem (k, j)) > abs (cm.elem (k+gap, j));
	       k -= gap)
	    {
	      Complex ctmp = cm.elem (k, j);
	      cm.elem (k, j) = cm.elem (k+gap, j);
	      cm.elem (k+gap, j) = ctmp;

	      if (return_idx)
		{
		  double tmp = idx.elem (k, j);
		  idx.elem (k, j) = idx.elem (k+gap, j);
		  idx.elem (k+gap, j) = tmp;
		}
	    }
    }
}

static void
mx_sort (ComplexRowVector& cv, RowVector& idx, int return_idx)
{
  int n = cv.capacity ();
  idx.resize (n);
  int i;

  if (return_idx)
    for (i = 0; i < n; i++)
      idx.elem (i) = i+1;

  for (int gap = n/2; gap > 0; gap /= 2)
    for (i = gap; i < n; i++)
      for (int k = i - gap;
	   k >= 0 && abs (cv.elem (k)) > abs (cv.elem (k+gap));
	   k -= gap)
	{
	  Complex tmp = cv.elem (k);
	  cv.elem (k) = cv.elem (k+gap);
	  cv.elem (k+gap) = tmp;

	  if (return_idx)
	    {
	      double tmp = idx.elem (k);
	      idx.elem (k) = idx.elem (k+gap);
	      idx.elem (k+gap) = tmp;
	    }
	}
}

DEFUN_DLD_BUILTIN ("sort", Fsort, Ssort, 2, 2,
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
	      RowVector idx;
	      mx_sort (v, idx, return_idx);

	      retval(0) = tree_constant (v, 0);
	      if (return_idx)
		retval(1) = tree_constant (idx, 0);
	    }
	  else
	    {
	      // Sorts m in place, optionally computes index Matrix.

	      Matrix idx;
	      mx_sort (m, idx, return_idx);

	      retval(0) = m;
	      if (return_idx)
		retval(1) = idx;
	    }
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
	      RowVector idx;
	      mx_sort (cv, idx, return_idx);

	      retval(0) = tree_constant (cv, 0);
	      if (return_idx)
		retval(1) = tree_constant (idx, 0);
	    }
	  else
	    {
	      // Sorts cm in place, optionally computes index Matrix.

	      Matrix idx;
	      mx_sort (cm, idx, return_idx);

	      retval(0) = cm;
	      if (return_idx)
		retval(1) = idx;
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("sort", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
