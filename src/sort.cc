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

// This is algorithm 5.2.4L from Knuth, Volume 3.

// XXX FIXME XXX -- there is way too much duplicated code here given
// that the sort algorithms are all the same, and only the type of the
// data and the comparison changes...

static Octave_object
mx_sort (const Matrix& m)
{
  Octave_object retval;

  int nr = m.rows ();
  int nc = m.columns ();

  Matrix ms (nr, nc);
  Matrix idx (nr, nc);

  if (nr > 0 && nc > 0)
    {
      for (int j = 0; j < nc; j++)
	{
	  Array<int> l (nr+2);

	  l (0) = 1;
	  for (int i = 1; i < nr - 1; i++)
	    l (i) = -(i+2);
	  l (nr-1) = 0;
	  l (nr) = 0;
	  l (nr+1) = 2;

	  while (1)
	    {
	      int s = 0;
	      int t = nr + 1;
	      int p = l (s);
	      int q = l (t);

	      if (q == 0)
		break;

	      while (1)
		{
		  if (m (p-1, j) > m (q-1, j))
		    {
		      l (s) = (l (s) < 0)
			? ((q < 0) ? q : -q)
			  : ((q >= 0) ? q : -q);
		      s = q;
		      q = l (q);
		      if (q <= 0)
			{
			  l (s) = p;
			  s = t;
			  do
			    {
			      t = p;
			      p = l (p);
			    }
			  while (p > 0);
			  p = -p;
			  q = -q;
			  if (q == 0)
			    {
			      l (s) = (l (s) < 0)
				? ((p < 0) ? p : -p)
				  : ((p >= 0) ? p : -p);
			      l (t) = 0;
			      break;
			    }
			}
		    }
		  else
		    {
		      l (s) = (l (s) < 0)
			? ((p < 0) ? p : -p)
			  : ((p >= 0) ? p : -p);
		      s = p;
		      p = l (p);
		      if (p <= 0)
			{
			  l (s) = q;
			  s = t;
			  do
			    {
			      t = q;
			      q = l (q);
			    }
			  while (q > 0);
			  p = -p;
			  q = -q;
			  if (q == 0)
			    {
			      l (s) = (l (s) < 0)
				? ((p < 0) ? p : -p)
				  : ((p >= 0) ? p : -p);
			      l (t) = 0;
			      break;
			    }		      
			}
		    }
		}
	    }

	  int k = l (0);
	  idx (0, j) = k;
	  ms (0, j) = m (k-1, j);
	  for (int i = 1; i < nr; i++)
	    {
	      k = l ((int) idx (i-1, j));
	      idx (i, j) = k;
	      ms (i, j) = m (k-1, j);
	    }
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

  if (n > 0)
    {
      Array<int> l (n+2);

      l (0) = 1;
      for (int i = 1; i < n - 1; i++)
	l (i) = -(i+2);
      l (n-1) = 0;
      l (n) = 0;
      l (n+1) = 2;

      while (1)
	{
	  int s = 0;
	  int t = n + 1;
	  int p = l (s);
	  int q = l (t);

	  if (q == 0)
	    break;

	  while (1)
	    {
	      if (v (p-1) > v (q-1))
		{
		  l (s) = (l (s) < 0)
		    ? ((q < 0) ? q : -q)
		      : ((q >= 0) ? q : -q);
		  s = q;
		  q = l (q);
		  if (q <= 0)
		    {
		      l (s) = p;
		      s = t;
		      do
			{
			  t = p;
			  p = l (p);
			}
		      while (p > 0);
		      p = -p;
		      q = -q;
		      if (q == 0)
			{
			  l (s) = (l (s) < 0)
			    ? ((p < 0) ? p : -p)
			      : ((p >= 0) ? p : -p);
			  l (t) = 0;
			  break;
			}
		    }
		}
	      else
		{
		  l (s) = (l (s) < 0)
		    ? ((p < 0) ? p : -p)
		      : ((p >= 0) ? p : -p);
		  s = p;
		  p = l (p);
		  if (p <= 0)
		    {
		      l (s) = q;
		      s = t;
		      do
			{
			  t = q;
			  q = l (q);
			}
		      while (q > 0);
		      p = -p;
		      q = -q;
		      if (q == 0)
			{
			  l (s) = (l (s) < 0)
			    ? ((p < 0) ? p : -p)
			      : ((p >= 0) ? p : -p);
			  l (t) = 0;
			  break;
			}		      
		    }
		}
	    }
	}

      int k = l (0);
      idx (0) = k;
      vs (0) = v (k-1);
      for (int i = 1; i < n; i++)
	{
	  k = l ((int) idx (i-1));
	  idx (i) = k;
	  vs (i) = v (k-1);
	}
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

  if (nr > 0 && nc > 0)
    {
      for (int j = 0; j < nc; j++)
	{
	  Array<int> l (nr+2);

	  l (0) = 1;
	  for (int i = 1; i < nr - 1; i++)
	    l (i) = -(i+2);
	  l (nr-1) = 0;
	  l (nr) = 0;
	  l (nr+1) = 2;

	  int all_elts_real = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (cm (i, j)) != 0.0)
	      {
		all_elts_real = 0;
		break;
	      }

	  while (1)
	    {
	      int s = 0;
	      int t = nr + 1;
	      int p = l (s);
	      int q = l (t);

	      if (q == 0)
		break;

	      while (1)
		{
		  if ((all_elts_real
		       && real (cm (p-1, j)) > real (cm (q-1, j)))
		      || abs (cm (p-1, j)) > abs (cm (q-1, j)))
		    {
		      l (s) = (l (s) < 0)
			? ((q < 0) ? q : -q)
			  : ((q >= 0) ? q : -q);
		      s = q;
		      q = l (q);
		      if (q <= 0)
			{
			  l (s) = p;
			  s = t;
			  do
			    {
			      t = p;
			      p = l (p);
			    }
			  while (p > 0);
			  p = -p;
			  q = -q;
			  if (q == 0)
			    {
			      l (s) = (l (s) < 0)
				? ((p < 0) ? p : -p)
				  : ((p >= 0) ? p : -p);
			      l (t) = 0;
			      break;
			    }
			}
		    }
		  else
		    {
		      l (s) = (l (s) < 0)
			? ((p < 0) ? p : -p)
			  : ((p >= 0) ? p : -p);
		      s = p;
		      p = l (p);
		      if (p <= 0)
			{
			  l (s) = q;
			  s = t;
			  do
			    {
			      t = q;
			      q = l (q);
			    }
			  while (q > 0);
			  p = -p;
			  q = -q;
			  if (q == 0)
			    {
			      l (s) = (l (s) < 0)
				? ((p < 0) ? p : -p)
				  : ((p >= 0) ? p : -p);
			      l (t) = 0;
			      break;
			    }		      
			}
		    }
		}
	    }

	  int k = l (0);
	  idx (0, j) = k;
	  cms (0, j) = cm (k-1, j);
	  for (int i = 1; i < nr; i++)
	    {
	      k = l ((int) idx (i-1, j));
	      idx (i, j) = k;
	      cms (i, j) = cm (k-1, j);
	    }
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

  if (n > 0)
    {
      Array<int> l (n+2);

      l (0) = 1;
      for (int i = 1; i < n - 1; i++)
	l (i) = -(i+2);
      l (n-1) = 0;
      l (n) = 0;
      l (n+1) = 2;

      int all_elts_real = 1;
      for (int i = 0; i < n; i++)
	if (imag (cv (i)) != 0.0)
	  {
	    all_elts_real = 0;
	    break;
	  }

      while (1)
	{
	  int s = 0;
	  int t = n + 1;
	  int p = l (s);
	  int q = l (t);

	  if (q == 0)
	    break;

	  while (1)
	    {
	      if ((all_elts_real && real (cv (p-1)) > real (cv (q-1)))
		  || abs (cv (p-1)) > abs (cv (q-1)))
		{
		  l (s) = (l (s) < 0)
		    ? ((q < 0) ? q : -q)
		      : ((q >= 0) ? q : -q);
		  s = q;
		  q = l (q);
		  if (q <= 0)
		    {
		      l (s) = p;
		      s = t;
		      do
			{
			  t = p;
			  p = l (p);
			}
		      while (p > 0);
		      p = -p;
		      q = -q;
		      if (q == 0)
			{
			  l (s) = (l (s) < 0)
			    ? ((p < 0) ? p : -p)
			      : ((p >= 0) ? p : -p);
			  l (t) = 0;
			  break;
			}
		    }
		}
	      else
		{
		  l (s) = (l (s) < 0)
		    ? ((p < 0) ? p : -p)
		      : ((p >= 0) ? p : -p);
		  s = p;
		  p = l (p);
		  if (p <= 0)
		    {
		      l (s) = q;
		      s = t;
		      do
			{
			  t = q;
			  q = l (q);
			}
		      while (q > 0);
		      p = -p;
		      q = -q;
		      if (q == 0)
			{
			  l (s) = (l (s) < 0)
			    ? ((p < 0) ? p : -p)
			      : ((p >= 0) ? p : -p);
			  l (t) = 0;
			  break;
			}		      
		    }
		}
	    }
	}

      int k = l (0);
      idx (0) = k;
      cvs (0) = cv (k-1);
      for (int i = 1; i < n; i++)
	{
	  k = l ((int) idx (i-1));
	  idx (i) = k;
	  cvs (i) = cv (k-1);
	}
    }

  retval (1) = tree_constant (idx, 0);
  retval (0) = tree_constant (cvs, 0);

  return retval;
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
