// f-fill.cc                                           -*- C++ -*-
/*

Copyright (C) 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "tree-const.h"
#include "user-prefs.h"
#include "utils.h"
#include "error.h"
#include "f-fill.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

static void
check_dimensions (int& nr, int& nc, const char *warnfor)
{
  if (nr < 0 || nc < 0)
    {
      if (user_pref.treat_neg_dim_as_zero)
	nr = nc = 0;
      else
	error ("%s: can't create a matrix with negative dimensions",
	       warnfor);
    }
}

static void
get_dimensions (const tree_constant& a, const char *warn_for,
		int& nr, int& nc)
{
  tree_constant tmpa = a.make_numeric ();

  if (tmpa.is_scalar_type ())
    {
      double tmp = tmpa.double_value ();
      nr = nc = NINT (tmp);
    }
  else
    {
      nr = tmpa.rows ();
      nc = tmpa.columns ();

      if ((nr == 1 && nc == 2) || (nr == 2 && nc == 1))
	{
	  ColumnVector v = tmpa.to_vector ();

	  nr = NINT (v.elem (0));
	  nc = NINT (v.elem (1));
	}
      else
	warning ("%s (A): use %s (size (A)) instead", warn_for, warn_for);
    }

  check_dimensions (nr, nc, warn_for); // May set error_state.
}

static void
get_dimensions (const tree_constant& a, const tree_constant& b,
		const char *warn_for, int& nr, int& nc)
{
  tree_constant tmpa = a.make_numeric ();
  tree_constant tmpb = b.make_numeric ();

  if (tmpa.is_scalar_type () && tmpb.is_scalar_type ())
    {
      nr = NINT (tmpa.double_value ());
      nc = NINT (tmpb.double_value ());

      check_dimensions (nr, nc, warn_for); // May set error_state.
    }
  else
    error ("%s: expecting two scalar arguments", warn_for);
}

tree_constant
fill_matrix (const tree_constant& a, double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, warn_for, nr, nc);

  if (error_state)
    return  tree_constant ();

  Matrix m (nr, nc, val);

  return m;
}

tree_constant
fill_matrix (const tree_constant& a, const tree_constant& b,
	     double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, b, warn_for, nr, nc); // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, val);

  return m;
}

tree_constant
identity_matrix (const tree_constant& a)
{
  int nr, nc;
  get_dimensions (a, "eye", nr, nc); // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      int n = MIN (nr, nc);
      for (int i = 0; i < n; i++)
	m.elem (i, i) = 1.0;
    }

  return m;
}

tree_constant
identity_matrix (const tree_constant& a, const tree_constant& b)
{
  int nr, nc;
  get_dimensions (a, b, "eye", nr, nc);  // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      int n = MIN (nr, nc);
      for (int i = 0; i < n; i++)
	m.elem (i, i) = 1.0;
    }

  return m;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
