// tc-inlines.cc                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

// Just a coupla more helper functions.

static inline int
tree_to_mat_idx (double x)
{
  if (x > 0)
    return ((int) (x + 0.5) - 1);
  else
    return ((int) (x - 0.5) - 1);
}

static inline int
range_max_check (int i, int imax)
{
  i++;
  if (i > imax)
    {
      error ("matrix index = %d exceeds maximum dimension = %d", i, imax);
      return -1;
    }
  return 0;
}

static inline int
range_max_check (int i, int j, int nr, int nc)
{
  int status = 0;
  i++;
  if (i > nr)
    {
      error ("matrix row index = %d exceeds maximum row dimension = %d",
	     i, nr);
      status = -1;
    }

  j++;
  if (j > nc)
    {
      error ("matrix column index = %d exceeds maximum column dimension = %d",
	     j, nc);
      status = -1;
    }
  return status;
}

static inline int
indexed_assign_conforms (int lhs_nr, int lhs_nc, int rhs_nr, int rhs_nc)
{
  return (lhs_nr == rhs_nr && lhs_nc == rhs_nc);
}

static inline int
is_one_zero (const Range& r)
{
  double b = r.base ();
  double l = r.limit ();
  return (r.nelem () == 2 && NINT (b) == 1 && NINT (l) == 0);
}

static inline int
is_zero_one (const Range& r)
{
  double b = r.base ();
  double l = r.limit ();
  return (r.nelem () == 2 && NINT (b) == 0 && NINT (l) == 1);
}

static inline int
index_check (int i, char *rc)
{
  if (i < 0)
    {
      error ("invalid %s index = %d", rc, i+1);
      return -1;
    }
  return 0;
}

static inline int
index_check (const Range& r, char *rc)
{
  if (r.nelem () < 1)
    {
      error ("range invalid as %s index", rc);
      return -1;
    }

  int imin = tree_to_mat_idx (r.min ());

  if (imin < 0)
    {
      error ("invalid %s index = %d", rc, imin+1);
      return -1;
    }

  return 0;
}

static inline int
fortran_row (int i, int nr)
{
  int r;
  r = i % nr;
  if (r == 0)
    r = nr;
  return r;
}

static inline int
fortran_column (int i, int nr)
{
  int c;
  int r;
  r = fortran_row (i, nr);
  c = (i - r) / nr + 1;
  return c;
}

static inline int
valid_scalar_indices (const Octave_object& args)
{
  int nargin = args.length ();
  int valid = ((nargin == 3
		&& args(2).valid_as_scalar_index ()
		&& args(1).valid_as_scalar_index ())
	       || (nargin == 2 && args(1).valid_as_scalar_index ()));

  return valid;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
