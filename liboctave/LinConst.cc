// LinConst.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include "LinConst.h"
#include "lo-error.h"

// error handling

void
LinConst::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal LinConst error: %s", msg);
}

LinConst::LinConst (const Matrix& a_eq, const Vector& b_eq,
		    const Matrix& a_ineq, const Vector& b_ineq)
{
// Need some checks here.

  int nc_eq = b_eq.capacity ();
  int nc_ineq = b_ineq.capacity ();
  nb = nc_eq + nc_ineq;

  lb.resize (nb);
  ub.resize (nb);

  lb.insert (b_eq, 0);
  lb.insert (-b_ineq, nc_eq);

  ub.insert (b_eq, 0);
  ub.fill (DBL_MAX, nc_eq, nb-1);

  int nx = a_eq.columns ();

  A.resize (nb, nx);

  A.insert (a_eq, 0, 0);
  A.insert (a_ineq, nc_eq, 0);
}

LinConst&
LinConst::resize (int nc, int n)
{
  nb = nc;
  lb.resize (nb);
  A.resize (nb, n);
  ub.resize (nb);

  return *this;
}

Matrix
LinConst::eq_constraint_matrix (void) const
{
  int anr = A.rows ();
  int anc = A.columns ();
  Matrix retval (anr, anc);
  int count = 0;
  for (int i = 0; i < anr; i++)
    {
      if (lb.elem (i) == ub.elem (i))
	{
	  retval.insert (A.extract (i, 0, i, anc-1), count, 0);
	  count++;
	}
    }
  retval.resize (count, anc);
  return retval;
}

Matrix
LinConst::ineq_constraint_matrix (void) const
{
  int anr = A.rows ();
  int anc = A.columns ();
  Matrix retval (2*anr, anc);
  int count = 0;
  for (int i = 0; i < anr; i++)
    {
      if (lb.elem (i) != ub.elem (i))
	{
	  Matrix tmp = A.extract (i, 0, i, anc-1);
	  retval.insert (tmp, count, 0);
	  count++;
	  if (ub.elem (i) < DBL_MAX)
	    {
	      retval.insert (-tmp, count, 0);
	      count++;
	    }
	}
    }
  retval.resize (count, anc);
  return retval;
}

Vector
LinConst::eq_constraint_vector (void) const
{
  Vector retval (nb);
  int count = 0;
  for (int i = 0; i < nb; i++)
    {
      if (lb.elem (i) == ub.elem (i))
	{
	  retval.elem (count) = lb.elem (i);
	  count++;
	}
    }
  retval.resize (count);
  return retval;
}

Vector
LinConst::ineq_constraint_vector (void) const
{
  Vector retval (2*nb);
  int count = 0;
  for (int i = 0; i < nb; i++)
    {
      if (lb.elem (i) != ub.elem (i))
	{
	  retval.elem (count) = -lb.elem (i);
	  count++;
	  if (ub.elem (i) < DBL_MAX)
	    {
	      retval.elem (count) = ub.elem (i);
	      count++;
	    }
	}
    }
  retval.resize (count);
  return retval;
}

ostream&
operator << (ostream& os, const LinConst& c)
{
  for (int i = 0; i < c.size (); i++)
    os << c.lower_bound (i) << " " << c.upper_bound (i) << "\n";

  os << "\n";
  os << c.constraint_matrix ();

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
