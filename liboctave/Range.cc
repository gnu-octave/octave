// Range.cc                                              -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <climits>
#include <cmath>

#include <iostream.h>

#include "Range.h"
#include "dMatrix.h"

Matrix
Range::matrix_value (void) const
{
  Matrix retval;

  if (rng_nelem > 0)
    {
      retval.resize (1, rng_nelem);
      double b = rng_base;
      double increment = rng_inc;
      for (int i = 0; i < rng_nelem; i++)
	retval.elem (0, i) = b + i * increment;
    }

  return retval;
}

// NOTE: max and min only return useful values if nelem > 0.

double
Range::min (void) const
{
  double retval = 0.0;
  if (rng_nelem > 0)
    {
      if (rng_inc > 0)
	retval = rng_base;
      else
	retval = rng_base + (rng_nelem - 1) * rng_inc;
    }
  return retval;
}

double
Range::max (void) const
{
  double retval = 0.0;
  if (rng_nelem > 0)
    {
      if (rng_inc > 0)
	retval = rng_base + (rng_nelem - 1) * rng_inc;
      else
	retval = rng_base;
    }
  return retval;
}

void
Range::sort (void)
{
  if (rng_base > rng_limit && rng_inc < 0.0)
    {
      double tmp = rng_base;
      rng_base = min ();
      rng_limit = tmp;
      rng_inc = -rng_inc;
    }
}

void
Range::print_range (void)
{
  cerr << "Range: rng_base = " << rng_base
       << " rng_limit " << rng_limit
       << " rng_inc " << rng_inc
       << " rng_nelem " << rng_nelem << "\n";
}

ostream&
operator << (ostream& os, const Range& a)
{
  double b = a.base ();
  double increment = a.inc ();
  int num_elem = a.nelem ();

  for (int i = 0; i < num_elem; i++)
    os << b + i * increment << " ";

  os << "\n";

  return os;
}

istream&
operator >> (istream& is, Range& a)
{
  is >> a.rng_base;
  if (is)
    {
      is >> a.rng_limit;
      if (is)
	{
	  is >> a.rng_inc;
	  a.rng_nelem = a.nelem_internal ();
	}
    }

  return is;
}

// C  See Knuth, Art Of Computer Programming, Vol. 1, Problem 1.2.4-5.
// C
// C===Tolerant FLOOR function.
// C
// C    X  -  is given as a Double Precision argument to be operated on.
// C          It is assumed that X is represented with M mantissa bits.
// C    CT -  is   given   as   a   Comparison   Tolerance   such   that
// C          0.LT.CT.LE.3-SQRT(5)/2. If the relative difference between
// C          X and A whole number is  less  than  CT,  then  TFLOOR  is
// C          returned   as   this   whole   number.   By  treating  the
// C          floating-point numbers as a finite ordered set  note  that
// C          the  heuristic  EPS=2.**(-(M-1))   and   CT=3*EPS   causes
// C          arguments  of  TFLOOR/TCEIL to be treated as whole numbers
// C          if they are  exactly  whole  numbers  or  are  immediately
// C          adjacent to whole number representations.  Since EPS,  the
// C          "distance"  between  floating-point  numbers  on  the unit
// C          interval, and M, the number of bits in X'S mantissa, exist
// C          on  every  floating-point   computer,   TFLOOR/TCEIL   are
// C          consistently definable on every floating-point computer.
// C
// C          For more information see the following references:
// C    (1) P. E. Hagerty, "More On Fuzzy Floor And Ceiling," APL  QUOTE
// C        QUAD 8(4):20-24, June 1978. Note that TFLOOR=FL5.
// C    (2) L. M. Breed, "Definitions For Fuzzy Floor And Ceiling",  APL
// C        QUOTE QUAD 8(3):16-23, March 1978. This paper cites FL1 through
// C        FL5, the history of five years of evolutionary development of
// C        FL5 - the seven lines of code below - by open collaboration
// C        and corroboration of the mathematical-computing community.
// C
// C  Penn State University Center for Academic Computing
// C  H. D. Knoble - August, 1978.

static inline double
tfloor (double x, double ct)
{
// C---------FLOOR(X) is the largest integer algebraically less than
// C         or equal to X; that is, the unfuzzy FLOOR function.

//  DINT (X) = X - DMOD (X, 1.0);
//  FLOOR (X) = DINT (X) - DMOD (2.0 + DSIGN (1.0, X), 3.0);

// C---------Hagerty's FL5 function follows...

  double q = 1.0;

  if (x < 0.0)
    q = 1.0 - ct;

  double rmax = q / (2.0 - ct);

  double t1 = 1.0 + floor (x);
  t1 = (ct / q) * (t1 < 0.0 ? -t1 : t1);
  t1 = rmax < t1 ? rmax : t1;
  t1 = ct > t1 ? ct : t1;
  t1 = floor (x + t1);

  if (x <= 0.0 || (t1 - x) < rmax)
    return t1;
  else
    return t1 - 1.0;
}

static inline double
tceil (double x, double ct)
{
  return -tfloor (-x, ct);
}

static inline double
round (double x, double ct)
{
  return tfloor (x+0.5, ct);
}

int
Range::nelem_internal (void) const
{
  double ct = 3.0 * DBL_EPSILON;

  double tmp = tfloor ((rng_limit - rng_base + rng_inc) / rng_inc, ct);

  int n_intervals = (int) (tmp > 0.0 ? tmp : 0);

  return (n_intervals >= INT_MAX - 1) ? -1 : n_intervals;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
