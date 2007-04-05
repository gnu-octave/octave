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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>

#include <iostream>
#include <limits>

#include "Range.h"
#include "lo-mappers.h"
#include "lo-utils.h"

bool
Range::all_elements_are_ints (void) const
{
  // If the base and increment are ints, the final value in the range
  // will also be an integer, even if the limit is not. If there is one
  // or fewer elements only the base needs to be an integer

  return (! (xisnan (rng_base) || xisnan (rng_inc))
	  && (NINTbig (rng_base) == rng_base || rng_nelem < 1)
	  && (NINTbig (rng_inc) == rng_inc || rng_nelem <= 1));
}

Matrix
Range::matrix_value (void) const
{
  if (rng_nelem > 0 && cache.rows () == 0)
    {
      cache.resize (1, rng_nelem);
      double b = rng_base;
      double increment = rng_inc;
      for (octave_idx_type i = 0; i < rng_nelem; i++)
	cache(i) = b + i * increment;

      // On some machines (x86 with extended precision floating point
      // arithmetic, for example) it is possible that we can overshoot
      // the limit by approximately the machine precision even though
      // we were very careful in our calculation of the number of
      // elements.

      if ((rng_inc > 0 && cache(rng_nelem-1) > rng_limit)
	  || (rng_inc < 0 && cache(rng_nelem-1) < rng_limit))
	cache(rng_nelem-1) = rng_limit;
    }

  return cache;
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
	{
	  retval = rng_base + (rng_nelem - 1) * rng_inc;

	  // See the note in the matrix_value method above.

	  if (retval < rng_limit)
	    retval = rng_limit;
	}

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
	{
	  retval = rng_base + (rng_nelem - 1) * rng_inc;

	  // See the note in the matrix_value method above.

	  if (retval > rng_limit)
	    retval = rng_limit;
	}
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
      clear_cache ();
    }
}

std::ostream&
operator << (std::ostream& os, const Range& a)
{
  double b = a.base ();
  double increment = a.inc ();
  octave_idx_type num_elem = a.nelem ();

  for (octave_idx_type i = 0; i < num_elem-1; i++)
    os << b + i * increment << " ";

  // Prevent overshoot.  See comment in the matrix_value method
  // above.

  os << (increment > 0 ? a.max () : a.min ()) << "\n";

  return os;
}

std::istream&
operator >> (std::istream& is, Range& a)
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

Range
operator - (const Range& r)
{
  return Range (-r.base (), -r.limit (), -r.inc ());
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

static inline bool
teq (double u, double v, double ct = 3.0 * DBL_EPSILON)
{
  double tu = fabs (u);
  double tv = fabs (v);

  return fabs (u - v) < ((tu > tv ? tu : tv) * ct);
}

octave_idx_type
Range::nelem_internal (void) const
{
  octave_idx_type retval = -1;

  if (rng_inc == 0
      || (rng_limit > rng_base && rng_inc < 0)
      || (rng_limit < rng_base && rng_inc > 0))
    {
      retval = 0;
    }
  else
    {
      double ct = 3.0 * DBL_EPSILON;

      double tmp = tfloor ((rng_limit - rng_base + rng_inc) / rng_inc, ct);

      octave_idx_type n_elt = (tmp > 0.0 ? static_cast<octave_idx_type> (tmp) : 0);

      // If the final element that we would compute for the range is
      // equal to the limit of the range, or is an adjacent floating
      // point number, accept it.  Otherwise, try a range with one
      // fewer element.  If that fails, try again with one more
      // element.
      //
      // I'm not sure this is very good, but it seems to work better than
      // just using tfloor as above.  For example, without it, the
      // expression 1.8:0.05:1.9 fails to produce the expected result of
      // [1.8, 1.85, 1.9].

      if (! teq (rng_base + (n_elt - 1) * rng_inc, rng_limit))
	{
	  if (teq (rng_base + (n_elt - 2) * rng_inc, rng_limit))
	    n_elt--;
	  else if (teq (rng_base + n_elt * rng_inc, rng_limit))
	    n_elt++;
	}

      retval = (n_elt >= std::numeric_limits<octave_idx_type>::max () - 1) ? -1 : n_elt;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
