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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>
#include <limits.h>

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

int
Range::nelem_internal (void) const
{
// Find an approximate number of intervals, then do the best we can to
// find the number of intervals that we would get if we had done
// something like
//
//   nelem = 0;
//   while (base + nelem * inc <= limit)
//     nelem++;
//
// (for limit > base && inc > 0)
//
// The number of elements in the range is one greater than the number
// of intervals.

// We can't have more than INT_MAX elements in the range.

  double d_n_intervals = (rng_limit - rng_base) / rng_inc;
  int max_intervals = INT_MAX - 1;
  double d_max_val = (double) max_intervals;

  if (d_n_intervals > d_max_val)
    return -1;

  int n_intervals = (d_n_intervals > 0)
    ? ((int) (d_n_intervals + 0.5))
    : ((int) (d_n_intervals - 0.5)); 

  if (rng_limit > rng_base && rng_inc > 0)
    {
// Our approximation may have been too big.

      while (rng_base + n_intervals * rng_inc > rng_limit && n_intervals > 0)
	n_intervals--;

// Now that we are close, get the actual number.  Try to avoid
// problems with extended precision registers.

      for (;;)
	{
	  volatile double tmp_inc = (n_intervals + 1) * rng_inc;
	  volatile double tmp_val = rng_base + tmp_inc;
	  if (tmp_val <= rng_limit && n_intervals < max_intervals)
	    n_intervals++;
	  else
	    break;
	}
    }
  else if (rng_limit < rng_base && rng_inc < 0)
    {
// Our approximation may have been too big.

      while (rng_base + n_intervals * rng_inc < rng_limit && n_intervals > 0)
	n_intervals--;

// Now that we are close, get the actual number.  Try to avoid
// problems with extended precision registers.

      for (;;)
	{
	  volatile double tmp_inc = (n_intervals + 1) * rng_inc;
	  volatile double tmp_val = rng_base + tmp_inc;
	  if (tmp_val >= rng_limit && n_intervals < max_intervals)
	    n_intervals++;
	  else
	    break;
	}
    }
  else if (rng_limit == rng_base)
    n_intervals = 0;
  else
    n_intervals = -1;

  return (n_intervals >= max_intervals) ? -1 : n_intervals + 1;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
