// Range.cc                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#include <limits.h>

#include "Range.h"

void
Range::print_range (void)
{
  cerr << "Range: _base = " << _base
       << " _limit " << _limit
       << " _inc " << _inc
       << " _nelem " << _nelem << "\n";
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
  is >> a._base;
  if (is)
    {
      is >> a._limit;
      if (is)
	{
	  is >> a._inc;
	  a._nelem = a.nelem_internal ();
	}
    }

  return is;
}

int
Range::nelem_internal (void) const
{
// Find an approximate number of elements, then do the best we can to
// find the number of elements that we would get if we had done
// something like
//
//   nelem = 0;
//   while (base + nelem * inc <= limit)
//     nelem++;
//
// (for limit > base && inc > 0)

  double ntry = (_limit - _base) / _inc;
  double max_val = (double) INT_MAX;

  if (ntry > max_val)
    return -1;

  if (_limit > _base && _inc > 0)
    {
// Our approximation may have been too big.

      while (_base + ntry * _inc > _limit && ntry > 0)
	ntry = ntry - 1;

// Now that we are close, get the actual number.

      while (_base + ntry * _inc <= _limit && ntry <= max_val)
	ntry = ntry + 1;
    }
  else if (_limit < _base && _inc < 0)
    {
// Our approximation may have been too big.

      while (_base + ntry * _inc < _limit && ntry > 0)
	ntry = ntry - 1;

// Now that we are close, get the actual number.

      while (_base + ntry * _inc >= _limit && ntry <= max_val)
	ntry = ntry + 1;
    }
  else if (_limit == _base)
    ntry = 1;
  else
    ntry = 0;

  if (ntry > max_val)
    return -1;
  else
    return (int) ntry;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
