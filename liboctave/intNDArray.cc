// N-D Array  manipulations.
/*

Copyright (C) 2004 John W. Eaton

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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array-util.h"
#include "mx-base.h"
#include "lo-ieee.h"

// unary operations

template <class T>
boolNDArray
intNDArray<T>::operator ! (void) const
{
  boolNDArray b (dims ());

  for (int i = 0; i < length (); i++)
    b.elem (i) = ! elem (i);

  return b;
}

// XXX FIXME XXX -- this is not quite the right thing.

template <class T>
boolNDArray
intNDArray<T>::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (elem (iter_idx) == T (0)), true);
}

template <class T>
boolNDArray
intNDArray<T>::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (elem (iter_idx) == T (0)), false);
}

template <class T>
int
intNDArray<T>::cat (const intNDArray<T>& ra_arg, int dim, int iidx, int move)
{
  return ::cat_ra (*this, ra_arg, dim, iidx, move);  
}

template <class T>
void
intNDArray<T>::increment_index (Array<int>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

template <class T>
int 
intNDArray<T>::compute_index (Array<int>& ra_idx,
			      const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

// This contains no information on the array structure !!!

template <class T>
std::ostream&
operator << (std::ostream& os, const intNDArray<T>& a)
{
  int nel = a.nelem ();

  for (int i = 0; i < nel; i++)
    os << " " << a.elem (i) << "\n";

  return os;
}

template <class T>
std::istream&
operator >> (std::istream& is, intNDArray<T>& a)
{
  int nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      T tmp;

      for (int i = 0; i < nel; i++)
	{
	  is >> tmp;

	  if (is)
	    a.elem (i) = tmp;
	  else
	    goto done;
	}
    }

 done:

  return is;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
