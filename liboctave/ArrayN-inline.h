// Inline functions used by ArrayN-idx.h and ArrayN.cc
/*

Copyright (C) 2000 John W. Eaton

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

static inline bool
index_in_bounds (const Array<int>& ra_idx, const Array<int>& dimensions)
{
  bool retval = true;

  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || ra_idx(i) >= dimensions (i))
	    {
	      retval = false;
	      break;
	    }
	}
    }
  else
    retval = false;

  return retval;
}

static inline void
increment_index (Array<int>& ra_idx, const Array<int>& dimensions,
		 int start_dimension = 0)
{
  ra_idx(start_dimension)++;

  int n = ra_idx.length () - 1;

  for (int i = start_dimension; i < n; i++)
    {
      if (ra_idx(i) < dimensions(i))
 	break;
      else
 	{
 	  ra_idx(i) = 0;
 	  ra_idx(i+1)++;
 	}
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
