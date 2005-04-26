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

#include <iostream>

#include "FEGrid.h"
#include "lo-error.h"

// error handling

void
FEGrid::error (const char* msg) const
{
  (*current_liboctave_error_handler) ("fatal FEGrid error: %s", msg);
}

void
FEGrid::nel_error (void) const
{
  error ("number of elements less than 1");
}

// Constructors

FEGrid::FEGrid (octave_idx_type nel, double width)
{
  if (nel < 1)
    {
      nel_error ();
      return;
    }

  elem.resize (nel+1);

  for (octave_idx_type i = 0; i <= nel; i++)
    elem.elem (i) = i * width;
}

FEGrid::FEGrid (octave_idx_type nel, double l, double r)
{
  if (nel < 1)
    {
      nel_error ();
      return;
    }

  elem.resize (nel+1);

  double width = (r - l) / nel;

  for (octave_idx_type i = 0; i <= nel; i++)
    elem.elem (i) = i * width + l;

  check_grid ();
}

octave_idx_type
FEGrid::element (double x) const
{
  if (! in_bounds (x))
    {
      error ("value not within grid boundaries");
      return -1;
    }

  octave_idx_type nel = elem.capacity () - 1;
  for (octave_idx_type i = 1; i <= nel; i++)
    {
      if (x >= elem.elem (i-1) && x <= elem.elem (i))
	return i;
    }
  return -1;
       
}

void
FEGrid::check_grid (void) const
{
  octave_idx_type nel = elem.capacity () - 1;
  if (nel < 1)
    {
      nel_error ();
      return;
    }

  for (octave_idx_type i = 1; i <= nel; i++)
    {
      if (elem.elem (i-1) > elem.elem (i))
	{
	  error ("element boundaries not in ascending order");
	  return;
	}

      if (elem.elem (i-1) == elem.elem (i))
	{
	  error ("zero width element");
	  return;
	}
    }
}

std::ostream&
operator << (std::ostream& s, const FEGrid& g)
{
  s << g.element_boundaries ();
  return s;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
