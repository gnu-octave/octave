/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005,
              2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "Bounds.h"
#include "lo-error.h"

// error handling

void
Bounds::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal bounds error: ", msg);
}

Bounds&
Bounds::set_bounds (const ColumnVector l, const ColumnVector u)
{
  if (l.capacity () != u.capacity ())
    {
      error ("inconsistent sizes for lower and upper bounds");
      return *this;
    }

  lb = l;
  ub = u;

  return *this;
}

Bounds&
Bounds::set_lower_bounds (const ColumnVector l)
{
  if (ub.capacity () != l.capacity ())
    {
      error ("inconsistent size for lower bounds");
      return *this;
    }

  lb = l;

  return *this;
}

Bounds&
Bounds::set_upper_bounds (const ColumnVector u)
{
  if (lb.capacity () != u.capacity ())
    {
      error ("inconsistent size for upper bounds");
      return *this;
    }

  ub = u;

  return *this;
}

std::ostream&
operator << (std::ostream& os, const Bounds& b)
{
  for (octave_idx_type i = 0; i < b.size (); i++)
    os << b.lower_bound (i) << " " << b.upper_bound (i) << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
