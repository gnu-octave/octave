// Bounds.cc                                              -*- C++ -*-
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

#include <iostream.h>
#include "Bounds.h"
#include "lo-error.h"

// error handling

void
Bounds::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal bounds error: ", msg);
}

Bounds::Bounds (void)
{
  nb = 0;
}

Bounds::Bounds (int n)
{
  nb = n;
  lb.resize (nb);
  ub.resize (nb);
  lb.fill (0.0);
  ub.fill (0.0);
}

Bounds::Bounds (const ColumnVector l, const ColumnVector u)
{
  if (l.capacity () != u.capacity ())
    {
      error ("inconsistent sizes for lower and upper bounds");
      return;
    }

  nb = l.capacity ();
  lb = l;
  ub = u;
}

Bounds::Bounds (const Bounds& a)
{
  nb = a.size ();
  lb = a.lower_bounds ();
  ub = a.upper_bounds ();
}

Bounds&
Bounds::operator = (const Bounds& a)
{
  nb = a.size ();
  lb = a.lower_bounds ();
  ub = a.upper_bounds ();

  return *this;
}

Bounds&
Bounds::resize (int n)
{
  nb = n;
  lb.resize (nb);
  ub.resize (nb);

  return *this;
}

double
Bounds::lower_bound (int index) const
{
  return lb.elem (index);
}

double
Bounds::upper_bound (int index) const
{
  return ub.elem (index);
}

ColumnVector
Bounds::lower_bounds (void) const
{
  return lb;
}

ColumnVector
Bounds::upper_bounds (void) const
{
  return ub;
}

int
Bounds::size (void) const
{
  return nb;
}

Bounds&
Bounds::set_bound (int index, double low, double high)
{
  lb.elem (index) = low;
  ub.elem (index) = high;

  return *this;
}

Bounds&
Bounds::set_bounds (double low, double high)
{
  lb.fill (low);
  ub.fill (high);

  return *this;
}

Bounds&
Bounds::set_bounds (const ColumnVector l, const ColumnVector u)
{
  if (l.capacity () != u.capacity ())
    {
      error ("inconsistent sizes for lower and upper bounds");
      return *this;
    }

  nb = l.capacity ();
  lb = l;
  ub = u;

  return *this;
}

Bounds&
Bounds::set_lower_bound (int index, double low)
{
  lb.elem (index) = low;

  return *this;
}

Bounds&
Bounds::set_upper_bound (int index, double high)
{
  ub.elem (index) = high;

  return *this;
}

Bounds&
Bounds::set_lower_bounds (double low)
{
  lb.fill (low);

  return *this;
}

Bounds&
Bounds::set_upper_bounds (double high)
{
  ub.fill (high);

  return *this;
}

Bounds&
Bounds::set_lower_bounds (const ColumnVector l)
{
  if (nb != l.capacity ())
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
  if (nb != u.capacity ())
    {
      error ("inconsistent size for upper bounds");
      return *this;
    }

  ub = u;

  return *this;
}

ostream&
operator << (ostream& os, const Bounds& b)
{
  for (int i = 0; i < b.size (); i++)
    os << b.lower_bound (i) << " " << b.upper_bound (i) << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
