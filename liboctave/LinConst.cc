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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "LinConst.h"
#include "lo-error.h"

// error handling

void
LinConst::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal LinConst error: %s", msg);
}

std::ostream&
operator << (std::ostream& os, const LinConst& c)
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
;;; End: ***
*/
