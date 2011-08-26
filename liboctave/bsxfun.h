/*

Copyright (C) 2011 Jordi Gutiérrez Hermoso <jordigh@octave.org>

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
#if !defined (bsxfun_h)
#define bsxfun_h 1

#include <algorithm>

#include "Array.h"
#include "dim-vector.h"

inline
bool
is_valid_bsxfun (const dim_vector& dx, const dim_vector& dy)
{
  for (int i = 0; i < std::min (dx.length (), dy.length ()); i++)
    {
      if ( dx(i) > 1 && dy(i) > 1 && dx(i) != dy(i))
        return false;
    }
  return true;
}

#include "bsxfun-defs.cc"

#endif
