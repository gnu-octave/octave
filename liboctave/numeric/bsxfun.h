////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_bsxfun_h)
#define octave_bsxfun_h 1

#include "octave-config.h"

#include <algorithm>
#include <string>

#include "dim-vector.h"
#include "lo-error.h"

inline
bool
is_valid_bsxfun (const std::string& name,
                 const dim_vector& xdv, const dim_vector& ydv)
{
  for (int i = 0; i < std::min (xdv.ndims (), ydv.ndims ()); i++)
    {
      octave_idx_type xk = xdv(i);
      octave_idx_type yk = ydv(i);
      // Check the three conditions for valid bsxfun dims
      if (! ((xk == yk) || (xk == 1 && yk != 1) || (xk != 1 && yk == 1)))
        return false;
    }

  (*current_liboctave_warning_with_id_handler)
    ("Octave:language-extension", "performing '%s' automatic broadcasting",
     name.c_str ());

  return true;
}

// For inplace operations the size of the resulting matrix cannot be changed.
// Therefore we can only apply singleton expansion on the second matrix which
// alters the conditions to check.
inline
bool
is_valid_inplace_bsxfun (const std::string& name,
                         const dim_vector& rdv, const dim_vector& xdv)
{
  octave_idx_type r_nd = rdv.ndims ();
  octave_idx_type x_nd = xdv.ndims ();
  if (r_nd < x_nd)
    return false;

  for (int i = 0; i < x_nd; i++)
    {
      octave_idx_type rk = rdv(i);
      octave_idx_type xk = xdv(i);

      // Only two valid conditions to check; can't stretch rk
      if ((rk != xk) && xk != 1)
        return false;
    }

  (*current_liboctave_warning_with_id_handler)
    ("Octave:language-extension", "performing '%s' automatic broadcasting",
     name.c_str ());

  return true;
}

#include "bsxfun-defs.cc"

#endif
