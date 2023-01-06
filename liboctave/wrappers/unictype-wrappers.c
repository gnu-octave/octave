////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "unictype.h"

#include "unictype-wrappers.h"

bool
octave_uc_is_alnum_wrapper (ucs4_t uc)
{
  return uc_is_alnum (uc);
}

bool
octave_uc_is_alpha_wrapper (ucs4_t uc)
{
  return uc_is_alpha (uc);
}

bool
octave_uc_is_blank_wrapper (ucs4_t uc)
{
  return uc_is_blank (uc);
}

bool
octave_uc_is_cntrl_wrapper (ucs4_t uc)
{
  return uc_is_cntrl (uc);
}

bool
octave_uc_is_digit_wrapper (ucs4_t uc)
{
  return uc_is_digit (uc);
}

bool
octave_uc_is_graph_wrapper (ucs4_t uc)
{
  return uc_is_graph (uc);
}

bool
octave_uc_is_lower_wrapper (ucs4_t uc)
{
  return uc_is_lower (uc);
}

bool
octave_uc_is_print_wrapper (ucs4_t uc)
{
  return uc_is_print (uc);
}

bool
octave_uc_is_punct_wrapper (ucs4_t uc)
{
  return uc_is_punct (uc);
}

bool
octave_uc_is_space_wrapper (ucs4_t uc)
{
  return uc_is_space (uc);
}

bool
octave_uc_is_upper_wrapper (ucs4_t uc)
{
  return uc_is_upper (uc);
}

bool
octave_uc_is_xdigit_wrapper (ucs4_t uc)
{
  return uc_is_xdigit (uc);
}
