////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#include "lo-error.h"
#include "oct-sparse.h"

#if (defined (HAVE_AMD) || defined (HAVE_CCOLAMD)               \
     || defined (HAVE_CHOLMOD) || defined (HAVE_COLAMD)         \
     || defined (HAVE_CXSPARSE) || defined (HAVE_UMFPACK))

OCTAVE_BEGIN_NAMESPACE(octave)

static inline void
check_suitesparse_integer_size (void)
{
  // FIXME: maybe it would be better to make this a configure check and
  // disable suitesparse if it fails?

  if (sizeof (suitesparse_integer) != sizeof (octave_idx_type))
    (*current_liboctave_error_handler)
      ("size of suitesparse integer does not match octave_idx_type!");
}

suitesparse_integer *
to_suitesparse_intptr (octave_idx_type *i)
{
  check_suitesparse_integer_size ();

  return reinterpret_cast<suitesparse_integer *> (i);
}

const suitesparse_integer *
to_suitesparse_intptr (const octave_idx_type *i)
{
  check_suitesparse_integer_size ();

  return reinterpret_cast<const suitesparse_integer *> (i);
}

octave_idx_type *
to_octave_idx_type_ptr (suitesparse_integer *i)
{
  check_suitesparse_integer_size ();

  return reinterpret_cast<octave_idx_type *> (i);
}

const octave_idx_type *
to_octave_idx_type_ptr (const suitesparse_integer *i)
{
  check_suitesparse_integer_size ();

  return reinterpret_cast<const octave_idx_type *> (i);
}

OCTAVE_END_NAMESPACE(octave)

#endif
