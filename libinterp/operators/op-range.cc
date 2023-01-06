////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Allow +RNG_VAL to avoid conversion to array.
DEFUNOP_OP (uplus, range, /* no-op */)

CONVDECL (range_to_matrix)
{
  const octave_range& v = dynamic_cast<const octave_range&> (a);

  return new octave_matrix (v.array_value ());
}

void
install_range_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_uplus, octave_range, uplus);

  // FIXME: this would be unnecessary if
  // octave_base_value::numeric_assign always tried converting lhs
  // before rhs.

  INSTALL_ASSIGNCONV_TI (ti, octave_range, octave_null_matrix, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_range, octave_null_str, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_range, octave_null_sq_str, octave_matrix);

  // Hmm, this one also seems to be needed.

  INSTALL_WIDENOP_TI (ti, octave_range, octave_matrix, range_to_matrix);
}

OCTAVE_END_NAMESPACE(octave)
