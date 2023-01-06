////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2023 The Octave Project Developers
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
#include "ov-fcn-handle.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFBINOP (eq, fcn_handle, fcn_handle)
{
  const octave_fcn_handle& v1 = dynamic_cast<const octave_fcn_handle&> (a1);
  const octave_fcn_handle& v2 = dynamic_cast<const octave_fcn_handle&> (a2);

  return is_equal_to (v1, v2);
}

DEFBINOP (ne, fcn_handle, fcn_handle)
{
  const octave_fcn_handle& v1 = dynamic_cast<const octave_fcn_handle&> (a1);
  const octave_fcn_handle& v2 = dynamic_cast<const octave_fcn_handle&> (a2);

  return ! is_equal_to (v1, v2);
}

void
install_fcn_ops (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_eq, octave_fcn_handle, octave_fcn_handle, eq);
  INSTALL_BINOP_TI (ti, op_ne, octave_fcn_handle, octave_fcn_handle, ne);
}

OCTAVE_END_NAMESPACE(octave)
