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
#include "ov-re-mat.h"
#include "ov-struct.h"
#include "ov-typeinfo.h"
#include "ops.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// struct ops.

DEFUNOP (transpose, struct)
{
  const octave_struct& v = dynamic_cast<const octave_struct&> (a);

  if (v.ndims () > 2)
    error ("transpose not defined for N-D objects");

  return octave_value (v.map_value ().transpose ());
}

DEFUNOP (scalar_transpose, scalar_struct)
{
  const octave_scalar_struct& v = dynamic_cast<const octave_scalar_struct&> (a);

  return octave_value (v.scalar_map_value ());
}

DEFNDCATOP_FN (s_s_concat, struct, struct, map, map, concat)
DEFNDCATOP_FN (s_ss_concat, struct, scalar_struct, map, map, concat)
DEFNDCATOP_FN (ss_s_concat, scalar_struct, struct, map, map, concat)
DEFNDCATOP_FN (ss_ss_concat, scalar_struct, scalar_struct, map, map, concat)

static octave_value
oct_catop_struct_matrix (const octave_base_value& a1,
                         const octave_base_value& a2,
                         const Array<octave_idx_type>&)
{
  const octave_struct& v1 = dynamic_cast<const octave_struct&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  NDArray tmp = v2.array_value ();
  dim_vector dv = tmp.dims ();

  if (! dv.all_zero ())
    error ("invalid concatenation of structure with matrix");

  return octave_value (v1.map_value ());
}

static octave_value
oct_catop_matrix_struct (const octave_base_value& a1,
                         const octave_base_value& a2,
                         const Array<octave_idx_type>&)
{
  const octave_matrix& v1 = dynamic_cast<const octave_matrix&> (a1);
  const octave_struct& v2 = dynamic_cast<const octave_struct&> (a2);

  NDArray tmp = v1.array_value ();
  dim_vector dv = tmp.dims ();

  if (! dv.all_zero ())
    error ("invalid concatenation of structure with matrix");

  return octave_value (v2.map_value ());
}

void
install_struct_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_transpose, octave_struct, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_struct, transpose);

  INSTALL_UNOP_TI (ti, op_transpose, octave_scalar_struct, scalar_transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_scalar_struct, scalar_transpose);

  INSTALL_CATOP_TI (ti, octave_struct, octave_struct, s_s_concat);
  INSTALL_CATOP_TI (ti, octave_struct, octave_scalar_struct, s_ss_concat)
  INSTALL_CATOP_TI (ti, octave_scalar_struct, octave_struct, ss_s_concat)
  INSTALL_CATOP_TI (ti, octave_scalar_struct, octave_scalar_struct, ss_ss_concat)

  INSTALL_CATOP_TI (ti, octave_struct, octave_matrix, struct_matrix);
  INSTALL_CATOP_TI (ti, octave_matrix, octave_struct, matrix_struct);
}

OCTAVE_END_NAMESPACE(octave)
