////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2026 The Octave Project Developers
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

#include "ops.h"
#include "xpow.h"
#include SINCLUDE
#include MINCLUDE

// matrix by diag matrix ops.

#if ! defined (SCALARV)
#  define SCALARV SCALAR
#endif

#if ! defined (DIAG_MATRIXV)
#  define DIAG_MATRIXV DIAG_MATRIX
#endif

#if ! defined (MATRIXV)
#  define MATRIXV MATRIX
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#define OCTAVE_DIAG_MATRIX CONCAT2(octave_, DIAG_MATRIX)
#define OCTAVE_MATRIX CONCAT2(octave_, MATRIX)
#define OCTAVE_SCALAR CONCAT2(octave_, SCALAR)
#define DIAG_MATRIX_VALUE CONCAT2(DIAG_MATRIXV, _value)
#define MATRIX_VALUE CONCAT2(MATRIXV, _value)
#define SCALAR_VALUE CONCAT2(SCALARV, _value)

template <typename DM, typename S>
octave_value
dm_s_mul (const DM& dm, const S& d)
{
  if constexpr (is_instance<std::complex, S>::value)
    {
      if (math::isfinite (std::norm (d)))
        return octave_value (dm.DIAG_MATRIX_VALUE () * d);
      else
        return octave_value (dm.MATRIX_VALUE () * d);
    }
  else
    {
      if (math::isfinite (d))
        return octave_value (dm.DIAG_MATRIX_VALUE () * d);
      else
        return octave_value (dm.MATRIX_VALUE () * d);
    }
}

DEFBINOP (dmsmul, DIAG_MATRIX, SCALAR)
{
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_DIAG_MATRIX&, v1, a1);
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_SCALAR&, v2, a2);

  return dm_s_mul<> (v1, v2.SCALAR_VALUE ());
}

DEFBINOP (dmsdiv, MATRIX, SCALAR)
{
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_DIAG_MATRIX&, v1, a1);
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_SCALAR&, v2, a2);

  return v1.DIAG_MATRIX_VALUE () / v2.SCALAR_VALUE ();
}

template <typename S, typename DM>
octave_value
s_dm_mul (const S& d, const DM& dm)
{
  if constexpr (is_instance<std::complex, S>::value)
    {
      if (math::isfinite (std::norm (d)))
        return octave_value (d * dm.DIAG_MATRIX_VALUE ());
      else
        return octave_value (d * dm.MATRIX_VALUE ());
    }
  else
    {
      if (math::isfinite (d))
        return octave_value (d * dm.DIAG_MATRIX_VALUE ());
      else
        return octave_value (d * dm.MATRIX_VALUE ());
    }
}

DEFBINOP (sdmmul, SCALAR, DIAG_MATRIX)
{
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_SCALAR&, v1, a1);
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_DIAG_MATRIX&, v2, a2);

  return s_dm_mul<> (v1.SCALAR_VALUE (), v2);
}

DEFBINOP (sdmldiv, SCALAR, MATRIX)
{
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_SCALAR&, v1, a1);
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_DIAG_MATRIX&, v2, a2);

  return v2.DIAG_MATRIX_VALUE () / v1.SCALAR_VALUE ();
}

DEFBINOP (dmspow, MATRIX, SCALAR)
{
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_DIAG_MATRIX&, v1, a1);
  OCTAVE_CAST_BASE_VALUE (const OCTAVE_SCALAR&, v2, a2);

  return xpow (v1.DIAG_MATRIX_VALUE (), v2.SCALAR_VALUE ());
}

#define SHORT_NAME CONCAT3(MSHORT, _, SSHORT)
#define INST_NAME CONCAT3(install_, SHORT_NAME, _ops)

void
INST_NAME (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_mul, OCTAVE_DIAG_MATRIX, OCTAVE_SCALAR, dmsmul);
  INSTALL_BINOP_TI (ti, op_div, OCTAVE_DIAG_MATRIX, OCTAVE_SCALAR, dmsdiv);
  INSTALL_BINOP_TI (ti, op_mul, OCTAVE_SCALAR, OCTAVE_DIAG_MATRIX, sdmmul);
  INSTALL_BINOP_TI (ti, op_ldiv, OCTAVE_SCALAR, OCTAVE_DIAG_MATRIX, sdmldiv);
  INSTALL_BINOP_TI (ti, op_pow, OCTAVE_DIAG_MATRIX, OCTAVE_SCALAR, dmspow);
}

OCTAVE_END_NAMESPACE(octave)
