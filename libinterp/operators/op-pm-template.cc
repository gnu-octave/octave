////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

// FIXME: it might be nice to only include the declarations of the
// operators that are actually needed instead of including all of them.
#include "mx-ops.h"

#include "ov-perm.h"
#include MINCLUDE
#include "ops.h"
#if defined (DEFINENULLASSIGNCONV)
#  include "ov-null-mat.h"
#endif

#if ! defined (LDMATRIX)
#  define LDMATRIX LMATRIX
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#define OCTAVE_LMATRIX CONCAT2(octave_, LMATRIX)
#define OCTAVE_LDMATRIX CONCAT2(octave_, LDMATRIX)
#define OCTAVE_RMATRIX CONCAT2(octave_, RMATRIX)
#if defined (LEFT)
#  define LMATRIX_VALUE perm_matrix_value
#  define RMATRIX_VALUE CONCAT2(RMATRIX, _value)
#else
#  define LMATRIX_VALUE CONCAT2(LMATRIX, _value)
#  define RMATRIX_VALUE perm_matrix_value
#endif

DEFBINOP (mul, LMATRIX, RMATRIX)
{
  const OCTAVE_LMATRIX& v1 = dynamic_cast<const OCTAVE_LMATRIX&> (a1);
  const OCTAVE_RMATRIX& v2 = dynamic_cast<const OCTAVE_RMATRIX&> (a2);

  return v1.LMATRIX_VALUE () * v2.RMATRIX_VALUE ();
}

#if defined (LEFT)
DEFBINOP (ldiv, LMATRIX, RMATRIX)
{
  const OCTAVE_LMATRIX& v1 = dynamic_cast<const OCTAVE_LMATRIX&> (a1);
  const OCTAVE_RMATRIX& v2 = dynamic_cast<const OCTAVE_RMATRIX&> (a2);

  return v1.perm_matrix_value ().inverse () * v2.RMATRIX_VALUE ();
}
#else
DEFBINOP (div, LMATRIX, RMATRIX)
{
  const OCTAVE_LMATRIX& v1 = dynamic_cast<const OCTAVE_LMATRIX&> (a1);
  const OCTAVE_RMATRIX& v2 = dynamic_cast<const OCTAVE_RMATRIX&> (a2);

  return v1.LMATRIX_VALUE () * v2.perm_matrix_value ().inverse ();
}
#endif

#define SHORT_NAME CONCAT3(LSHORT, _, RSHORT)
#define INST_NAME CONCAT3(install_, SHORT_NAME, _ops)

void
INST_NAME (octave::type_info& ti)
{
  INSTALL_BINOP_TI (ti, op_mul, OCTAVE_LMATRIX, OCTAVE_RMATRIX, mul);
#if defined (LEFT)
  INSTALL_BINOP_TI (ti, op_ldiv, OCTAVE_LMATRIX, OCTAVE_RMATRIX, ldiv);
#else
  INSTALL_BINOP_TI (ti, op_div, OCTAVE_LMATRIX, OCTAVE_RMATRIX, div);
#endif
#if defined (DEFINENULLASSIGNCONV)
  INSTALL_ASSIGNCONV_TI (ti, OCTAVE_LMATRIX, octave_null_matrix, OCTAVE_LDMATRIX);
  INSTALL_ASSIGNCONV_TI (ti, OCTAVE_LMATRIX, octave_null_str, OCTAVE_LDMATRIX);
  INSTALL_ASSIGNCONV_TI (ti, OCTAVE_LMATRIX, octave_null_sq_str, OCTAVE_LDMATRIX);
#endif
}

OCTAVE_END_NAMESPACE(octave)
