////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020-2023 The Octave Project Developers
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

#include <iostream>

#include "errwarn.h"
#include "ops.h"
#include "ov-magic-int.h"
#include "ov-typeinfo.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Magic integer unary ops.  Only + and - are allowed so that
// expressions like
//
//   int64 (-9007199254740994)
//
// produce proper int64 constants.

static octave_value
oct_unop_unsigned_uplus (const octave_base_value& a)
{
  const octave_magic_uint& v = dynamic_cast<const octave_magic_uint&> (a);
  // no-op.
  // FIXME: but can we do this just by incrementing the reference count?
  return octave_value (v.clone ());
}

static octave_value
oct_unop_unsigned_uminus (const octave_base_value& a)
{
  const octave_magic_uint& v = dynamic_cast<const octave_magic_uint&> (a);

  // We are storing a uint64 value, so some fakery is needed here.
  // Is there a better way?

  // FIXME: Maybe there should also be octave_magic_int::as_TYPE_value
  // functions?
  octave_uint64 val (v.scalar_ref ());

  uint64_t ival = val.value ();

  static const uint64_t max_val
    = static_cast<uint64_t> (std::numeric_limits<int64_t>::max ());

  static const uint64_t max_val_p1 = max_val + 1;

  if (ival <= max_val)
    {
      int64_t signed_ival = ival;
      return octave_value (new octave_magic_int (-signed_ival));
    }

  if (ival == max_val_p1)
    {
      // Correctly capture intmin.  For example, negating uint8(128)
      // should return int8(-128) but converting directly to int8 and
      // negating will not return the correct result.

      static const int64_t min_signed_ival
        = std::numeric_limits<int64_t>::min ();

      return octave_value (new octave_magic_int (min_signed_ival));
    }

  return octave_value (-static_cast<double> (ival));
}

static octave_value
oct_unop_signed_uplus (const octave_base_value& a)
{
  const octave_magic_int& v = dynamic_cast<const octave_magic_int&> (a);
  // no-op.
  // FIXME: but can we do this just by incrementing the reference count?
  return octave_value (v.clone ());
}

static octave_value
oct_unop_signed_uminus (const octave_base_value& a)
{
  const octave_magic_int& v = dynamic_cast<const octave_magic_int&> (a);

  // FIXME: Maybe there should also be octave_magic_int::as_TYPE_value
  // functions?
  octave_int64 val (v.scalar_ref ());

  return octave_value (new octave_magic_int (-val));
}

void
install_mi_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_uplus, octave_magic_uint, unsigned_uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_magic_uint, unsigned_uminus);

  INSTALL_UNOP_TI (ti, op_uplus, octave_magic_int, signed_uplus);
  INSTALL_UNOP_TI (ti, op_uminus, octave_magic_int, signed_uminus);
}

OCTAVE_END_NAMESPACE(octave)
