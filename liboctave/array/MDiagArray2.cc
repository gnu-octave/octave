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

// This file should not include config.h.  It is only included in other
// C++ source files that should have included config.h before including
// this file.

#include "MDiagArray2.h"
#include "Array-util.h"
#include "lo-error.h"

template <typename T>
bool
MDiagArray2<T>::is_multiple_of_identity (T val) const
{
  bool retval = this->rows () == this->cols ();
  if (retval)
    {
      octave_idx_type len = this->length ();
      octave_idx_type i = 0;
      for (; i < len; i++)
        if (DiagArray2<T>::elem (i, i) != val) break;
      retval = i == len;
    }

  return retval;
}

// Two dimensional diagonal array with math ops.

// Element by element MDiagArray2 by MDiagArray2 ops.

// Element by element MDiagArray2 by scalar ops.

#define MARRAY_DAS_OP(OP, FN)                                   \
  template <typename T>                                         \
  MDiagArray2<T>                                                \
  operator OP (const MDiagArray2<T>& a, const T& s)             \
  {                                                             \
    return MDiagArray2<T> (do_ms_binary_op<T, T, T> (a, s, FN), \
                           a.m_d1, a.m_d2);                     \
  }

MARRAY_DAS_OP (*, mx_inline_mul)
MARRAY_DAS_OP (/, mx_inline_div)

// Element by element scalar by MDiagArray2 ops.

template <typename T>
MDiagArray2<T>
operator * (const T& s, const MDiagArray2<T>& a)
{
  return MDiagArray2<T> (do_sm_binary_op<T, T, T> (s, a, mx_inline_mul),
                         a.m_d1, a.m_d2);
}

// Element by element MDiagArray2 by MDiagArray2 ops.

#define MARRAY_DADA_OP(FCN, OP, FN)                                     \
  template <typename T>                                                 \
  MDiagArray2<T>                                                        \
  FCN (const MDiagArray2<T>& a, const MDiagArray2<T>& b)                \
  {                                                                     \
    if (a.m_d1 != b.m_d1 || a.m_d2 != b.m_d2)                           \
      octave::err_nonconformant (#FCN, a.m_d1, a.m_d2, b.m_d1, b.m_d2); \
                                                                        \
    return MDiagArray2<T> (do_mm_binary_op<T, T, T> (a, b, FN, FN, FN, #FCN), \
                           a.m_d1, a.m_d2);                             \
  }

MARRAY_DADA_OP (operator +, +, mx_inline_add)
MARRAY_DADA_OP (operator -, -, mx_inline_sub)
MARRAY_DADA_OP (product,    *, mx_inline_mul)

// Unary MDiagArray2 ops.

template <typename T>
MDiagArray2<T>
operator + (const MDiagArray2<T>& a)
{
  return a;
}

template <typename T>
MDiagArray2<T>
operator - (const MDiagArray2<T>& a)
{
  return MDiagArray2<T> (do_mx_unary_op<T, T> (a, mx_inline_uminus),
                         a.m_d1, a.m_d2);
}
