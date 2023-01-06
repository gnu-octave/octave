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

#include <cassert>

#include <algorithm>

#include "DiagArray2.h"

#include "lo-error.h"

template <typename T>
DiagArray2<T>::DiagArray2 (const Array<T>& a, octave_idx_type r,
                           octave_idx_type c)
  : Array<T> (a.as_column ()), m_d1 (r), m_d2 (c)
{
  octave_idx_type rcmin = std::min (r, c);
  if (rcmin != a.numel ())
    Array<T>::resize (dim_vector (rcmin, 1));
}

template <typename T>
Array<T>
DiagArray2<T>::extract_diag (octave_idx_type k) const
{
  Array<T> d;

  if (k == 0)
    // The main diagonal is shallow-copied.
    d = *this;
  else if (k > 0 && k < cols ())
    d = Array<T> (dim_vector (std::min (cols () - k, rows ()), 1), T ());
  else if (k < 0 && -k < rows ())
    d = Array<T> (dim_vector (std::min (rows () + k, cols ()), 1), T ());
  else  // Matlab returns [] 0x1 for out-of-range diagonal
    d.resize (dim_vector (0, 1));

  return d;
}

template <typename T>
DiagArray2<T>
DiagArray2<T>::transpose (void) const
{
  return DiagArray2<T> (*this, m_d2, m_d1);
}

template <typename T>
DiagArray2<T>
DiagArray2<T>::hermitian (T (* fcn) (const T&)) const
{
  return DiagArray2<T> (Array<T>::template map<T> (fcn), m_d2, m_d1);
}

// A two-dimensional array with diagonal elements only.

template <typename T>
T&
DiagArray2<T>::elem (octave_idx_type r, octave_idx_type c)
{
  static T zero (0);
  return (r == c) ? Array<T>::elem (r) : zero;
}

template <typename T>
T&
DiagArray2<T>::checkelem (octave_idx_type r, octave_idx_type c)
{
  static T zero (0);
  return check_idx (r, c) ? elem (r, c) : zero;
}

template <typename T>
void
DiagArray2<T>::resize (octave_idx_type r, octave_idx_type c,
                       const T& rfv)
{
  if (r < 0 || c < 0)
    (*current_liboctave_error_handler) ("can't resize to negative dimensions");

  if (r != dim1 () || c != dim2 ())
    {
      Array<T>::resize (dim_vector (std::min (r, c), 1), rfv);
      m_d1 = r; m_d2 = c;
    }
}

template <typename T>
Array<T>
DiagArray2<T>::array_value (void) const
{
  Array<T> result (dims (), T (0));

  for (octave_idx_type i = 0, len = length (); i < len; i++)
    result.xelem (i, i) = dgelem (i);

  return result;
}

template <typename T>
bool
DiagArray2<T>::check_idx (octave_idx_type r, octave_idx_type c) const
{
  bool ok = true;

  if (r < 0 || r >= dim1 ())
    octave::err_index_out_of_range (2, 1, r+1, dim1 (), dims ());

  if (c < 0 || c >= dim2 ())
    octave::err_index_out_of_range (2, 2, c+1, dim2 (), dims ());

  return ok;
}
