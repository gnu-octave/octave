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

#if ! defined (octave_MDiagArray2_h)
#define octave_MDiagArray2_h 1

#include "octave-config.h"

#include "DiagArray2.h"
#include "MArray.h"

template <typename T> class MDiagArray2;

template <typename T> MDiagArray2<T> operator + (const MDiagArray2<T>&);
template <typename T> MDiagArray2<T> operator - (const MDiagArray2<T>&);
template <typename T> MDiagArray2<T> operator * (const MDiagArray2<T>&,
                                                 const T&);
template <typename T> MDiagArray2<T> operator / (const MDiagArray2<T>&,
                                                 const T&);
template <typename T> MDiagArray2<T> operator * (const T&,
                                                 const MDiagArray2<T>&);
template <typename T> MDiagArray2<T> operator + (const MDiagArray2<T>&,
                                                 const MDiagArray2<T>&);
template <typename T> MDiagArray2<T> operator - (const MDiagArray2<T>&,
                                                 const MDiagArray2<T>&);
template <typename T> MDiagArray2<T> product (const MDiagArray2<T>&,
                                              const MDiagArray2<T>&);

//! Template for two dimensional diagonal array with math operators.
template <typename T>
class
OCTAVE_API
MDiagArray2 : public DiagArray2<T>
{
public:

  MDiagArray2 (void) : DiagArray2<T> () { }

  MDiagArray2 (octave_idx_type r, octave_idx_type c) : DiagArray2<T> (r, c) { }

  MDiagArray2 (octave_idx_type r, octave_idx_type c, const T& val)
    : DiagArray2<T> (r, c, val) { }

  MDiagArray2 (const MDiagArray2<T>& a) : DiagArray2<T> (a) { }

  MDiagArray2 (const DiagArray2<T>& a) : DiagArray2<T> (a) { }

  template <typename U>
  MDiagArray2 (const DiagArray2<U>& a) : DiagArray2<T> (a) { }

  explicit MDiagArray2 (const Array<T>& a) : DiagArray2<T> (a) { }

  MDiagArray2 (const Array<T>& a, octave_idx_type r, octave_idx_type c)
    : DiagArray2<T> (a, r, c) { }

  ~MDiagArray2 (void) = default;

  MDiagArray2<T>& operator = (const MDiagArray2<T>& a)
  {
    DiagArray2<T>::operator = (a);
    return *this;
  }

  MArray<T> array_value () const
  {
    return DiagArray2<T>::array_value ();
  }

  octave_idx_type nnz (void) const
  {
    const T *d = this->data ();

    octave_idx_type nel = this->length ();

    static constexpr T zero = T ();

    return std::count_if (d, d + nel,
                          [] (T elem) { return elem != zero; });
  }

  MArray<T> diag (octave_idx_type k = 0) const
  { return DiagArray2<T>::extract_diag (k); }

  MDiagArray2<T> transpose (void) const { return DiagArray2<T>::transpose (); }
  MDiagArray2<T> hermitian (T (*fcn) (const T&) = nullptr) const
  { return DiagArray2<T>::hermitian (fcn); }

  OCTAVE_API bool is_multiple_of_identity (T val) const;

  // Currently, the OPS functions don't need to be friends, but that
  // may change.

  friend OCTAVE_API MDiagArray2<T> operator + <> (const MDiagArray2<T>&);
  friend OCTAVE_API MDiagArray2<T> operator - <> (const MDiagArray2<T>&);
  friend OCTAVE_API MDiagArray2<T>
  operator * <> (const MDiagArray2<T>&, const T&);
  friend OCTAVE_API MDiagArray2<T>
  operator / <> (const MDiagArray2<T>&, const T&);
  friend OCTAVE_API MDiagArray2<T>
  operator * <> (const T&, const MDiagArray2<T>&);
  friend OCTAVE_API MDiagArray2<T>
  operator + <> (const MDiagArray2<T>&, const MDiagArray2<T>&);
  friend OCTAVE_API MDiagArray2<T>
  operator - <> (const MDiagArray2<T>&, const MDiagArray2<T>&);
  friend OCTAVE_API MDiagArray2<T>
  product <> (const MDiagArray2<T>&, const MDiagArray2<T>&);

};

#define MDIAGARRAY2_FORWARD_DEFS(B, R, T)                               \
  inline R                                                              \
  operator + (const R& x)                                               \
  {                                                                     \
    return R (operator + (dynamic_cast<const B<T>&> (x)));              \
  }                                                                     \
  inline R                                                              \
  operator - (const R& x)                                               \
  {                                                                     \
    return R (operator - (dynamic_cast<const B<T>&> (x)));              \
  }                                                                     \
  inline R                                                              \
  operator * (const R& x, const T& y)                                   \
  {                                                                     \
    return R (operator * (dynamic_cast<const B<T>&> (x), (y)));         \
  }                                                                     \
  inline R                                                              \
  operator / (const R& x, const T& y)                                   \
  {                                                                     \
    return R (operator / (dynamic_cast<const B<T>&> (x), (y)));         \
  }                                                                     \
  inline R                                                              \
  operator * (const T& x, const R& y)                                   \
  {                                                                     \
    return R (operator * ( (x), dynamic_cast<const B<T>&> (y)));        \
  }                                                                     \
  inline R                                                              \
  operator + (const R& x, const R& y)                                   \
  {                                                                     \
    return R (operator + (dynamic_cast<const B<T>&> (x),                \
                          dynamic_cast<const B<T>&> (y)));              \
  }                                                                     \
  inline R                                                              \
  operator - (const R& x, const R& y)                                   \
  {                                                                     \
    return R (operator - (dynamic_cast<const B<T>&> (x),                \
                          dynamic_cast<const B<T>&> (y)));              \
  }                                                                     \
  inline R                                                              \
  product (const R& x, const R& y)                                      \
  {                                                                     \
    return R (product (dynamic_cast<const B<T>&> (x),                   \
                       dynamic_cast<const B<T>&> (y)));                 \
  }

#endif
