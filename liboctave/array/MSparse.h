////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#if ! defined (octave_MSparse_h)
#define octave_MSparse_h 1

#include "octave-config.h"

#include "Array-util.h"
#include "MArray.h"
#include "Sparse.h"
#include "lo-error.h"
#include "quit.h"


// forward declare template with visibility attributes
template <typename T> class OCTAVE_API MSparse;

// Two dimensional sparse array with math ops.
template <typename T>
class
OCTAVE_API
MSparse : public Sparse<T>
{
public:

  MSparse (void) : Sparse<T> () { }

  MSparse (octave_idx_type n, octave_idx_type m) : Sparse<T> (n, m) { }

  MSparse (const dim_vector& dv, octave_idx_type nz = 0)
    : Sparse<T> (dv, nz) { }

  MSparse (const MSparse<T>& a) : Sparse<T> (a) { }

  MSparse (const MSparse<T>& a, const dim_vector& dv) : Sparse<T> (a, dv) { }

  MSparse (const Sparse<T>& a) : Sparse<T> (a) { }

  template <typename U>
  MSparse (const Sparse<U>& a) : Sparse<T> (a) { }

  MSparse (const Array<T>& a, const octave::idx_vector& r, const octave::idx_vector& c,
           octave_idx_type nr = -1, octave_idx_type nc = -1,
           bool sum_terms = true, octave_idx_type nzm = -1)
    : Sparse<T> (a, r, c, nr, nc, sum_terms, nzm) { }

  explicit MSparse (octave_idx_type r, octave_idx_type c, T val)
    : Sparse<T> (r, c, val) { }

  explicit MSparse (const PermMatrix& a) : Sparse<T>(a) { }

  MSparse (octave_idx_type r, octave_idx_type c, octave_idx_type num_nz)
    : Sparse<T> (r, c, num_nz) { }

  ~MSparse (void) = default;

  MSparse<T>& operator = (const MSparse<T>& a)
  {
    Sparse<T>::operator = (a);
    return *this;
  }

  MSparse<T>& insert (const Sparse<T>& a, octave_idx_type r, octave_idx_type c)
  {
    Sparse<T>::insert (a, r, c);
    return *this;
  }

  MSparse<T>& insert (const Sparse<T>& a, const Array<octave_idx_type>& indx)
  {
    Sparse<T>::insert (a, indx);
    return *this;
  }

  MSparse<T> transpose (void) const { return Sparse<T>::transpose (); }

  MSparse<T> squeeze (void) const { return Sparse<T>::squeeze (); }

  MSparse<T> reshape (const dim_vector& new_dims) const
  { return Sparse<T>::reshape (new_dims); }

  MSparse<T> permute (const Array<octave_idx_type>& vec, bool inv = false) const
  { return Sparse<T>::permute (vec, inv); }

  MSparse<T> ipermute (const Array<octave_idx_type>& vec) const
  { return Sparse<T>::ipermute (vec); }

  MSparse<T> diag (octave_idx_type k = 0) const
  {
    return Sparse<T>::diag (k);
  }

  // FIXME: should go away.
  template <typename U>
  Sparse<U>
  map (U (&fcn) (T)) const
  { return Sparse<T>::template map<U> (fcn); }

  template <typename U>
  Sparse<U>
  map (U (&fcn) (const T&)) const
  { return Sparse<T>::template map<U> (fcn); }
};

// Include operator templates for MSparse
#include "MSparse.cc"

// A macro that can be used to declare and instantiate OP= operators.
#define SPARSE_OP_ASSIGN_DECL(T, OP, API)       \
  template API MSparse<T>&                      \
  operator OP (MSparse<T>&, const MSparse<T>&)

// A macro that can be used to declare and instantiate unary operators.
#define SPARSE_UNOP_DECL(T, OP, API)            \
  template API MSparse<T>                       \
  operator OP (const MSparse<T>&)

// A macro that can be used to declare and instantiate binary operators.
#define SPARSE_BINOP_DECL(A_T, T, F, API, X_T, Y_T)     \
  template API A_T<T>                                   \
  F (const X_T&, const Y_T&)

// A function that can be used to forward OP= operations from derived
// classes back to us.
#define SPARSE_OP_ASSIGN_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T)   \
  inline R                                                      \
  F (X_T& x, const Y_T& y)                                      \
  {                                                             \
    return R (F (C_X (x), C_Y (y)));                            \
  }

// A function that can be used to forward unary operations from derived
// classes back to us.
#define SPARSE_UNOP_FWD_FCN(R, F, T, C_X, X_T)  \
  inline R                                      \
  F (const X_T& x)                              \
  {                                             \
    return R (F (C_X (x)));                     \
  }

// A function that can be used to forward binary operations from derived
// classes back to us.
#define SPARSE_BINOP_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T)       \
  inline R                                                      \
  F (const X_T& x, const Y_T& y)                                \
  {                                                             \
    return R (F (C_X (x), C_Y (y)));                            \
  }

// Instantiate all the MSparse friends for MSparse element type T.
#define INSTANTIATE_SPARSE_FRIENDS(T, API)                              \
  SPARSE_OP_ASSIGN_DECL (T, +=, API);                                   \
  SPARSE_OP_ASSIGN_DECL (T, -=, API);                                   \
  SPARSE_UNOP_DECL (T, +, API);                                         \
  SPARSE_UNOP_DECL (T, -, API);                                         \
  SPARSE_BINOP_DECL (MArray,  T, operator +, API, MSparse<T>, T);       \
  SPARSE_BINOP_DECL (MArray,  T, operator -, API, MSparse<T>, T);       \
  SPARSE_BINOP_DECL (MSparse, T, operator *, API, MSparse<T>, T);       \
  SPARSE_BINOP_DECL (MSparse, T, operator /, API, MSparse<T>, T);       \
  SPARSE_BINOP_DECL (MArray,  T, operator +, API, T, MSparse<T>);       \
  SPARSE_BINOP_DECL (MArray,  T, operator -, API, T, MSparse<T>);       \
  SPARSE_BINOP_DECL (MSparse, T, operator *, API, T, MSparse<T>);       \
  SPARSE_BINOP_DECL (MSparse, T, operator /, API, T, MSparse<T>);       \
  SPARSE_BINOP_DECL (MSparse, T, operator +, API, MSparse<T>, MSparse<T>); \
  SPARSE_BINOP_DECL (MSparse, T, operator -, API, MSparse<T>, MSparse<T>); \
  SPARSE_BINOP_DECL (MSparse, T, quotient,   API, MSparse<T>, MSparse<T>); \
  SPARSE_BINOP_DECL (MSparse, T, product,    API, MSparse<T>, MSparse<T>);

// Define all the MSparse forwarding functions for return type R and
// MSparse element type T
#define SPARSE_FORWARD_DEFS(B, R, F, T)                                 \
  SPARSE_OP_ASSIGN_FWD_FCN (R, operator +=, T, dynamic_cast<B<T>&>,     \
                            R, dynamic_cast<const B<T>&>, R)            \
  SPARSE_OP_ASSIGN_FWD_FCN (R, operator -=, T, dynamic_cast<B<T>&>,     \
                            R, dynamic_cast<const B<T>&>, R)            \
  SPARSE_UNOP_FWD_FCN (R, operator +, T, dynamic_cast<const B<T>&>, R)  \
  SPARSE_UNOP_FWD_FCN (R, operator -, T, dynamic_cast<const B<T>&>, R)  \
  SPARSE_BINOP_FWD_FCN (F, operator +, T, dynamic_cast<const B<T>&>, R, , T) \
  SPARSE_BINOP_FWD_FCN (F, operator -, T, dynamic_cast<const B<T>&>, R, , T) \
  SPARSE_BINOP_FWD_FCN (R, operator *, T, dynamic_cast<const B<T>&>, R, , T) \
  SPARSE_BINOP_FWD_FCN (R, operator /, T, dynamic_cast<const B<T>&>, R, , T) \
  SPARSE_BINOP_FWD_FCN (F, operator +, T, , T, dynamic_cast<const B<T>&>, R) \
  SPARSE_BINOP_FWD_FCN (F, operator -, T, , T, dynamic_cast<const B<T>&>, R) \
  SPARSE_BINOP_FWD_FCN (R, operator *, T, , T, dynamic_cast<const B<T>&>, R) \
  SPARSE_BINOP_FWD_FCN (R, operator /, T, , T, dynamic_cast<const B<T>&>, R) \
  SPARSE_BINOP_FWD_FCN (R, operator +, T, dynamic_cast<const B<T>&>,    \
                        R, dynamic_cast<const B<T>&>, R)                \
  SPARSE_BINOP_FWD_FCN (R, operator -, T, dynamic_cast<const B<T>&>,    \
                        R, dynamic_cast<const B<T>&>, R)                \
  SPARSE_BINOP_FWD_FCN (R, product,    T, dynamic_cast<const B<T>&>,    \
                        R, dynamic_cast<const B<T>&>, R)                \
  SPARSE_BINOP_FWD_FCN (R, quotient, T, dynamic_cast<const B<T>&>,      \
                        R, dynamic_cast<const B<T>&>, R)

#endif
