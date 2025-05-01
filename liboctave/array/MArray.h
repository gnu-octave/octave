////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2025 The Octave Project Developers
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

#if ! defined (octave_MArray_h)
#define octave_MArray_h 1

#include "octave-config.h"

#include "Array-oct.h"
#include "MArray-fwd.h"
#include "mx-inlines.cc"

template <typename T> OCTAVE_API MArray<T>& operator += (MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T>& operator -= (MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T>& operator *= (MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T>& operator /= (MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T>& operator += (MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T>& operator -= (MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T>& product_eq (MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T>& quotient_eq (MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator + (const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator - (const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator + (const MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T> operator - (const MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T> operator * (const MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T> operator / (const MArray<T>&, const T&);
template <typename T> OCTAVE_API MArray<T> operator + (const T&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator - (const T&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator * (const T&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator / (const T&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator + (const MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> operator - (const MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> quotient (const MArray<T>&, const MArray<T>&);
template <typename T> OCTAVE_API MArray<T> product (const MArray<T>&, const MArray<T>&);

//! Template for N-dimensional array classes with like-type math operators.
template <typename T>
class OCTAVE_API MArray : public Array<T>
{
public:

  OCTAVE_OVERRIDABLE_FUNC_API
  MArray () : Array<T> () { }

  explicit OCTAVE_OVERRIDABLE_FUNC_API
  MArray (const dim_vector& dv)
    : Array<T> (dv) { }

  explicit OCTAVE_OVERRIDABLE_FUNC_API
  MArray (const dim_vector& dv, const T& val)
    : Array<T> (dv, val) { }

  OCTAVE_OVERRIDABLE_FUNC_API
  MArray (const MArray<T>& a) : Array<T> (a) { }

  template <typename U>
  OCTAVE_OVERRIDABLE_FUNC_API
  MArray (const Array<U>& a) : Array<T> (a) { }

  OCTAVE_OVERRIDABLE_FUNC_API ~MArray () = default;

  OCTAVE_OVERRIDABLE_FUNC_API MArray<T>& operator = (const MArray<T>& a)
  {
    Array<T>::operator = (a);
    return *this;
  }

  OCTAVE_OVERRIDABLE_FUNC_API MArray<T>
  reshape (const dim_vector& new_dims) const
  { return Array<T>::reshape (new_dims); }

  OCTAVE_OVERRIDABLE_FUNC_API MArray<T>
  permute (const Array<octave_idx_type>& vec, bool inv = false) const
  { return Array<T>::permute (vec, inv); }

  OCTAVE_OVERRIDABLE_FUNC_API MArray<T>
  ipermute (const Array<octave_idx_type>& vec) const
  { return Array<T>::ipermute (vec); }

  OCTAVE_OVERRIDABLE_FUNC_API MArray squeeze () const
  { return Array<T>::squeeze (); }

  OCTAVE_OVERRIDABLE_FUNC_API MArray<T> transpose () const
  { return Array<T>::transpose (); }

  OCTAVE_OVERRIDABLE_FUNC_API MArray<T>
  hermitian (T (*fcn) (const T&) = nullptr) const
  { return Array<T>::hermitian (fcn); }

  //! Performs indexed accumulative addition.
  //@{
  OCTAVE_API void idx_add (const octave::idx_vector& idx, T val);
  OCTAVE_API void
  idx_add (const octave::idx_vector& idx, const MArray<T>& vals);
  //@}

  OCTAVE_API void
  idx_min (const octave::idx_vector& idx, const MArray<T>& vals);

  OCTAVE_API void
  idx_max (const octave::idx_vector& idx, const MArray<T>& vals);

  OCTAVE_API void
  idx_add_nd (const octave::idx_vector& idx, const MArray<T>& vals,
              int dim = -1);

  OCTAVE_API void changesign ();

private:
  OCTAVE_API static void instantiation_guard ();
};

// Define all the MArray forwarding functions for return type R and
// MArray element type T
#define MARRAY_FORWARD_DEFS(B, R, T)                                    \
  inline R operator += (R& x, const T& y)                               \
  {                                                                     \
    return R (operator += (dynamic_cast<B<T>&> (x), (y)));              \
  }                                                                     \
  inline R operator -= (R& x, const T& y)                               \
  {                                                                     \
    return R (operator -= (dynamic_cast<B<T>&> (x), (y)));              \
  }                                                                     \
  inline R operator *= (R& x, const T& y)                               \
  {                                                                     \
    return R (operator *= (dynamic_cast<B<T>&> (x), (y)));              \
  }                                                                     \
  inline R operator /= (R& x, const T& y)                               \
  {                                                                     \
    return R (operator /= (dynamic_cast<B<T>&> (x), (y)));              \
  }                                                                     \
  inline R operator += (R& x, const R& y)                               \
  {                                                                     \
    return R (operator += (dynamic_cast<B<T>&> (x),                     \
                           dynamic_cast<const B<T>&> (y)));             \
  }                                                                     \
  inline R operator -= (R& x, const R& y)                               \
  {                                                                     \
    return R (operator -= (dynamic_cast<B<T>&> (x),                     \
                           dynamic_cast<const B<T>&> (y)));             \
  }                                                                     \
  inline R product_eq (R& x, const R& y)                                \
  {                                                                     \
    return R (product_eq (dynamic_cast<B<T>&> (x),                      \
                          dynamic_cast<const B<T>&> (y)));              \
  }                                                                     \
  inline R quotient_eq (R& x, const R& y)                               \
  {                                                                     \
    return R (quotient_eq (dynamic_cast<B<T>&> (x),                     \
                           dynamic_cast<const B<T>&> (y)));             \
  }                                                                     \
  inline R operator + (const R& x)                                      \
  {                                                                     \
    return R (operator + (dynamic_cast<const B<T>&> (x)));              \
  }                                                                     \
  inline R operator - (const R& x)                                      \
  {                                                                     \
    return R (operator - (dynamic_cast<const B<T>&> (x)));              \
  }                                                                     \
  inline R operator + (const R& x, const T& y)                          \
  {                                                                     \
    return R (operator + (dynamic_cast<const B<T>&> (x), (y)));         \
  }                                                                     \
  inline R operator - (const R& x, const T& y)                          \
  {                                                                     \
    return R (operator - (dynamic_cast<const B<T>&> (x), (y)));         \
  }                                                                     \
  inline R operator * (const R& x, const T& y)                          \
  {                                                                     \
    return R (operator * (dynamic_cast<const B<T>&> (x), (y)));         \
  }                                                                     \
  inline R operator / (const R& x, const T& y)                          \
  {                                                                     \
    return R (operator / (dynamic_cast<const B<T>&> (x), (y)));         \
  }                                                                     \
  inline R operator + (const T& x, const R& y)                          \
  {                                                                     \
    return R (operator + ( (x), dynamic_cast<const B<T>&> (y)));        \
  }                                                                     \
  inline R operator - (const T& x, const R& y)                          \
  {                                                                     \
    return R (operator - ( (x), dynamic_cast<const B<T>&> (y)));        \
  }                                                                     \
  inline R operator * (const T& x, const R& y)                          \
  {                                                                     \
    return R (operator * ( (x), dynamic_cast<const B<T>&> (y)));        \
  }                                                                     \
  inline R operator / (const T& x, const R& y)                          \
  {                                                                     \
    return R (operator / ( (x), dynamic_cast<const B<T>&> (y)));        \
  }                                                                     \
  inline R operator + (const R& x, const R& y)                          \
  {                                                                     \
    return R (operator + (dynamic_cast<const B<T>&> (x),                \
                          dynamic_cast<const B<T>&> (y)));              \
  }                                                                     \
  inline R operator - (const R& x, const R& y)                          \
  {                                                                     \
    return R (operator - (dynamic_cast<const B<T>&> (x),                \
                          dynamic_cast<const B<T>&> (y)));              \
  }                                                                     \
  inline R product (const R& x, const R& y)                             \
  {                                                                     \
    return R (product (dynamic_cast<const B<T>&> (x),                   \
                       dynamic_cast<const B<T>&> (y)));                 \
  }                                                                     \
  inline R quotient (const R& x, const R& y)                            \
  {                                                                     \
    return R (quotient (dynamic_cast<const B<T>&> (x),                  \
                        dynamic_cast<const B<T>&> (y)));                \
  }

// Instantiate all the MArray friends for MArray element type T.
#define INSTANTIATE_MARRAY_FRIENDS(T, API)                              \
  template API MArray<T>& operator += (MArray<T>&, const T&);           \
  template API MArray<T>& operator -= (MArray<T>&, const T&);           \
  template API MArray<T>& operator *= (MArray<T>&, const T&);           \
  template API MArray<T>& operator /= (MArray<T>&, const T&);           \
  template API MArray<T>& operator += (MArray<T>&, const MArray<T>&);   \
  template API MArray<T>& operator -= (MArray<T>&, const MArray<T>&);   \
  template API MArray<T>& product_eq (MArray<T>&, const MArray<T>&);    \
  template API MArray<T>& quotient_eq (MArray<T>&, const MArray<T>&);   \
  template API MArray<T> operator + (const MArray<T>&);                 \
  template API MArray<T> operator - (const MArray<T>&);                 \
  template API MArray<T> operator + (const MArray<T>&, const T&);       \
  template API MArray<T> operator - (const MArray<T>&, const T&);       \
  template API MArray<T> operator * (const MArray<T>&, const T&);       \
  template API MArray<T> operator / (const MArray<T>&, const T&);       \
  template API MArray<T> operator + (const T&, const MArray<T>&);       \
  template API MArray<T> operator - (const T&, const MArray<T>&);       \
  template API MArray<T> operator * (const T&, const MArray<T>&);       \
  template API MArray<T> operator / (const T&, const MArray<T>&);       \
  template API MArray<T> operator + (const MArray<T>&, const MArray<T>&); \
  template API MArray<T> operator - (const MArray<T>&, const MArray<T>&); \
  template API MArray<T> quotient (const MArray<T>&, const MArray<T>&); \
  template API MArray<T> product (const MArray<T>&, const MArray<T>&);

// Instantiate all the MDiagArray2 friends for MDiagArray2 element type T.
#define INSTANTIATE_MDIAGARRAY2_FRIENDS(T, API)                         \
  template API MDiagArray2<T> operator + (const MDiagArray2<T>&);       \
  template API MDiagArray2<T> operator - (const MDiagArray2<T>&);       \
  template API MDiagArray2<T> operator * (const MDiagArray2<T>&, const T&); \
  template API MDiagArray2<T> operator / (const MDiagArray2<T>&, const T&); \
  template API MDiagArray2<T> operator * (const T&, const MDiagArray2<T>&); \
  template API MDiagArray2<T> operator + (const MDiagArray2<T>&,        \
                                          const MDiagArray2<T>&);       \
  template API MDiagArray2<T> operator - (const MDiagArray2<T>&,        \
                                          const MDiagArray2<T>&);       \
  template API MDiagArray2<T> product (const MDiagArray2<T>&,           \
                                       const MDiagArray2<T>&);

#if ! defined (OCTAVE_EXTERN_TEMPLATE_MARRAY)
// guard against double extern template declarations
#define OCTAVE_EXTERN_TEMPLATE_MARRAY

#include "oct-cmplx.h"
#include "oct-inttypes-fwd.h"

// "Protect" MArray<T> instantiations that are exported by liboctave from
// being implicitly instantiated in compilation units including this file.

// instantiated in MArray-C.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<Complex>;
// instantiated in MArray-d.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<double>;
// instantiated in MArray-f.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<float>;
// instantiated in MArray-fC.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<FloatComplex>;
// instantiated in MArray-i.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<int>;
#if defined (OCTAVE_ENABLE_64)
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<int64_t>;
#endif
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_int8>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_int16>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_int32>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_int64>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_uint8>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_uint16>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_uint32>;
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<octave_uint64>;
// instantiated in MArray-s.cc
extern template class OCTAVE_EXTERN_TEMPLATE_API MArray<short>;

#endif  // OCTAVE_EXTERN_TEMPLATE_MARRAY

#endif
