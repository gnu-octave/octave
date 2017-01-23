/*

Copyright (C) 1993-2016 John W. Eaton
Copyright (C) 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_MArray_h)
#define octave_MArray_h 1

#include "octave-config.h"

#include "Array.h"
#include "mx-inlines.cc"

template <typename T> class MArray;

template <typename T> MArray<T>& operator += (MArray<T>&, const T&);
template <typename T> MArray<T>& operator -= (MArray<T>&, const T&);
template <typename T> MArray<T>& operator *= (MArray<T>&, const T&);
template <typename T> MArray<T>& operator /= (MArray<T>&, const T&);
template <typename T> MArray<T>& operator += (MArray<T>&, const MArray<T>&);
template <typename T> MArray<T>& operator -= (MArray<T>&, const MArray<T>&);
template <typename T> MArray<T>& product_eq (MArray<T>&, const MArray<T>&);
template <typename T> MArray<T>& quotient_eq (MArray<T>&, const MArray<T>&);
template <typename T> MArray<T> operator + (const MArray<T>&);
template <typename T> MArray<T> operator - (const MArray<T>&);
template <typename T> MArray<T> operator + (const MArray<T>&, const T&);
template <typename T> MArray<T> operator - (const MArray<T>&, const T&);
template <typename T> MArray<T> operator * (const MArray<T>&, const T&);
template <typename T> MArray<T> operator / (const MArray<T>&, const T&);
template <typename T> MArray<T> operator + (const T&, const MArray<T>&);
template <typename T> MArray<T> operator - (const T&, const MArray<T>&);
template <typename T> MArray<T> operator * (const T&, const MArray<T>&);
template <typename T> MArray<T> operator / (const T&, const MArray<T>&);
template <typename T> MArray<T> operator + (const MArray<T>&, const MArray<T>&);
template <typename T> MArray<T> operator - (const MArray<T>&, const MArray<T>&);
template <typename T> MArray<T> quotient (const MArray<T>&, const MArray<T>&);
template <typename T> MArray<T> product (const MArray<T>&, const MArray<T>&);

//! Template for N-dimensional array classes with like-type math operators.
template <typename T>
class
MArray : public Array<T>
{
protected:

  // For jit support
  MArray (T *sdata, octave_idx_type slen, octave_idx_type *adims, void *arep)
    : Array<T> (sdata, slen, adims, arep) { }

public:

  MArray (void) : Array<T> () { }

  explicit MArray (const dim_vector& dv)
    : Array<T> (dv) { }

  explicit MArray (const dim_vector& dv, const T& val)
    : Array<T> (dv, val) { }

  MArray (const MArray<T>& a) : Array<T> (a) { }

  template <typename U>
  MArray (const Array<U>& a) : Array<T> (a) { }

  ~MArray (void) = default;

  MArray<T>& operator = (const MArray<T>& a)
  {
    Array<T>::operator = (a);
    return *this;
  }

  MArray<T> reshape (const dim_vector& new_dims) const
  { return Array<T>::reshape (new_dims); }

  MArray<T> permute (const Array<octave_idx_type>& vec,
                     bool inv = false) const
  { return Array<T>::permute (vec, inv); }

  MArray<T> ipermute (const Array<octave_idx_type>& vec) const
  { return Array<T>::ipermute (vec); }

  MArray squeeze (void) const { return Array<T>::squeeze (); }

  MArray<T> transpose (void) const
  { return Array<T>::transpose (); }

  MArray<T> hermitian (T (*fcn) (const T&) = 0) const
  { return Array<T>::hermitian (fcn); }

  //! Performs indexed accumulative addition.
  //@{
  void idx_add (const idx_vector& idx, T val);
  void idx_add (const idx_vector& idx, const MArray<T>& vals);
  //@}

  void idx_min (const idx_vector& idx, const MArray<T>& vals);

  void idx_max (const idx_vector& idx, const MArray<T>& vals);

  void idx_add_nd (const idx_vector& idx, const MArray<T>& vals, int dim = -1);

  void changesign (void);
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

#endif
