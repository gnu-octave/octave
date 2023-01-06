////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_mx_inlines_h)
#define octave_mx_inlines_h 1

// This file should *not* include config.h.  It is only included in other C++
// source files that should have included config.h before including this file.

#include <cstddef>
#include <cmath>

#include <algorithm>

#include "Array-util.h"
#include "Array.h"
#include "bsxfun.h"
#include "oct-cmplx.h"
#include "oct-inttypes-fwd.h"
#include "oct-locbuf.h"

// Provides some commonly repeated, basic loop templates.

template <typename R, typename S>
inline void mx_inline_fill (std::size_t n, R *r, S s)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = s;
}

template <typename R, typename X>
inline void
mx_inline_uminus (std::size_t n, R *r, const X *x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = -x[i];
}

template <typename R>
inline void
mx_inline_uminus2 (std::size_t n, R *r)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = -r[i];
}

template <typename X>
inline void
mx_inline_iszero (std::size_t n, bool *r, const X *x)
{
  const X zero = X ();
  for (std::size_t i = 0; i < n; i++)
    r[i] = x[i] == zero;
}

template <typename X>
inline void
mx_inline_notzero (std::size_t n, bool *r, const X *x)
{
  const X zero = X ();
  for (std::size_t i = 0; i < n; i++)
    r[i] = x[i] != zero;
}

#define DEFMXBINOP(F, OP)                                       \
  template <typename R, typename X, typename Y>                 \
  inline void F (std::size_t n, R *r, const X *x, const Y *y)   \
  {                                                             \
    for (std::size_t i = 0; i < n; i++)                         \
      r[i] = x[i] OP y[i];                                      \
  }                                                             \
  template <typename R, typename X, typename Y>                 \
  inline void F (std::size_t n, R *r, const X *x, Y y)          \
  {                                                             \
    for (std::size_t i = 0; i < n; i++)                         \
      r[i] = x[i] OP y;                                         \
  }                                                             \
  template <typename R, typename X, typename Y>                 \
  inline void F (std::size_t n, R *r, X x, const Y *y)          \
  {                                                             \
    for (std::size_t i = 0; i < n; i++)                         \
      r[i] = x OP y[i];                                         \
  }

DEFMXBINOP (mx_inline_add, +)
DEFMXBINOP (mx_inline_sub, -)
DEFMXBINOP (mx_inline_mul, *)
DEFMXBINOP (mx_inline_div, /)

#define DEFMXBINOPEQ(F, OP)                             \
  template <typename R, typename X>                     \
  inline void F (std::size_t n, R *r, const X *x)       \
  {                                                     \
    for (std::size_t i = 0; i < n; i++)                 \
      r[i] OP x[i];                                     \
  }                                                     \
  template <typename R, typename X>                     \
  inline void F (std::size_t n, R *r, X x)              \
  {                                                     \
    for (std::size_t i = 0; i < n; i++)                 \
      r[i] OP x;                                        \
  }

DEFMXBINOPEQ (mx_inline_add2, +=)
DEFMXBINOPEQ (mx_inline_sub2, -=)
DEFMXBINOPEQ (mx_inline_mul2, *=)
DEFMXBINOPEQ (mx_inline_div2, /=)

#define DEFMXCMPOP(F, OP)                                               \
  template <typename X, typename Y>                                     \
  inline void F (std::size_t n, bool *r, const X *x, const Y *y)        \
  {                                                                     \
    for (std::size_t i = 0; i < n; i++)                                 \
      r[i] = x[i] OP y[i];                                              \
  }                                                                     \
  template <typename X, typename Y>                                     \
  inline void F (std::size_t n, bool *r, const X *x, Y y)               \
  {                                                                     \
    for (std::size_t i = 0; i < n; i++)                                 \
      r[i] = x[i] OP y;                                                 \
  }                                                                     \
  template <typename X, typename Y>                                     \
  inline void F (std::size_t n, bool *r, X x, const Y *y)               \
  {                                                                     \
    for (std::size_t i = 0; i < n; i++)                                 \
      r[i] = x OP y[i];                                                 \
  }

DEFMXCMPOP (mx_inline_lt, <)
DEFMXCMPOP (mx_inline_le, <=)
DEFMXCMPOP (mx_inline_gt, >)
DEFMXCMPOP (mx_inline_ge, >=)
DEFMXCMPOP (mx_inline_eq, ==)
DEFMXCMPOP (mx_inline_ne, !=)

// Convert to logical value, for logical op purposes.
template <typename T>
inline bool
logical_value (T x)
{
  return x;
}

template <typename T>
inline bool
logical_value (const std::complex<T>& x)
{
  return x.real () != 0 || x.imag () != 0;
}

template <typename T>
inline bool
logical_value (const octave_int<T>& x)
{
  return x.value ();
}

template <typename X>
void mx_inline_not (std::size_t n, bool *r, const X *x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = ! logical_value (x[i]);
}

inline void mx_inline_not2 (std::size_t n, bool *r)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = ! r[i];
}

#define DEFMXBOOLOP(F, NOT1, OP, NOT2)                                  \
  template <typename X, typename Y>                                     \
  inline void F (std::size_t n, bool *r, const X *x, const Y *y)        \
  {                                                                     \
    for (std::size_t i = 0; i < n; i++)                                 \
      r[i] = ((NOT1 logical_value (x[i]))                               \
              OP (NOT2 logical_value (y[i])));                          \
  }                                                                     \
  template <typename X, typename Y>                                     \
  inline void F (std::size_t n, bool *r, const X *x, Y y)               \
  {                                                                     \
    const bool yy = (NOT2 logical_value (y));                           \
    for (std::size_t i = 0; i < n; i++)                                 \
      r[i] = (NOT1 logical_value (x[i])) OP yy;                         \
  }                                                                     \
  template <typename X, typename Y>                                     \
  inline void F (std::size_t n, bool *r, X x, const Y *y)               \
  {                                                                     \
    const bool xx = (NOT1 logical_value (x));                           \
    for (std::size_t i = 0; i < n; i++)                                 \
      r[i] = xx OP (NOT2 logical_value (y[i]));                         \
  }

DEFMXBOOLOP (mx_inline_and, , &&, )
DEFMXBOOLOP (mx_inline_or, , ||, )
DEFMXBOOLOP (mx_inline_not_and, !, &&, )
DEFMXBOOLOP (mx_inline_not_or, !, ||, )
DEFMXBOOLOP (mx_inline_and_not, , &&, !)
DEFMXBOOLOP (mx_inline_or_not, , ||, !)

template <typename X>
inline void
mx_inline_and2 (std::size_t n, bool *r, const X *x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] &= logical_value (x[i]);
}

template <typename X>
inline void
mx_inline_and2 (std::size_t n, bool *r, X x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] &= x;
}

template <typename X>
inline void
mx_inline_or2 (std::size_t n, bool *r, const X *x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] |= logical_value (x[i]);
}

template <typename X>
inline void
mx_inline_or2 (std::size_t n, bool *r, X x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] |= x;
}

template <typename T>
inline bool
mx_inline_any_nan (std::size_t n, const T *x)
{
  for (std::size_t i = 0; i < n; i++)
    {
      if (octave::math::isnan (x[i]))
        return true;
    }

  return false;
}

template <typename T>
inline bool
mx_inline_all_finite (std::size_t n, const T *x)
{
  for (std::size_t i = 0; i < n; i++)
    {
      if (! octave::math::isfinite (x[i]))
        return false;
    }

  return true;
}

template <typename T>
inline bool
mx_inline_any_negative (std::size_t n, const T *x)
{
  for (std::size_t i = 0; i < n; i++)
    {
      if (x[i] < 0)
        return true;
    }

  return false;
}

template <typename T>
inline bool
mx_inline_any_positive (std::size_t n, const T *x)
{
  for (std::size_t i = 0; i < n; i++)
    {
      if (x[i] > 0)
        return true;
    }

  return false;
}

template <typename T>
inline bool
mx_inline_all_real (std::size_t n, const std::complex<T>* x)
{
  for (std::size_t i = 0; i < n; i++)
    {
      if (x[i].imag () != 0)
        return false;
    }

  return true;
}

template <typename T>
inline void mx_inline_real (std::size_t n, T *r, const std::complex<T>* x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = x[i].real ();
}

template <typename T>
inline void mx_inline_imag (std::size_t n, T *r, const std::complex<T>* x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = x[i].imag ();
}

template <typename T>
inline void
mx_inline_xmin (std::size_t n, T *r, const T *x, const T *y)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = octave::math::min (x[i], y[i]);
}

template <typename T>
inline void
mx_inline_xmin (std::size_t n, T *r, const T *x, T y)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = octave::math::min (x[i], y);
}

template <typename T>
inline void
mx_inline_xmin (std::size_t n, T *r, T x, const T *y)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = octave::math::min (x, y[i]);
}

template <typename T>
inline void
mx_inline_xmax (std::size_t n, T *r, const T *x, const T *y)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = octave::math::max (x[i], y[i]);
}

template <typename T>
inline void
mx_inline_xmax (std::size_t n, T *r, const T *x, T y)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = octave::math::max (x[i], y);
}

template <typename T>
inline void
mx_inline_xmax (std::size_t n, T *r, T x, const T *y)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = octave::math::max (x, y[i]);
}

// Specialize array-scalar max/min
#define DEFMINMAXSPEC(T, F, OP)                                 \
  template <>                                                   \
  inline void F<T> (std::size_t n, T *r, const T *x, T y)       \
  {                                                             \
    if (octave::math::isnan (y))                                \
      std::memcpy (r, x, n * sizeof (T));                       \
    else                                                        \
      for (std::size_t i = 0; i < n; i++)                       \
        r[i] = (x[i] OP y ? x[i] : y);                          \
  }                                                             \
  template <>                                                   \
  inline void F<T> (std::size_t n, T *r, T x, const T *y)       \
  {                                                             \
    if (octave::math::isnan (x))                                \
      std::memcpy (r, y, n * sizeof (T));                       \
    else                                                        \
      for (std::size_t i = 0; i < n; i++)                       \
        r[i] = (y[i] OP x ? y[i] : x);                          \
  }

DEFMINMAXSPEC (double, mx_inline_xmin, <=)
DEFMINMAXSPEC (double, mx_inline_xmax, >=)
DEFMINMAXSPEC (float, mx_inline_xmin, <=)
DEFMINMAXSPEC (float, mx_inline_xmax, >=)

// FIXME: Is this comment correct anymore?  It seems like std::pow is chosen.
// Let the compiler decide which pow to use, whichever best matches the
// arguments provided.

template <typename R, typename X, typename Y>
inline void
mx_inline_pow (std::size_t n, R *r, const X *x, const Y *y)
{
  using std::pow;

  for (std::size_t i = 0; i < n; i++)
    r[i] = pow (x[i], y[i]);
}

template <typename R, typename X, typename Y>
inline void
mx_inline_pow (std::size_t n, R *r, const X *x, Y y)
{
  using std::pow;

  for (std::size_t i = 0; i < n; i++)
    r[i] = pow (x[i], y);
}

template <typename R, typename X, typename Y>
inline void
mx_inline_pow (std::size_t n, R *r, X x, const Y *y)
{
  using std::pow;

  for (std::size_t i = 0; i < n; i++)
    r[i] = pow (x, y[i]);
}

// Arbitrary function appliers.
// The function is a template parameter to enable inlining.
template <typename R, typename X, R fcn (X x)>
inline void mx_inline_map (std::size_t n, R *r, const X *x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = fcn (x[i]);
}

template <typename R, typename X, R fcn (const X& x)>
inline void mx_inline_map (std::size_t n, R *r, const X *x)
{
  for (std::size_t i = 0; i < n; i++)
    r[i] = fcn (x[i]);
}

// Appliers.  Since these call the operation just once, we pass it as
// a pointer, to allow the compiler reduce number of instances.

template <typename R, typename X>
inline Array<R>
do_mx_unary_op (const Array<X>& x,
                void (*op) (std::size_t, R *, const X *))
{
  Array<R> r (x.dims ());
  op (r.numel (), r.fortran_vec (), x.data ());
  return r;
}

// Shortcuts for applying mx_inline_map.

template <typename R, typename X, R fcn (X)>
inline Array<R>
do_mx_unary_map (const Array<X>& x)
{
  return do_mx_unary_op<R, X> (x, mx_inline_map<R, X, fcn>);
}

template <typename R, typename X, R fcn (const X&)>
inline Array<R>
do_mx_unary_map (const Array<X>& x)
{
  return do_mx_unary_op<R, X> (x, mx_inline_map<R, X, fcn>);
}

template <typename R>
inline Array<R>&
do_mx_inplace_op (Array<R>& r,
                  void (*op) (std::size_t, R *))
{
  op (r.numel (), r.fortran_vec ());
  return r;
}

template <typename R, typename X, typename Y>
inline Array<R>
do_mm_binary_op (const Array<X>& x, const Array<Y>& y,
                 void (*op) (std::size_t, R *, const X *, const Y *),
                 void (*op1) (std::size_t, R *, X, const Y *),
                 void (*op2) (std::size_t, R *, const X *, Y),
                 const char *opname)
{
  dim_vector dx = x.dims ();
  dim_vector dy = y.dims ();
  if (dx == dy)
    {
      Array<R> r (dx);
      op (r.numel (), r.fortran_vec (), x.data (), y.data ());
      return r;
    }
  else if (is_valid_bsxfun (opname, dx, dy))
    {
      return do_bsxfun_op (x, y, op, op1, op2);
    }
  else
    octave::err_nonconformant (opname, dx, dy);
}

template <typename R, typename X, typename Y>
inline Array<R>
do_ms_binary_op (const Array<X>& x, const Y& y,
                 void (*op) (std::size_t, R *, const X *, Y))
{
  Array<R> r (x.dims ());
  op (r.numel (), r.fortran_vec (), x.data (), y);
  return r;
}

template <typename R, typename X, typename Y>
inline Array<R>
do_sm_binary_op (const X& x, const Array<Y>& y,
                 void (*op) (std::size_t, R *, X, const Y *))
{
  Array<R> r (y.dims ());
  op (r.numel (), r.fortran_vec (), x, y.data ());
  return r;
}

template <typename R, typename X>
inline Array<R>&
do_mm_inplace_op (Array<R>& r, const Array<X>& x,
                  void (*op) (std::size_t, R *, const X *),
                  void (*op1) (std::size_t, R *, X),
                  const char *opname)
{
  dim_vector dr = r.dims ();
  dim_vector dx = x.dims ();
  if (dr == dx)
    op (r.numel (), r.fortran_vec (), x.data ());
  else if (is_valid_inplace_bsxfun (opname, dr, dx))
    do_inplace_bsxfun_op (r, x, op, op1);
  else
    octave::err_nonconformant (opname, dr, dx);

  return r;
}

template <typename R, typename X>
inline Array<R>&
do_ms_inplace_op (Array<R>& r, const X& x,
                  void (*op) (std::size_t, R *, X))
{
  op (r.numel (), r.fortran_vec (), x);
  return r;
}

template <typename T1, typename T2>
inline bool
mx_inline_equal (std::size_t n, const T1 *x, const T2 *y)
{
  for (std::size_t i = 0; i < n; i++)
    if (x[i] != y[i])
      return false;
  return true;
}

template <typename T>
inline bool
do_mx_check (const Array<T>& a,
             bool (*op) (std::size_t, const T *))
{
  return op (a.numel (), a.data ());
}

// NOTE: we don't use std::norm because it typically does some heavyweight
// magic to avoid underflows, which we don't need here.
template <typename T>
inline T cabsq (const std::complex<T>& c)
{
  return c.real () * c.real () + c.imag () * c.imag ();
}

// default.  works for integers and bool.
template <typename T>
inline bool
xis_true (T x)
{
  return x;
}

template <typename T>
inline bool
xis_false (T x)
{
  return ! x;
}

// for octave_ints
template <typename T>
inline bool
xis_true (const octave_int<T>& x)
{
  return x.value ();
}

template <typename T>
inline bool
xis_false (const octave_int<T>& x)
{
  return ! x.value ();
}

// for reals, we want to ignore NaNs.
inline bool
xis_true (double x)
{
  return ! octave::math::isnan (x) && x != 0.0;
}

inline bool
xis_false (double x)
{
  return x == 0.0;
}

inline bool
xis_true (float x)
{
  return ! octave::math::isnan (x) && x != 0.0f;
}

inline bool
xis_false (float x)
{
  return x == 0.0f;
}

// Ditto for complex.
inline bool
xis_true (const Complex& x)
{
  return ! octave::math::isnan (x) && x != 0.0;
}

inline bool
xis_false (const Complex& x)
{
  return x == 0.0;
}

inline bool
xis_true (const FloatComplex& x)
{
  return ! octave::math::isnan (x) && x != 0.0f;
}

inline bool
xis_false (const FloatComplex& x)
{
  return x == 0.0f;
}

#define OP_RED_SUM(ac, el) ac += el
#define OP_RED_PROD(ac, el) ac *= el
#define OP_RED_SUMSQ(ac, el) ac += ((el)*(el))
#define OP_RED_SUMSQC(ac, el) ac += cabsq (el)

inline void
op_dble_prod (double& ac, float el)
{
  ac *= el;
}

// FIXME: guaranteed?
inline void
op_dble_prod (Complex& ac, const FloatComplex& el)
{
  ac *= el;
}

template <typename T>
inline void
op_dble_prod (double& ac, const octave_int<T>& el)
{
  ac *= el.double_value ();
}

inline void
op_dble_sum (double& ac, float el)
{
  ac += el;
}

// FIXME: guaranteed?
inline void
op_dble_sum (Complex& ac, const FloatComplex& el)
{
  ac += el;
}

template <typename T>
inline void
op_dble_sum (double& ac, const octave_int<T>& el)
{
  ac += el.double_value ();
}

// The following two implement a simple short-circuiting.
#define OP_RED_ANYC(ac, el)                     \
  if (xis_true (el))                            \
    {                                           \
      ac = true;                                \
      break;                                    \
    }                                           \
  else                                          \
    continue

#define OP_RED_ALLC(ac, el)                     \
  if (xis_false (el))                           \
    {                                           \
      ac = false;                               \
      break;                                    \
    }                                           \
  else                                          \
    continue

#define OP_RED_FCN(F, TSRC, TRES, OP, ZERO)     \
  template <typename T>                         \
  inline TRES                                   \
  F (const TSRC *v, octave_idx_type n)          \
  {                                             \
    TRES ac = ZERO;                             \
    for (octave_idx_type i = 0; i < n; i++)     \
      OP(ac, v[i]);                             \
    return ac;                                  \
  }

#define PROMOTE_DOUBLE(T)                                       \
  typename subst_template_param<std::complex, T, double>::type

OP_RED_FCN (mx_inline_sum, T, T, OP_RED_SUM, 0)
OP_RED_FCN (mx_inline_dsum, T, PROMOTE_DOUBLE(T), op_dble_sum, 0.0)
OP_RED_FCN (mx_inline_count, bool, T, OP_RED_SUM, 0)
OP_RED_FCN (mx_inline_prod, T, T, OP_RED_PROD, 1)
OP_RED_FCN (mx_inline_dprod, T, PROMOTE_DOUBLE(T), op_dble_prod, 1)
OP_RED_FCN (mx_inline_sumsq, T, T, OP_RED_SUMSQ, 0)
OP_RED_FCN (mx_inline_sumsq, std::complex<T>, T, OP_RED_SUMSQC, 0)
OP_RED_FCN (mx_inline_any, T, bool, OP_RED_ANYC, false)
OP_RED_FCN (mx_inline_all, T, bool, OP_RED_ALLC, true)

#define OP_RED_FCN2(F, TSRC, TRES, OP, ZERO)                            \
  template <typename T>                                                 \
  inline void                                                           \
  F (const TSRC *v, TRES *r, octave_idx_type m, octave_idx_type n)      \
  {                                                                     \
    for (octave_idx_type i = 0; i < m; i++)                             \
      r[i] = ZERO;                                                      \
    for (octave_idx_type j = 0; j < n; j++)                             \
      {                                                                 \
        for (octave_idx_type i = 0; i < m; i++)                         \
          OP(r[i], v[i]);                                               \
        v += m;                                                         \
      }                                                                 \
  }

OP_RED_FCN2 (mx_inline_sum, T, T, OP_RED_SUM, 0)
OP_RED_FCN2 (mx_inline_dsum, T, PROMOTE_DOUBLE(T), op_dble_sum, 0.0)
OP_RED_FCN2 (mx_inline_count, bool, T, OP_RED_SUM, 0)
OP_RED_FCN2 (mx_inline_prod, T, T, OP_RED_PROD, 1)
OP_RED_FCN2 (mx_inline_dprod, T, PROMOTE_DOUBLE(T), op_dble_prod, 0.0)
OP_RED_FCN2 (mx_inline_sumsq, T, T, OP_RED_SUMSQ, 0)
OP_RED_FCN2 (mx_inline_sumsq, std::complex<T>, T, OP_RED_SUMSQC, 0)

#define OP_RED_ANYR(ac, el) ac |= xis_true (el)
#define OP_RED_ALLR(ac, el) ac &= xis_true (el)

OP_RED_FCN2 (mx_inline_any_r, T, bool, OP_RED_ANYR, false)
OP_RED_FCN2 (mx_inline_all_r, T, bool, OP_RED_ALLR, true)

// Using the general code for any/all would sacrifice short-circuiting.
// OTOH, going by rows would sacrifice cache-coherence.  The following
// algorithm will achieve both, at the cost of a temporary octave_idx_type
// array.

#define OP_ROW_SHORT_CIRCUIT(F, PRED, ZERO)                             \
  template <typename T>                                                 \
  inline void                                                           \
  F (const T *v, bool *r, octave_idx_type m, octave_idx_type n)         \
  {                                                                     \
    if (n <= 8)                                                         \
      return F ## _r (v, r, m, n);                                      \
                                                                        \
    /* FIXME: it may be sub-optimal to allocate the buffer here. */     \
    OCTAVE_LOCAL_BUFFER (octave_idx_type, iact, m);                     \
    for (octave_idx_type i = 0; i < m; i++) iact[i] = i;                \
    octave_idx_type nact = m;                                           \
    for (octave_idx_type j = 0; j < n; j++)                             \
      {                                                                 \
        octave_idx_type k = 0;                                          \
        for (octave_idx_type i = 0; i < nact; i++)                      \
          {                                                             \
            octave_idx_type ia = iact[i];                               \
            if (! PRED (v[ia]))                                         \
              iact[k++] = ia;                                           \
          }                                                             \
        nact = k;                                                       \
        v += m;                                                         \
      }                                                                 \
    for (octave_idx_type i = 0; i < m; i++) r[i] = ! ZERO;              \
    for (octave_idx_type i = 0; i < nact; i++) r[iact[i]] = ZERO;       \
  }

OP_ROW_SHORT_CIRCUIT (mx_inline_any, xis_true, false)
OP_ROW_SHORT_CIRCUIT (mx_inline_all, xis_false, true)

#define OP_RED_FCNN(F, TSRC, TRES)              \
  template <typename T>                         \
  inline void                                   \
  F (const TSRC *v, TRES *r, octave_idx_type l, \
     octave_idx_type n, octave_idx_type u)      \
  {                                             \
    if (l == 1)                                 \
      {                                         \
        for (octave_idx_type i = 0; i < u; i++) \
          {                                     \
            r[i] = F<T> (v, n);                 \
            v += n;                             \
          }                                     \
      }                                         \
    else                                        \
      {                                         \
        for (octave_idx_type i = 0; i < u; i++) \
          {                                     \
            F (v, r, l, n);                     \
            v += l*n;                           \
            r += l;                             \
          }                                     \
      }                                         \
  }

OP_RED_FCNN (mx_inline_sum, T, T)
OP_RED_FCNN (mx_inline_dsum, T, PROMOTE_DOUBLE(T))
OP_RED_FCNN (mx_inline_count, bool, T)
OP_RED_FCNN (mx_inline_prod, T, T)
OP_RED_FCNN (mx_inline_dprod, T, PROMOTE_DOUBLE(T))
OP_RED_FCNN (mx_inline_sumsq, T, T)
OP_RED_FCNN (mx_inline_sumsq, std::complex<T>, T)
OP_RED_FCNN (mx_inline_any, T, bool)
OP_RED_FCNN (mx_inline_all, T, bool)

#define OP_CUM_FCN(F, TSRC, TRES, OP)           \
  template <typename T>                         \
  inline void                                   \
  F (const TSRC *v, TRES *r, octave_idx_type n) \
  {                                             \
    if (n)                                      \
      {                                         \
        TRES t = r[0] = v[0];                   \
        for (octave_idx_type i = 1; i < n; i++) \
          r[i] = t = t OP v[i];                 \
      }                                         \
  }

OP_CUM_FCN (mx_inline_cumsum, T, T, +)
OP_CUM_FCN (mx_inline_cumprod, T, T, *)
OP_CUM_FCN (mx_inline_cumcount, bool, T, +)

#define OP_CUM_FCN2(F, TSRC, TRES, OP)                                  \
  template <typename T>                                                 \
  inline void                                                           \
  F (const TSRC *v, TRES *r, octave_idx_type m, octave_idx_type n)      \
  {                                                                     \
    if (n)                                                              \
      {                                                                 \
        for (octave_idx_type i = 0; i < m; i++)                         \
          r[i] = v[i];                                                  \
        const T *r0 = r;                                                \
        for (octave_idx_type j = 1; j < n; j++)                         \
          {                                                             \
            r += m; v += m;                                             \
            for (octave_idx_type i = 0; i < m; i++)                     \
              r[i] = r0[i] OP v[i];                                     \
            r0 += m;                                                    \
          }                                                             \
      }                                                                 \
  }

OP_CUM_FCN2 (mx_inline_cumsum, T, T, +)
OP_CUM_FCN2 (mx_inline_cumprod, T, T, *)
OP_CUM_FCN2 (mx_inline_cumcount, bool, T, +)

#define OP_CUM_FCNN(F, TSRC, TRES)              \
  template <typename T>                         \
  inline void                                   \
  F (const TSRC *v, TRES *r, octave_idx_type l, \
     octave_idx_type n, octave_idx_type u)      \
  {                                             \
    if (l == 1)                                 \
      {                                         \
        for (octave_idx_type i = 0; i < u; i++) \
          {                                     \
            F (v, r, n);                        \
            v += n;                             \
            r += n;                             \
          }                                     \
      }                                         \
    else                                        \
      {                                         \
        for (octave_idx_type i = 0; i < u; i++) \
          {                                     \
            F (v, r, l, n);                     \
            v += l*n;                           \
            r += l*n;                           \
          }                                     \
      }                                         \
  }

OP_CUM_FCNN (mx_inline_cumsum, T, T)
OP_CUM_FCNN (mx_inline_cumprod, T, T)
OP_CUM_FCNN (mx_inline_cumcount, bool, T)

#define OP_MINMAX_FCN(F, OP)                                            \
  template <typename T>                                                 \
  void F (const T *v, T *r, octave_idx_type n)                          \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    T tmp = v[0];                                                       \
    octave_idx_type i = 1;                                              \
    if (octave::math::isnan (tmp))                                      \
      {                                                                 \
        for (; i < n && octave::math::isnan (v[i]); i++) ;              \
        if (i < n)                                                      \
          tmp = v[i];                                                   \
      }                                                                 \
    for (; i < n; i++)                                                  \
      if (v[i] OP tmp)                                                  \
        tmp = v[i];                                                     \
    *r = tmp;                                                           \
  }                                                                     \
  template <typename T>                                                 \
  void F (const T *v, T *r, octave_idx_type *ri, octave_idx_type n)     \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    T tmp = v[0];                                                       \
    octave_idx_type tmpi = 0;                                           \
    octave_idx_type i = 1;                                              \
    if (octave::math::isnan (tmp))                                      \
      {                                                                 \
        for (; i < n && octave::math::isnan (v[i]); i++) ;              \
        if (i < n)                                                      \
          {                                                             \
            tmp = v[i];                                                 \
            tmpi = i;                                                   \
          }                                                             \
      }                                                                 \
    for (; i < n; i++)                                                  \
      if (v[i] OP tmp)                                                  \
        {                                                               \
          tmp = v[i];                                                   \
          tmpi = i;                                                     \
        }                                                               \
    *r = tmp;                                                           \
    *ri = tmpi;                                                         \
  }

OP_MINMAX_FCN (mx_inline_min, <)
OP_MINMAX_FCN (mx_inline_max, >)

// Row reductions will be slightly complicated.  We will proceed with checks
// for NaNs until we detect that no row will yield a NaN, in which case we
// proceed to a faster code.

#define OP_MINMAX_FCN2(F, OP)                                           \
  template <typename T>                                                 \
  inline void                                                           \
  F (const T *v, T *r, octave_idx_type m, octave_idx_type n)            \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    bool nan = false;                                                   \
    octave_idx_type j = 0;                                              \
    for (octave_idx_type i = 0; i < m; i++)                             \
      {                                                                 \
        r[i] = v[i];                                                    \
        if (octave::math::isnan (v[i]))                                 \
          nan = true;                                                   \
      }                                                                 \
    j++;                                                                \
    v += m;                                                             \
    while (nan && j < n)                                                \
      {                                                                 \
        nan = false;                                                    \
        for (octave_idx_type i = 0; i < m; i++)                         \
          {                                                             \
            if (octave::math::isnan (v[i]))                             \
              nan = true;                                               \
            else if (octave::math::isnan (r[i]) || v[i] OP r[i])        \
              r[i] = v[i];                                              \
          }                                                             \
        j++;                                                            \
        v += m;                                                         \
      }                                                                 \
    while (j < n)                                                       \
      {                                                                 \
        for (octave_idx_type i = 0; i < m; i++)                         \
          if (v[i] OP r[i])                                             \
            r[i] = v[i];                                                \
        j++;                                                            \
        v += m;                                                         \
      }                                                                 \
  }                                                                     \
  template <typename T>                                                 \
  inline void                                                           \
  F (const T *v, T *r, octave_idx_type *ri,                             \
     octave_idx_type m, octave_idx_type n)                              \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    bool nan = false;                                                   \
    octave_idx_type j = 0;                                              \
    for (octave_idx_type i = 0; i < m; i++)                             \
      {                                                                 \
        r[i] = v[i];                                                    \
        ri[i] = j;                                                      \
        if (octave::math::isnan (v[i]))                                 \
          nan = true;                                                   \
      }                                                                 \
    j++;                                                                \
    v += m;                                                             \
    while (nan && j < n)                                                \
      {                                                                 \
        nan = false;                                                    \
        for (octave_idx_type i = 0; i < m; i++)                         \
          {                                                             \
            if (octave::math::isnan (v[i]))                             \
              nan = true;                                               \
            else if (octave::math::isnan (r[i]) || v[i] OP r[i])        \
              {                                                         \
                r[i] = v[i];                                            \
                ri[i] = j;                                              \
              }                                                         \
          }                                                             \
        j++;                                                            \
        v += m;                                                         \
      }                                                                 \
    while (j < n)                                                       \
      {                                                                 \
        for (octave_idx_type i = 0; i < m; i++)                         \
          if (v[i] OP r[i])                                             \
            {                                                           \
              r[i] = v[i];                                              \
              ri[i] = j;                                                \
            }                                                           \
        j++;                                                            \
        v += m;                                                         \
      }                                                                 \
  }

OP_MINMAX_FCN2 (mx_inline_min, <)
OP_MINMAX_FCN2 (mx_inline_max, >)

#define OP_MINMAX_FCNN(F)                                       \
  template <typename T>                                         \
  inline void                                                   \
  F (const T *v, T *r, octave_idx_type l,                       \
     octave_idx_type n, octave_idx_type u)                      \
  {                                                             \
    if (! n)                                                    \
      return;                                                   \
    if (l == 1)                                                 \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, n);                                        \
            v += n;                                             \
            r++;                                                \
          }                                                     \
      }                                                         \
    else                                                        \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, l, n);                                     \
            v += l*n;                                           \
            r += l;                                             \
          }                                                     \
      }                                                         \
  }                                                             \
  template <typename T>                                         \
  inline void                                                   \
  F (const T *v, T *r, octave_idx_type *ri,                     \
     octave_idx_type l, octave_idx_type n, octave_idx_type u)   \
  {                                                             \
    if (! n) return;                                            \
    if (l == 1)                                                 \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, ri, n);                                    \
            v += n;                                             \
            r++;                                                \
            ri++;                                               \
          }                                                     \
      }                                                         \
    else                                                        \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, ri, l, n);                                 \
            v += l*n;                                           \
            r += l;                                             \
            ri += l;                                            \
          }                                                     \
      }                                                         \
  }

OP_MINMAX_FCNN (mx_inline_min)
OP_MINMAX_FCNN (mx_inline_max)

#define OP_CUMMINMAX_FCN(F, OP)                                         \
  template <typename T>                                                 \
  void F (const T *v, T *r, octave_idx_type n)                          \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    T tmp = v[0];                                                       \
    octave_idx_type i = 1;                                              \
    octave_idx_type j = 0;                                              \
    if (octave::math::isnan (tmp))                                      \
      {                                                                 \
        for (; i < n && octave::math::isnan (v[i]); i++) ;              \
        for (; j < i; j++)                                              \
          r[j] = tmp;                                                   \
        if (i < n)                                                      \
          tmp = v[i];                                                   \
      }                                                                 \
    for (; i < n; i++)                                                  \
      if (v[i] OP tmp)                                                  \
        {                                                               \
          for (; j < i; j++)                                            \
            r[j] = tmp;                                                 \
          tmp = v[i];                                                   \
        }                                                               \
    for (; j < i; j++)                                                  \
      r[j] = tmp;                                                       \
  }                                                                     \
  template <typename T>                                                 \
  void F (const T *v, T *r, octave_idx_type *ri, octave_idx_type n)     \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    T tmp = v[0];                                                       \
    octave_idx_type tmpi = 0;                                           \
    octave_idx_type i = 1;                                              \
    octave_idx_type j = 0;                                              \
    if (octave::math::isnan (tmp))                                      \
      {                                                                 \
        for (; i < n && octave::math::isnan (v[i]); i++) ;              \
        for (; j < i; j++)                                              \
          {                                                             \
            r[j] = tmp;                                                 \
            ri[j] = tmpi;                                               \
          }                                                             \
        if (i < n)                                                      \
          {                                                             \
            tmp = v[i];                                                 \
            tmpi = i;                                                   \
          }                                                             \
      }                                                                 \
    for (; i < n; i++)                                                  \
      if (v[i] OP tmp)                                                  \
        {                                                               \
          for (; j < i; j++)                                            \
            {                                                           \
              r[j] = tmp;                                               \
              ri[j] = tmpi;                                             \
            }                                                           \
          tmp = v[i];                                                   \
          tmpi = i;                                                     \
        }                                                               \
    for (; j < i; j++)                                                  \
      {                                                                 \
        r[j] = tmp;                                                     \
        ri[j] = tmpi;                                                   \
      }                                                                 \
  }

OP_CUMMINMAX_FCN (mx_inline_cummin, <)
OP_CUMMINMAX_FCN (mx_inline_cummax, >)

// Row reductions will be slightly complicated.  We will proceed with checks
// for NaNs until we detect that no row will yield a NaN, in which case we
// proceed to a faster code.

#define OP_CUMMINMAX_FCN2(F, OP)                                        \
  template <typename T>                                                 \
  inline void                                                           \
  F (const T *v, T *r, octave_idx_type m, octave_idx_type n)            \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    bool nan = false;                                                   \
    const T *r0;                                                        \
    octave_idx_type j = 0;                                              \
    for (octave_idx_type i = 0; i < m; i++)                             \
      {                                                                 \
        r[i] = v[i];                                                    \
        if (octave::math::isnan (v[i]))                                 \
          nan = true;                                                   \
      }                                                                 \
    j++;                                                                \
    v += m;                                                             \
    r0 = r;                                                             \
    r += m;                                                             \
    while (nan && j < n)                                                \
      {                                                                 \
        nan = false;                                                    \
        for (octave_idx_type i = 0; i < m; i++)                         \
          {                                                             \
            if (octave::math::isnan (v[i]))                             \
              {                                                         \
                r[i] = r0[i];                                           \
                nan = true;                                             \
              }                                                         \
            else if (octave::math::isnan (r0[i]) || v[i] OP r0[i])      \
              r[i] = v[i];                                              \
            else                                                        \
              r[i] = r0[i];                                             \
          }                                                             \
        j++;                                                            \
        v += m;                                                         \
        r0 = r;                                                         \
        r += m;                                                         \
      }                                                                 \
    while (j < n)                                                       \
      {                                                                 \
        for (octave_idx_type i = 0; i < m; i++)                         \
          if (v[i] OP r0[i])                                            \
            r[i] = v[i];                                                \
          else                                                          \
            r[i] = r0[i];                                               \
        j++;                                                            \
        v += m;                                                         \
        r0 = r;                                                         \
        r += m;                                                         \
      }                                                                 \
  }                                                                     \
  template <typename T>                                                 \
  inline void                                                           \
  F (const T *v, T *r, octave_idx_type *ri,                             \
     octave_idx_type m, octave_idx_type n)                              \
  {                                                                     \
    if (! n)                                                            \
      return;                                                           \
    bool nan = false;                                                   \
    const T *r0;                                                        \
    const octave_idx_type *r0i;                                         \
    octave_idx_type j = 0;                                              \
    for (octave_idx_type i = 0; i < m; i++)                             \
      {                                                                 \
        r[i] = v[i]; ri[i] = 0;                                         \
        if (octave::math::isnan (v[i]))                                 \
          nan = true;                                                   \
      }                                                                 \
    j++;                                                                \
    v += m;                                                             \
    r0 = r;                                                             \
    r += m;                                                             \
    r0i = ri;                                                           \
    ri += m;                                                            \
    while (nan && j < n)                                                \
      {                                                                 \
        nan = false;                                                    \
        for (octave_idx_type i = 0; i < m; i++)                         \
          {                                                             \
            if (octave::math::isnan (v[i]))                             \
              {                                                         \
                r[i] = r0[i];                                           \
                ri[i] = r0i[i];                                         \
                nan = true;                                             \
              }                                                         \
            else if (octave::math::isnan (r0[i]) || v[i] OP r0[i])      \
              {                                                         \
                r[i] = v[i];                                            \
                ri[i] = j;                                              \
              }                                                         \
            else                                                        \
              {                                                         \
                r[i] = r0[i];                                           \
                ri[i] = r0i[i];                                         \
              }                                                         \
          }                                                             \
        j++;                                                            \
        v += m;                                                         \
        r0 = r;                                                         \
        r += m;                                                         \
        r0i = ri;                                                       \
        ri += m;                                                        \
      }                                                                 \
    while (j < n)                                                       \
      {                                                                 \
        for (octave_idx_type i = 0; i < m; i++)                         \
          if (v[i] OP r0[i])                                            \
            {                                                           \
              r[i] = v[i];                                              \
              ri[i] = j;                                                \
            }                                                           \
          else                                                          \
            {                                                           \
              r[i] = r0[i];                                             \
              ri[i] = r0i[i];                                           \
            }                                                           \
        j++;                                                            \
        v += m;                                                         \
        r0 = r;                                                         \
        r += m;                                                         \
        r0i = ri;                                                       \
        ri += m;                                                        \
      }                                                                 \
  }

OP_CUMMINMAX_FCN2 (mx_inline_cummin, <)
OP_CUMMINMAX_FCN2 (mx_inline_cummax, >)

#define OP_CUMMINMAX_FCNN(F)                                    \
  template <typename T>                                         \
  inline void                                                   \
  F (const T *v, T *r, octave_idx_type l,                       \
     octave_idx_type n, octave_idx_type u)                      \
  {                                                             \
    if (! n)                                                    \
      return;                                                   \
    if (l == 1)                                                 \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, n);                                        \
            v += n;                                             \
            r += n;                                             \
          }                                                     \
      }                                                         \
    else                                                        \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, l, n);                                     \
            v += l*n;                                           \
            r += l*n;                                           \
          }                                                     \
      }                                                         \
  }                                                             \
  template <typename T>                                         \
  inline void                                                   \
  F (const T *v, T *r, octave_idx_type *ri,                     \
     octave_idx_type l, octave_idx_type n, octave_idx_type u)   \
  {                                                             \
    if (! n)                                                    \
      return;                                                   \
    if (l == 1)                                                 \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, ri, n);                                    \
            v += n;                                             \
            r += n;                                             \
            ri += n;                                            \
          }                                                     \
      }                                                         \
    else                                                        \
      {                                                         \
        for (octave_idx_type i = 0; i < u; i++)                 \
          {                                                     \
            F (v, r, ri, l, n);                                 \
            v += l*n;                                           \
            r += l*n;                                           \
            ri += l*n;                                          \
          }                                                     \
      }                                                         \
  }

OP_CUMMINMAX_FCNN (mx_inline_cummin)
OP_CUMMINMAX_FCNN (mx_inline_cummax)

template <typename T>
void mx_inline_diff (const T *v, T *r, octave_idx_type n,
                     octave_idx_type order)
{
  switch (order)
    {
    case 1:
      for (octave_idx_type i = 0; i < n-1; i++)
        r[i] = v[i+1] - v[i];
      break;
    case 2:
      if (n > 1)
        {
          T lst = v[1] - v[0];
          for (octave_idx_type i = 0; i < n-2; i++)
            {
              T dif = v[i+2] - v[i+1];
              r[i] = dif - lst;
              lst = dif;
            }
        }
      break;
    default:
      {
        OCTAVE_LOCAL_BUFFER (T, buf, n-1);

        for (octave_idx_type i = 0; i < n-1; i++)
          buf[i] = v[i+1] - v[i];

        for (octave_idx_type o = 2; o <= order; o++)
          {
            for (octave_idx_type i = 0; i < n-o; i++)
              buf[i] = buf[i+1] - buf[i];
          }

        for (octave_idx_type i = 0; i < n-order; i++)
          r[i] = buf[i];
      }
    }
}

template <typename T>
void mx_inline_diff (const T *v, T *r,
                     octave_idx_type m, octave_idx_type n,
                     octave_idx_type order)
{
  switch (order)
    {
    case 1:
      for (octave_idx_type i = 0; i < m*(n-1); i++)
        r[i] = v[i+m] - v[i];
      break;
    case 2:
      for (octave_idx_type i = 0; i < n-2; i++)
        {
          for (octave_idx_type j = i*m; j < i*m+m; j++)
            r[j] = (v[j+m+m] - v[j+m]) - (v[j+m] - v[j]);
        }
      break;
    default:
      {
        OCTAVE_LOCAL_BUFFER (T, buf, n-1);

        for (octave_idx_type j = 0; j < m; j++)
          {
            for (octave_idx_type i = 0; i < n-1; i++)
              buf[i] = v[i*m+j+m] - v[i*m+j];

            for (octave_idx_type o = 2; o <= order; o++)
              {
                for (octave_idx_type i = 0; i < n-o; i++)
                  buf[i] = buf[i+1] - buf[i];
              }

            for (octave_idx_type i = 0; i < n-order; i++)
              r[i*m+j] = buf[i];
          }
      }
    }
}

template <typename T>
inline void
mx_inline_diff (const T *v, T *r,
                octave_idx_type l, octave_idx_type n, octave_idx_type u,
                octave_idx_type order)
{
  if (! n) return;
  if (l == 1)
    {
      for (octave_idx_type i = 0; i < u; i++)
        {
          mx_inline_diff (v, r, n, order);
          v += n; r += n-order;
        }
    }
  else
    {
      for (octave_idx_type i = 0; i < u; i++)
        {
          mx_inline_diff (v, r, l, n, order);
          v += l*n;
          r += l*(n-order);
        }
    }
}

// Assistant function

inline void
get_extent_triplet (const dim_vector& dims, int& dim,
                    octave_idx_type& l, octave_idx_type& n,
                    octave_idx_type& u)
{
  octave_idx_type ndims = dims.ndims ();
  if (dim >= ndims)
    {
      l = dims.numel ();
      n = 1;
      u = 1;
    }
  else
    {
      if (dim < 0)
        dim = dims.first_non_singleton ();

      // calculate extent triplet.
      l = 1, n = dims(dim), u = 1;
      for (octave_idx_type i = 0; i < dim; i++)
        l *= dims(i);
      for (octave_idx_type i = dim + 1; i < ndims; i++)
        u *= dims(i);
    }
}

// Appliers.
// FIXME: is this the best design? C++ gives a lot of options here...
// maybe it can be done without an explicit parameter?

template <typename R, typename T>
inline Array<R>
do_mx_red_op (const Array<T>& src, int dim,
              void (*mx_red_op) (const T *, R *, octave_idx_type,
                                 octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  dim_vector dims = src.dims ();
  // M*b inconsistency: sum ([]) = 0 etc.
  if (dims.ndims () == 2 && dims(0) == 0 && dims(1) == 0)
    dims(1) = 1;

  get_extent_triplet (dims, dim, l, n, u);

  // Reduction operation reduces the array size.
  if (dim < dims.ndims ()) dims(dim) = 1;
  dims.chop_trailing_singletons ();

  Array<R> ret (dims);
  mx_red_op (src.data (), ret.fortran_vec (), l, n, u);

  return ret;
}

template <typename R, typename T>
inline Array<R>
do_mx_cum_op (const Array<T>& src, int dim,
              void (*mx_cum_op) (const T *, R *, octave_idx_type,
                                 octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  dim_vector dims = src.dims ();
  get_extent_triplet (dims, dim, l, n, u);

  // Cumulative operation doesn't reduce the array size.
  Array<R> ret (dims);
  mx_cum_op (src.data (), ret.fortran_vec (), l, n, u);

  return ret;
}

template <typename R>
inline Array<R>
do_mx_minmax_op (const Array<R>& src, int dim,
                 void (*mx_minmax_op) (const R *, R *, octave_idx_type,
                                       octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  dim_vector dims = src.dims ();
  get_extent_triplet (dims, dim, l, n, u);

  // If the dimension is zero, we don't do anything.
  if (dim < dims.ndims () && dims(dim) != 0) dims(dim) = 1;
  dims.chop_trailing_singletons ();

  Array<R> ret (dims);
  mx_minmax_op (src.data (), ret.fortran_vec (), l, n, u);

  return ret;
}

template <typename R>
inline Array<R>
do_mx_minmax_op (const Array<R>& src, Array<octave_idx_type>& idx, int dim,
                 void (*mx_minmax_op) (const R *, R *, octave_idx_type *,
                                       octave_idx_type, octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  dim_vector dims = src.dims ();
  get_extent_triplet (dims, dim, l, n, u);

  // If the dimension is zero, we don't do anything.
  if (dim < dims.ndims () && dims(dim) != 0) dims(dim) = 1;
  dims.chop_trailing_singletons ();

  Array<R> ret (dims);
  if (idx.dims () != dims) idx = Array<octave_idx_type> (dims);

  mx_minmax_op (src.data (), ret.fortran_vec (), idx.fortran_vec (),
                l, n, u);

  return ret;
}

template <typename R>
inline Array<R>
do_mx_cumminmax_op (const Array<R>& src, int dim,
                    void (*mx_cumminmax_op) (const R *, R *, octave_idx_type,
                                             octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  dim_vector dims = src.dims ();
  get_extent_triplet (dims, dim, l, n, u);

  Array<R> ret (dims);
  mx_cumminmax_op (src.data (), ret.fortran_vec (), l, n, u);

  return ret;
}

template <typename R>
inline Array<R>
do_mx_cumminmax_op (const Array<R>& src, Array<octave_idx_type>& idx, int dim,
                    void (*mx_cumminmax_op) (const R *, R *, octave_idx_type *,
                                             octave_idx_type, octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  dim_vector dims = src.dims ();
  get_extent_triplet (dims, dim, l, n, u);

  Array<R> ret (dims);
  if (idx.dims () != dims) idx = Array<octave_idx_type> (dims);

  mx_cumminmax_op (src.data (), ret.fortran_vec (), idx.fortran_vec (),
                   l, n, u);

  return ret;
}

template <typename R>
inline Array<R>
do_mx_diff_op (const Array<R>& src, int dim, octave_idx_type order,
               void (*mx_diff_op) (const R *, R *,
                                   octave_idx_type, octave_idx_type,
                                   octave_idx_type, octave_idx_type))
{
  octave_idx_type l, n, u;
  if (order <= 0)
    return src;

  dim_vector dims = src.dims ();

  get_extent_triplet (dims, dim, l, n, u);
  if (dim >= dims.ndims ())
    dims.resize (dim+1, 1);

  if (dims(dim) <= order)
    {
      dims(dim) = 0;
      return Array<R> (dims);
    }
  else
    {
      dims(dim) -= order;
    }

  Array<R> ret (dims);
  mx_diff_op (src.data (), ret.fortran_vec (), l, n, u, order);

  return ret;
}

// Fast extra-precise summation.  According to
// T. Ogita, S. M. Rump, S. Oishi:
// Accurate Sum And Dot Product,
// SIAM J. Sci. Computing, Vol. 26, 2005

template <typename T>
inline void twosum_accum (T& s, T& e,
                          const T& x)
{
  T s1 = s + x;
  T t = s1 - s;
  T e1 = (s - (s1 - t)) + (x - t);
  s = s1;
  e += e1;
}

template <typename T>
inline T
mx_inline_xsum (const T *v, octave_idx_type n)
{
  T s, e;
  s = e = 0;
  for (octave_idx_type i = 0; i < n; i++)
    twosum_accum (s, e, v[i]);

  return s + e;
}

template <typename T>
inline void
mx_inline_xsum (const T *v, T *r,
                octave_idx_type m, octave_idx_type n)
{
  OCTAVE_LOCAL_BUFFER (T, e, m);
  for (octave_idx_type i = 0; i < m; i++)
    e[i] = r[i] = T ();

  for (octave_idx_type j = 0; j < n; j++)
    {
      for (octave_idx_type i = 0; i < m; i++)
        twosum_accum (r[i], e[i], v[i]);

      v += m;
    }

  for (octave_idx_type i = 0; i < m; i++)
    r[i] += e[i];
}

OP_RED_FCNN (mx_inline_xsum, T, T)

#endif
