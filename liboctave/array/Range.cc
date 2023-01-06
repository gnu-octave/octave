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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include <istream>
#include <limits>
#include <ostream>

#include "Array-util.h"
#include "Range.h"
#include "lo-error.h"
#include "lo-mappers.h"
#include "lo-utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename T>
T xtfloor (T x, T ct)
{
  // C---------FLOOR(X) is the largest integer algebraically less than
  // C         or equal to X; that is, the unfuzzy FLOOR function.

  //  DINT (X) = X - DMOD (X, 1.0);
  //  FLOOR (X) = DINT (X) - DMOD (2.0 + DSIGN (1.0, X), 3.0);

  // C---------Hagerty's FL5 function follows...

  T q = 1;

  if (x < 0)
    q = 1 - ct;

  T rmax = q / (2 - ct);

  T t1 = 1 + std::floor (x);
  t1 = (ct / q) * (t1 < 0 ? -t1 : t1);
  t1 = (rmax < t1 ? rmax : t1);
  t1 = (ct > t1 ? ct : t1);
  t1 = std::floor (x + t1);

  if (x <= 0 || (t1 - x) < rmax)
    return t1;
  else
    return t1 - 1;
}

template <typename T>
bool xteq (T u, T v, T ct = 3 * std::numeric_limits<T>::epsilon ())
{
  T tu = std::abs (u);
  T tv = std::abs (v);

  return std::abs (u - v) < ((tu > tv ? tu : tv) * ct);
}

template <typename T>
octave_idx_type xnumel_internal (T base, T limit, T inc)
{
  octave_idx_type retval = -1;
  if (! math::isfinite (base) || ! math::isfinite (inc)
      || math::isnan (limit))
    retval = -2;
  else if (math::isinf (limit)
           && ((inc > 0 && limit > 0)
               || (inc < 0 && limit < 0)))
    retval = std::numeric_limits<octave_idx_type>::max () - 1;
  else if (inc == 0
           || (limit > base && inc < 0)
           || (limit < base && inc > 0))
    {
      retval = 0;
    }
  else
    {
      T ct = 3 * std::numeric_limits<T>::epsilon ();

      T tmp = xtfloor ((limit - base + inc) / inc, ct);

      octave_idx_type n_elt
        = (tmp > 0 ? static_cast<octave_idx_type> (tmp) : 0);

      // If the final element that we would compute for the range is
      // equal to the limit of the range, or is an adjacent floating
      // point number, accept it.  Otherwise, try a range with one
      // fewer element.  If that fails, try again with one more
      // element.
      //
      // I'm not sure this is very good, but it seems to work better
      // than just using tfloor as above.  For example, without it,
      // the expression 1.8:0.05:1.9 fails to produce the expected
      // result of [1.8, 1.85, 1.9].

      if (! xteq (base + (n_elt - 1) * inc, limit))
        {
          if (xteq (base + (n_elt - 2) * inc, limit))
            n_elt--;
          else if (xteq (base + n_elt * inc, limit))
            n_elt++;
        }

      retval = (n_elt < std::numeric_limits<octave_idx_type>::max () - 1
                ? n_elt : -1);
    }

  return retval;
}

template <typename T>
bool xall_elements_are_ints (T base, T inc, T final_val, octave_idx_type nel)
{
  // If the range is empty or NaN then there are no elements so there
  // can be no int elements.
  if (nel == 0 || math::isnan (final_val))
    return false;

  // If the base and increment are ints, all elements will be
  // integers.
  if (math::nint_big (base) == base && math::nint_big (inc) == inc)
    return true;

  // If the range has only one element, then the base needs to be an
  // integer.
  if (nel == 1 && math::nint_big (base))
    return true;

  return false;
}

template <typename T>
T
xfinal_value (T base, T limit, T inc, octave_idx_type nel)
{
  T retval = T (0);

  if (nel <= 1)
    return base;

  // If increment is 0, then numel should also be zero.

  retval = base + (nel - 1) * inc;

  // On some machines (x86 with extended precision floating point
  // arithmetic, for example) it is possible that we can overshoot
  // the limit by approximately the machine precision even though
  // we were very careful in our calculation of the number of
  // elements.  Therefore, we clip the result to the limit if it
  // overshoots.

  // NOTE: The test also includes equality (>= limit) to have
  // expressions such as -5:1:-0 result in a -0 endpoint.

  if ((inc > T (0) && retval >= limit) || (inc < T (0) && retval <= limit))
    retval = limit;

  // If all elements are integers, then ensure the final value is.
  // Note that we pass the preliminary computed final value to
  // xall_elements_are_ints, but it only checks whether that value is
  // NaN.

  if (xall_elements_are_ints (base, inc, retval, nel))
    retval = std::round (retval);

  return retval;
}

template <typename T>
void
xinit (T base, T limit, T inc, bool reverse, T& final_val,
       octave_idx_type& nel)
{
  // Catch obvious NaN ranges.
  if (math::isnan (base) || math::isnan (limit) || math::isnan (inc))
    {
      final_val = numeric_limits<T>::NaN ();
      nel = 1;
      return;
    }

  // Floating point numbers are always signed
  if (reverse)
    inc = -inc;

  // Catch empty ranges.
  if (inc == 0
      || (limit < base && inc > 0)
      || (limit > base && inc < 0))
    {
      nel = 0;
      return;
    }

  // The following case also catches Inf values for increment when
  // there will be only one element.

  if ((limit <= base && base + inc < limit)
      || (limit >= base && base + inc > limit))
    {
      final_val = base;
      nel = 1;
      return;
    }

  // Any other calculations with Inf will give us either a NaN range
  // or an infinite nember of elements.

  T dnel = (limit - base) / inc;
  if (math::isnan (dnel))
    {
      nel = 1;
      final_val = numeric_limits<T>::NaN ();
      return;
    }

  if (dnel > 0 && math::isinf (dnel))
    {
      // FIXME: Should this be an immediate error?
      nel = std::numeric_limits<octave_idx_type>::max ();

      // FIXME: Will this do the right thing in all cases?
      final_val = xfinal_value (base, limit, inc, nel);

      return;
    }

  // Now that we have handled all the special cases, we can compute
  // the number of elements and the final value in a way that attempts
  // to avoid rounding errors as much as possible.

  nel = xnumel_internal (base, limit, inc);
  final_val = xfinal_value (base, limit, inc, nel);
}

template <typename T>
void
xinit (const octave_int<T>& base, const octave_int<T>& limit,
       const octave_int<T>& inc, bool reverse,
       octave_int<T>& final_val, octave_idx_type& nel)
{
  // We need an integer division that is truncating decimals instead
  // of rounding.  So, use underlying C++ types instead of
  // octave_int<T>.

  // FIXME: The numerator might underflow or overflow. Add checks for
  // that.
  if (reverse)
    {
      nel = ((inc == octave_int<T> (0)
              || (limit > base && inc > octave_int<T> (0))
              || (limit < base && inc < octave_int<T> (0)))
             ? 0
             : (base.value () - limit.value () + inc.value ())
             / inc.value ());

      final_val = base - (nel - 1) * inc;
    }
  else
    {
      nel = ((inc == octave_int<T> (0)
              || (limit > base && inc < octave_int<T> (0))
              || (limit < base && inc > octave_int<T> (0)))
             ? 0
             : (limit.value () - base.value () + inc.value ())
             / inc.value ());

      final_val = base + (nel - 1) * inc;
    }
}

template <typename T>
bool
xis_storable (T base, T limit, octave_idx_type nel)
{
  return ! (nel > 1 && (math::isinf (base) || math::isinf (limit)));
}

template <>
bool
range<double>::all_elements_are_ints (void) const
{
  return xall_elements_are_ints (m_base, m_increment, m_final, m_numel);
}

template <>
bool
range<float>::all_elements_are_ints (void) const
{
  return xall_elements_are_ints (m_base, m_increment, m_final, m_numel);
}

template <>
void
range<double>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<float>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

// For now, only define for float and double.

#if 0

template <>
void
range<octave_int8>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_int16>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_int32>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_int64>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_uint8>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_uint16>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_uint32>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<octave_uint64>::init (void)
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

#endif

template <>
bool
range<double>::is_storable (void) const
{
  return xis_storable (m_base, m_limit, m_numel);
}

template <>
bool
range<float>::is_storable (void) const
{
  return xis_storable (m_base, m_limit, m_numel);
}

template <typename T>
octave_idx_type
xnnz (T base, T limit, T inc, T final_val, octave_idx_type nel)
{
  // Note that the order of the following checks matters.

  // If there are no elements, there can be no non-zero elements.
  if (nel == 0)
    return 0;

  // All elements have the same sign, hence there are no zeros.
  if ((base > 0 && limit > 0) || (base < 0 && limit < 0))
    return nel;

  // All elements are equal (inc = 0) but we know from the previous
  // condition that they are not positive or negative, therefore all
  // elements are zero.
  if (inc == 0)
    return 0;

  // Exactly one zero at beginning or end of range.
  if (base == 0 || final_val == 0)
    return nel - 1;

  // Range crosses negative/positive without hitting zero.
  // FIXME: Is this test sufficiently tolerant or do we need to be
  // more careful?
  if (math::mod (-base, inc) != 0)
    return nel;

  // Range crosses negative/positive and hits zero.
  return nel - 1;
}

template <>
octave_idx_type
range<double>::nnz (void) const
{
  return xnnz (m_base, m_limit, m_increment, m_final, m_numel);
}

template <>
octave_idx_type
range<float>::nnz (void) const
{
  return xnnz (m_base, m_limit, m_increment, m_final, m_numel);
}

OCTAVE_END_NAMESPACE(octave)

bool
Range::all_elements_are_ints (void) const
{
  // If the base and increment are ints, the final value in the range will also
  // be an integer, even if the limit is not.  If there is one or fewer
  // elements only the base needs to be an integer.

  return (! (octave::math::isnan (m_base) || octave::math::isnan (m_inc))
          && (octave::math::nint_big (m_base) == m_base || m_numel < 1)
          && (octave::math::nint_big (m_inc) == m_inc || m_numel <= 1));
}

octave_idx_type
Range::nnz (void) const
{
  octave_idx_type retval = 0;

  if (! isempty ())
    {
      if ((m_base > 0.0 && m_limit > 0.0) || (m_base < 0.0 && m_limit < 0.0))
        {
          // All elements have the same sign, hence there are no zeros.
          retval = m_numel;
        }
      else if (m_inc != 0.0)
        {
          if (m_base == 0.0 || m_limit == 0.0)
            // Exactly one zero at beginning or end of range.
            retval = m_numel - 1;
          else if ((m_base / m_inc) != std::floor (m_base / m_inc))
            // Range crosses negative/positive without hitting zero.
            retval = m_numel;
          else
            // Range crosses negative/positive and hits zero.
            retval = m_numel - 1;
        }
      else
        {
          // All elements are equal (m_inc = 0) but not positive or negative,
          // therefore all elements are zero.
          retval = 0;
        }
    }

  return retval;
}

Matrix
Range::matrix_value (void) const
{
  Matrix retval (1, m_numel);

  if (m_numel > 0)
    {
      // The first element must always be *exactly* the base.
      // E.g, -0 would otherwise become +0 in the loop (-0 + 0*increment).
      retval(0) = m_base;

      double b = m_base;
      double increment = m_inc;
      for (octave_idx_type i = 1; i < m_numel - 1; i++)
        retval.xelem (i) = b + i * increment;

      retval.xelem (m_numel - 1) = m_limit;
    }

  return retval;
}

double
Range::checkelem (octave_idx_type i) const
{
  if (i < 0 || i >= m_numel)
    octave::err_index_out_of_range (2, 2, i+1, m_numel, dims ());

  if (i == 0)
    return m_base;
  else if (i < m_numel - 1)
    return m_base + i * m_inc;
  else
    return m_limit;
}

double
Range::checkelem (octave_idx_type i, octave_idx_type j) const
{
  // Ranges are *always* row vectors.
  if (i != 0)
    octave::err_index_out_of_range (1, 1, i+1, m_numel, dims ());

  return checkelem (j);
}

double
Range::elem (octave_idx_type i) const
{
  if (i == 0)
    return m_base;
  else if (i < m_numel - 1)
    return m_base + i * m_inc;
  else
    return m_limit;
}

Array<double>
Range::index (const octave::idx_vector& idx) const
{
  Array<double> retval;

  octave_idx_type n = m_numel;

  if (idx.is_colon ())
    {
      retval = matrix_value ().reshape (dim_vector (m_numel, 1));
    }
  else
    {
      if (idx.extent (n) != n)
        octave::err_index_out_of_range (1, 1, idx.extent (n), n, dims ()); // throws

      dim_vector idx_dims = idx.orig_dimensions ();
      octave_idx_type idx_len = idx.length (n);

      // taken from Array.cc.
      if (n != 1 && idx_dims.isvector ())
        idx_dims = dim_vector (1, idx_len);

      retval.clear (idx_dims);

      // Loop over all values in IDX, executing the lambda expression
      // for each index value.

      double *array = retval.fortran_vec ();

      idx.loop (n, [=, &array] (idx_vector i)
      {
        if (i == 0)
          *array++ = m_base;
        else if (i < m_numel - 1)
          *array++ = m_base + i * m_inc;
        else
          *array++ = m_limit;
      });
    }

  return retval;
}

// NOTE: max and min only return useful values if numel > 0.
//       do_minmax_body() in max.cc avoids calling Range::min/max if numel == 0.

double
Range::min (void) const
{
  double retval = 0.0;
  if (m_numel > 0)
    {
      if (m_inc > 0)
        retval = m_base;
      else
        {
          retval = m_base + (m_numel - 1) * m_inc;

          // Require '<=' test.  See note in max ().
          if (retval <= m_limit)
            retval = m_limit;
        }

    }
  return retval;
}

double
Range::max (void) const
{
  double retval = 0.0;
  if (m_numel > 0)
    {
      if (m_inc > 0)
        {
          retval = m_base + (m_numel - 1) * m_inc;

          // On some machines (x86 with extended precision floating point
          // arithmetic, for example) it is possible that we can overshoot the
          // limit by approximately the machine precision even though we were
          // very careful in our calculation of the number of elements.
          // Therefore, we clip the result to the limit if it overshoots.
          // The test also includes equality (>= m_limit) to have expressions
          // such as -5:1:-0 result in a -0 endpoint.
          if (retval >= m_limit)
            retval = m_limit;
        }
      else
        retval = m_base;
    }
  return retval;
}

void
Range::sort_internal (bool ascending)
{
  if ((ascending && m_base > m_limit && m_inc < 0.0)
      || (! ascending && m_base < m_limit && m_inc > 0.0))
    {
      std::swap (m_base, m_limit);
      m_inc = -m_inc;
    }
}

void
Range::sort_internal (Array<octave_idx_type>& sidx, bool ascending)
{
  octave_idx_type nel = numel ();

  sidx.resize (dim_vector (1, nel));

  octave_idx_type *psidx = sidx.fortran_vec ();

  bool reverse = false;

  if ((ascending && m_base > m_limit && m_inc < 0.0)
      || (! ascending && m_base < m_limit && m_inc > 0.0))
    {
      std::swap (m_base, m_limit);
      m_inc = -m_inc;
      reverse = true;
    }

  octave_idx_type tmp = (reverse ? nel - 1 : 0);
  octave_idx_type stp = (reverse ? -1 : 1);

  for (octave_idx_type i = 0; i < nel; i++, tmp += stp)
    psidx[i] = tmp;
}

Matrix
Range::diag (octave_idx_type k) const
{
  return matrix_value ().diag (k);
}

Range
Range::sort (octave_idx_type dim, sortmode mode) const
{
  Range retval = *this;

  if (dim == 1)
    {
      if (mode == ASCENDING)
        retval.sort_internal (true);
      else if (mode == DESCENDING)
        retval.sort_internal (false);
    }
  else if (dim != 0)
    (*current_liboctave_error_handler) ("Range::sort: invalid dimension");

  return retval;
}

Range
Range::sort (Array<octave_idx_type>& sidx, octave_idx_type dim,
             sortmode mode) const
{
  Range retval = *this;

  if (dim == 1)
    {
      if (mode == ASCENDING)
        retval.sort_internal (sidx, true);
      else if (mode == DESCENDING)
        retval.sort_internal (sidx, false);
    }
  else if (dim != 0)
    (*current_liboctave_error_handler) ("Range::sort: invalid dimension");

  return retval;
}

sortmode
Range::issorted (sortmode mode) const
{
  if (m_numel > 1 && m_inc > 0)
    mode = (mode == DESCENDING) ? UNSORTED : ASCENDING;
  else if (m_numel > 1 && m_inc < 0)
    mode = (mode == ASCENDING) ? UNSORTED : DESCENDING;
  else
    mode = (mode == UNSORTED) ? ASCENDING : mode;

  return mode;
}

void
Range::set_base (double b)
{
  if (m_base != b)
    {
      m_base = b;

      init ();
    }
}

void
Range::set_limit (double l)
{
  if (m_limit != l)
    {
      m_limit = l;

      init ();
    }
}

void
Range::set_inc (double i)
{
  if (m_inc != i)
    {
      m_inc = i;

      init ();
    }
}

std::ostream&
operator << (std::ostream& os, const Range& a)
{
  double b = a.base ();
  double increment = a.increment ();
  octave_idx_type nel = a.numel ();

  if (nel > 1)
    {
      // First element must be the base *exactly* (e.g., -0).
      os << b << ' ';
      for (octave_idx_type i = 1; i < nel-1; i++)
        os << b + i * increment << ' ';
    }

  // Print out the last element exactly, rather than a calculated last element.
  os << a.m_limit << "\n";

  return os;
}

std::istream&
operator >> (std::istream& is, Range& a)
{
  is >> a.m_base;
  if (is)
    {
      double tmp_limit;
      is >> tmp_limit;

      if (is)
        is >> a.m_inc;

      // Clip the m_limit to the true limit, rebuild numel, clear cache
      a.set_limit (tmp_limit);
    }

  return is;
}

// DEPRECATED in Octave 7.
Range operator - (const Range& r)
{
  return Range (-r.base (), -r.limit (), -r.increment (), r.numel ());
}

// DEPRECATED in Octave 7.
Range operator + (double x, const Range& r)
{
  return Range (x + r.base (), x + r.limit (), r.increment (), r.numel ());
}

// DEPRECATED in Octave 7.
Range operator + (const Range& r, double x)
{
  return Range (r.base () + x, r.limit () + x, r.increment (), r.numel ());
}

// DEPRECATED in Octave 7.
Range operator - (double x, const Range& r)
{
  return Range (x - r.base (), x - r.limit (), -r.increment (), r.numel ());
}

// DEPRECATED in Octave 7.
Range operator - (const Range& r, double x)
{
  return Range (r.base () - x, r.limit () - x, r.increment (), r.numel ());
}

// DEPRECATED in Octave 7.
Range operator * (double x, const Range& r)
{
  return Range (x * r.base (), x * r.limit (), x * r.increment (), r.numel ());
}

// DEPRECATED in Octave 7.
Range operator * (const Range& r, double x)
{
  return Range (r.base () * x, r.limit () * x, r.increment () * x, r.numel ());
}

// C  See Knuth, Art Of Computer Programming, Vol. 1, Problem 1.2.4-5.
// C
// C===Tolerant FLOOR function.
// C
// C    X  -  is given as a Double Precision argument to be operated on.
// C          It is assumed that X is represented with M mantissa bits.
// C    CT -  is   given   as   a   Comparison   Tolerance   such   that
// C          0.LT.CT.LE.3-SQRT(5)/2. If the relative difference between
// C          X and A whole number is  less  than  CT,  then  TFLOOR  is
// C          returned   as   this   whole   number.   By  treating  the
// C          floating-point numbers as a finite ordered set  note  that
// C          the  heuristic  EPS=2.**(-(M-1))   and   CT=3*EPS   causes
// C          arguments  of  TFLOOR/TCEIL to be treated as whole numbers
// C          if they are  exactly  whole  numbers  or  are  immediately
// C          adjacent to whole number representations.  Since EPS,  the
// C          "distance"  between  floating-point  numbers  on  the unit
// C          interval, and M, the number of bits in X'S mantissa, exist
// C          on  every  floating-point   computer,   TFLOOR/TCEIL   are
// C          consistently definable on every floating-point computer.
// C
// C          For more information see the following references:
// C    (1) P. E. Hagerty, "More On Fuzzy Floor And Ceiling," APL  QUOTE
// C        QUAD 8(4):20-24, June 1978. Note that TFLOOR=FL5.
// C    (2) L. M. Breed, "Definitions For Fuzzy Floor And Ceiling",  APL
// C        QUOTE QUAD 8(3):16-23, March 1978. This paper cites FL1 through
// C        FL5, the history of five years of evolutionary development of
// C        FL5 - the seven lines of code below - by open collaboration
// C        and corroboration of the mathematical-computing community.
// C
// C  Penn State University Center for Academic Computing
// C  H. D. Knoble - August, 1978.

static inline double
tfloor (double x, double ct)
{
// C---------FLOOR(X) is the largest integer algebraically less than
// C         or equal to X; that is, the unfuzzy FLOOR function.

//  DINT (X) = X - DMOD (X, 1.0);
//  FLOOR (X) = DINT (X) - DMOD (2.0 + DSIGN (1.0, X), 3.0);

// C---------Hagerty's FL5 function follows...

  double q = 1.0;

  if (x < 0.0)
    q = 1.0 - ct;

  double rmax = q / (2.0 - ct);

  double t1 = 1.0 + std::floor (x);
  t1 = (ct / q) * (t1 < 0.0 ? -t1 : t1);
  t1 = (rmax < t1 ? rmax : t1);
  t1 = (ct > t1 ? ct : t1);
  t1 = std::floor (x + t1);

  if (x <= 0.0 || (t1 - x) < rmax)
    return t1;
  else
    return t1 - 1.0;
}

static inline bool
teq (double u, double v,
     double ct = 3.0 * std::numeric_limits<double>::epsilon ())
{
  double tu = std::abs (u);
  double tv = std::abs (v);

  return std::abs (u - v) < ((tu > tv ? tu : tv) * ct);
}

octave_idx_type
Range::numel_internal (void) const
{
  octave_idx_type retval = -1;

  if (! octave::math::isfinite (m_base) || ! octave::math::isfinite (m_inc)
      || octave::math::isnan (m_limit))
    retval = -2;
  else if (octave::math::isinf (m_limit)
           && ((m_inc > 0 && m_limit > 0)
               || (m_inc < 0 && m_limit < 0)))
    retval = std::numeric_limits<octave_idx_type>::max () - 1;
  else if (m_inc == 0
           || (m_limit > m_base && m_inc < 0)
           || (m_limit < m_base && m_inc > 0))
    {
      retval = 0;
    }
  else
    {
      double ct = 3.0 * std::numeric_limits<double>::epsilon ();

      double tmp = tfloor ((m_limit - m_base + m_inc) / m_inc, ct);

      octave_idx_type n_elt = (tmp > 0.0
                               ? static_cast<octave_idx_type> (tmp) : 0);

      // If the final element that we would compute for the range is equal to
      // the limit of the range, or is an adjacent floating point number,
      // accept it.  Otherwise, try a range with one fewer element.  If that
      // fails, try again with one more element.
      //
      // I'm not sure this is very good, but it seems to work better than just
      // using tfloor as above.  For example, without it, the expression
      // 1.8:0.05:1.9 fails to produce the expected result of [1.8, 1.85, 1.9].

      if (! teq (m_base + (n_elt - 1) * m_inc, m_limit))
        {
          if (teq (m_base + (n_elt - 2) * m_inc, m_limit))
            n_elt--;
          else if (teq (m_base + n_elt * m_inc, m_limit))
            n_elt++;
        }

      retval = ((n_elt < std::numeric_limits<octave_idx_type>::max ())
                ? n_elt : -1);
    }

  return retval;
}

double
Range::limit_internal (void) const
{
  double new_limit = m_inc > 0 ? max () : min ();

  // If result must be an integer then force the new_limit to be one.
  if (all_elements_are_ints ())
    new_limit = std::round (new_limit);

  return new_limit;
}

void
Range::init (void)
{
  m_numel = numel_internal ();

  if (! octave::math::isinf (m_limit))
    m_limit = limit_internal ();
}
