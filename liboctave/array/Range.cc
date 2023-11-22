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
range<double>::all_elements_are_ints () const
{
  return xall_elements_are_ints (m_base, m_increment, m_final, m_numel);
}

template <>
bool
range<float>::all_elements_are_ints () const
{
  return xall_elements_are_ints (m_base, m_increment, m_final, m_numel);
}

template <>
void
range<double>::init ()
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

template <>
void
range<float>::init ()
{
  xinit (m_base, m_limit, m_increment, m_reverse, m_final, m_numel);
}

// For now, only define for float and double.

template <>
bool
range<double>::is_storable () const
{
  return xis_storable (m_base, m_limit, m_numel);
}

template <>
bool
range<float>::is_storable () const
{
  return xis_storable (m_base, m_limit, m_numel);
}

template <typename T>
octave_idx_type
xnnz (T base, T limit, T inc, T final_val, octave_idx_type nel)
{
  // Note that the order of the following checks matters.

  // If there are no elements, there can be no nonzero elements.
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
range<double>::nnz () const
{
  return xnnz (m_base, m_limit, m_increment, m_final, m_numel);
}

template <>
octave_idx_type
range<float>::nnz () const
{
  return xnnz (m_base, m_limit, m_increment, m_final, m_numel);
}

OCTAVE_END_NAMESPACE(octave)
