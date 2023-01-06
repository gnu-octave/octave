////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

// Instantiate Arrays of float values.

#include "lo-mappers.h"
#include "Array.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave::idx_vector>;
extern template class Array<octave_idx_type>;

#include "Array-base.cc"
#include "oct-locbuf.h"

#define INLINE_ASCENDING_SORT 1
#define INLINE_DESCENDING_SORT 1
#include "oct-sort.cc"

template <>
inline bool
sort_isnan<float> (float x)
{
  return octave::math::isnan (x);
}

static bool
nan_ascending_compare (float x, float y)
{
  return octave::math::isnan (y) ? ! octave::math::isnan (x) : x < y;
}

static bool
nan_descending_compare (float x, float y)
{
  return octave::math::isnan (x) ? ! octave::math::isnan (y) : x > y;
}

Array<float>::compare_fcn_type
safe_comparator (sortmode mode, const Array<float>& a, bool allow_chk)
{
  Array<float>::compare_fcn_type result = nullptr;

  if (allow_chk)
    {
      octave_idx_type k = 0;
      for (; k < a.numel () && ! octave::math::isnan (a(k)); k++) ;
      if (k == a.numel ())
        {
          if (mode == ASCENDING)
            result = octave_sort<float>::ascending_compare;
          else if (mode == DESCENDING)
            result = octave_sort<float>::descending_compare;
        }
    }

  if (! result)
    {
      if (mode == ASCENDING)
        result = nan_ascending_compare;
      else if (mode == DESCENDING)
        result = nan_descending_compare;
    }

  return result;
}

// The default solution using NaN-safe comparator is OK, but almost twice as
// slow than this code.
template <>
OCTAVE_API
sortmode
Array<float>::issorted (sortmode mode) const
{
  octave_idx_type n = numel ();

  const float *el = data ();

  if (n <= 1)
    return (mode == UNSORTED) ? ASCENDING : mode;

  if (mode == UNSORTED)
    {
      // Auto-detect mode.
      if (el[n-1] < el[0] || octave::math::isnan (el[0]))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  if (mode == DESCENDING)
    {
      octave_idx_type j = 0;
      float r;
      // Sort out NaNs.
      do
        r = el[j++];
      while (octave::math::isnan (r) && j < n);

      // Orient the test so that NaN will not pass through.
      for (; j < n; j++)
        {
          if (r >= el[j])
            r = el[j];
          else
            {
              mode = UNSORTED;
              break;
            }
        }

    }
  else  // mode == ASCENDING
    {
      // Sort out NaNs.
      while (n > 0 && octave::math::isnan (el[n-1]))
        n--;

      if (n > 0)
        {
          // Orient the test so that NaN will not pass through.
          float r = el[0];
          for (octave_idx_type j = 1; j < n; j++)
            {
              if (r <= el[j])
                r = el[j];
              else
                {
                  mode = UNSORTED;
                  break;
                }
            }
        }
    }

  return mode;
}

template class octave_sort<float>;

INSTANTIATE_ARRAY (float, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);

template OCTAVE_API std::ostream& operator << (std::ostream&,
                                               const Array<float>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class DiagArray2<float>;
