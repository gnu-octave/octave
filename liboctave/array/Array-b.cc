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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

// Instantiate Arrays of bool values.

#include "Array.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_EXTERN_TEMPLATE_API Array<octave::idx_vector>;
extern template class Array<octave_idx_type>;

#include "Array-base.cc"

#define INLINE_ASCENDING_SORT 1
#define INLINE_DESCENDING_SORT 1
#include "oct-sort.cc"

// Specialize bool sorting (aka stable partitioning).

template <bool desc>
static void do_bool_partition (bool *data, octave_idx_type nel)
{
  octave_idx_type k = 0;
  for (octave_idx_type i = 0; i < nel; i++)
    if (data[i] == desc)
      data[k++] = desc;
  for (octave_idx_type i = k; i < nel; i++)
    data[i] = ! desc;
}

template <bool desc>
static void do_bool_partition (bool *data, octave_idx_type *idx,
                               octave_idx_type nel)
{
  // FIXME: This is essentially a simple bucket sort.
  // Can it be efficiently done by std::partition?
  OCTAVE_LOCAL_BUFFER (octave_idx_type, jdx, nel);
  octave_idx_type k = 0;
  octave_idx_type l = 0;
  for (octave_idx_type i = 0; i < nel; i++)
    {
      if (data[i] == desc)
        {
          data[k] = desc;
          idx[k++] = idx[i];
        }
      else
        jdx[l++] = idx[i];
    }

  for (octave_idx_type i = k; i < nel; i++)
    {
      data[i] = ! desc;
      idx[i] = jdx[i-k];
    }
}

template <>
template <>
void
octave_sort<bool>::sort (bool *data, octave_idx_type nel,
                         std::less<bool>)
{
  do_bool_partition<false> (data, nel);
}

template <>
template <>
void
octave_sort<bool>::sort (bool *data, octave_idx_type nel,
                         std::greater<bool>)
{
  do_bool_partition<true> (data, nel);
}

template <>
template <>
void
octave_sort<bool>::sort (bool *data, octave_idx_type *idx, octave_idx_type nel,
                         std::less<bool>)
{
  do_bool_partition<false> (data, idx, nel);
}

template <>
template <>
void
octave_sort<bool>::sort (bool *data, octave_idx_type *idx, octave_idx_type nel,
                         std::greater<bool>)
{
  do_bool_partition<true> (data, idx, nel);
}

template class octave_sort<bool>;

INSTANTIATE_ARRAY (bool, OCTAVE_CLASS_TEMPLATE_INSTANTIATION_API);

template OCTAVE_API std::ostream& operator << (std::ostream&,
                                               const Array<bool>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class DiagArray2<bool>;
