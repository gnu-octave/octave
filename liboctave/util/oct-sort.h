////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2026 The Octave Project Developers
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

#if ! defined (octave_oct_sort_h)
#define octave_oct_sort_h 1

#include "octave-config.h"

#include <functional>

#include "oct-traits.h"

// Enum for type of sort function
enum sortmode { UNSORTED = 0, ASCENDING, DESCENDING };

template <typename T>
class OCTAVE_TEMPLATE_API octave_sort
{
public:

  typedef std::function<bool (typename ref_param<T>::type,
                              typename ref_param<T>::type)> compare_fcn_type;

  octave_sort ();

  octave_sort (const compare_fcn_type&);

  OCTAVE_DISABLE_COPY_MOVE (octave_sort)

  ~octave_sort () = default;

  void set_compare (const compare_fcn_type& comp) { m_compare = comp; }

  void set_compare (sortmode mode);

  // Sort an array in-place.
  void sort (T *data, octave_idx_type nel);

  // Ditto, but also permute the passed indices (may not be valid indices).
  void sort (T *data, octave_idx_type *idx, octave_idx_type nel);

  // Check whether an array is sorted.
  bool issorted (const T *data, octave_idx_type nel);

  // Sort a matrix by rows, return a permutation vector.
  void sort_rows (const T *data, octave_idx_type *idx,
                  octave_idx_type rows, octave_idx_type cols);

  // Determine whether a matrix (as a contiguous block) is sorted by rows.
  bool is_sorted_rows (const T *data,
                       octave_idx_type rows, octave_idx_type cols);

  // Do a binary lookup in a sorted array.
  octave_idx_type lookup (const T *data, octave_idx_type nel,
                          const T& value);

  // Ditto, but for an array.
  void lookup (const T *data, octave_idx_type nel,
               const T *values, octave_idx_type nvalues,
               octave_idx_type *idx);

  // A linear merge of two sorted tables.  rev indicates the second table is
  // in reverse order.
  void lookup_sorted (const T *data, octave_idx_type nel,
                      const T *values, octave_idx_type nvalues,
                      octave_idx_type *idx, bool rev = false);

  // Rearranges the array so that the elements with indices
  // lo..up-1 are in their correct place.
  void nth_element (T *data, octave_idx_type nel,
                    octave_idx_type lo, octave_idx_type up = -1);

  static bool ascending_compare (typename ref_param<T>::type,
                                 typename ref_param<T>::type);

  static bool descending_compare (typename ref_param<T>::type,
                                  typename ref_param<T>::type);

private:

  compare_fcn_type m_compare;

  template <typename Comp>
  void sort (T *data, octave_idx_type nel, Comp comp);

  template <typename Comp>
  void sort (T *data, octave_idx_type *idx, octave_idx_type nel, Comp comp);

  template <typename Comp>
  bool issorted (const T *data, octave_idx_type nel, Comp comp);

  template <typename Comp>
  void sort_rows (const T *data, octave_idx_type *idx,
                  octave_idx_type rows, octave_idx_type cols,
                  Comp comp);

  template <typename Comp>
  bool is_sorted_rows (const T *data, octave_idx_type rows,
                       octave_idx_type cols, Comp comp);

  template <typename Comp>
  octave_idx_type lookup (const T *data, octave_idx_type nel,
                          const T& value, Comp comp);

  template <typename Comp>
  void lookup (const T *data, octave_idx_type nel,
               const T *values, octave_idx_type nvalues,
               octave_idx_type *idx, Comp comp);

  template <typename Comp>
  void lookup_sorted (const T *data, octave_idx_type nel,
                      const T *values, octave_idx_type nvalues,
                      octave_idx_type *idx, bool rev, Comp comp);

  template <typename Comp>
  void nth_element (T *data, octave_idx_type nel,
                    octave_idx_type lo, octave_idx_type up,
                    Comp comp);
};

#endif
