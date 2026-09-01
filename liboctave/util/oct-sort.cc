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

// This file should not include config.h.  It is only included in other
// C++ source files that should have included config.h before including
// this file.

#include <cstddef>

#include <algorithm>
#include <concepts>
#include <functional>
#include <limits>
#include <memory>
#include <stack>
#include <type_traits>
#include <utility>

#include "oct-error.h"
#include "oct-sort.h"

namespace octave_sort_detail
{
  inline constexpr std::size_t NUM_8BIT_VALUES = 256;
  inline constexpr std::size_t NUM_16BIT_VALUES = 65536;

  // For short arrays, clearing even the 8-bit histogram costs more than STL.
  inline constexpr octave_idx_type COUNTING_SORT_8BIT_THRESHOLD = 32;

  // Zeroing 65,536 counters costs enough that the O(N) 16-bit path only
  // starts winning for larger arrays.
  inline constexpr octave_idx_type COUNTING_SORT_16BIT_THRESHOLD = 131072;

  enum class standard_order
  {
    none,
    ascending,
    descending
  };

  template <typename T>
  using compare_fcn_ptr = bool (*) (typename ref_param<T>::type,
                                    typename ref_param<T>::type);

  template <typename T>
  bool
  is_ascending_compare (const typename octave_sort<T>::compare_fcn_type& comp)
  {
    const auto target = comp.template target<compare_fcn_ptr<T>> ();

    return target && *target == octave_sort<T>::ascending_compare;
  }

  template <typename T>
  bool
  is_descending_compare (const typename octave_sort<T>::compare_fcn_type& comp)
  {
    const auto target = comp.template target<compare_fcn_ptr<T>> ();

    return target && *target == octave_sort<T>::descending_compare;
  }

  template <typename T, typename Comp>
  constexpr standard_order
  order_for ()
  {
    using comp_type = std::remove_cvref_t<Comp>;

    if constexpr (std::same_as<comp_type, std::less<T>>)
      return standard_order::ascending;
    else if constexpr (std::same_as<comp_type, std::greater<T>>)
      return standard_order::descending;
    else
      return standard_order::none;
  }

  template <typename T>
  concept octave_int_like
    = requires (const T& value)
      {
        typename T::val_type;
        { value.value () } -> std::convertible_to<typename T::val_type>;
      }
      && std::integral<typename T::val_type>;

  template <typename T, bool = octave_int_like<T>>
  struct integer_traits
  {
    using value_type = T;

    static value_type value (const T& x) { return x; }
  };

  template <typename T>
  struct integer_traits<T, true>
  {
    using value_type = typename T::val_type;

    static value_type value (const T& x) { return x.value (); }
  };

  template <typename T>
  using integer_value_type = typename integer_traits<T>::value_type;

  template <typename T>
  concept counting_sortable_integer
    // bucket_index uses make_unsigned_t, which is intentionally undefined
    // for bool.
    = (std::integral<T> || octave_int_like<T>)
      && ! std::same_as<std::remove_cv_t<integer_value_type<T>>, bool>;

  template <typename T>
  concept byte_counting_sortable
    = counting_sortable_integer<T> && sizeof (integer_value_type<T>) == 1;

  template <typename T>
  concept word_counting_sortable
    = counting_sortable_integer<T> && sizeof (integer_value_type<T>) == 2;

  template <typename V>
  std::size_t
  bucket_index (V value)
  {
    using U = std::make_unsigned_t<V>;

    return static_cast<std::size_t> (static_cast<U> (value));
  }

  template <typename V, std::size_t nvalues, typename F>
  void
  for_each_value (bool ascending, F f)
  {
    if constexpr (std::is_signed_v<V>)
      {
        const int min_value = static_cast<int> (std::numeric_limits<V>::min ());
        const int max_value = static_cast<int> (std::numeric_limits<V>::max ());

        if (ascending)
          {
            for (int value = min_value; value <= max_value; value++)
              f (static_cast<V> (value));
          }
        else
          {
            for (int value = max_value; ; value--)
              {
                f (static_cast<V> (value));

                if (value == min_value)
                  break;
              }
          }
      }
    else
      {
        if (ascending)
          {
            for (std::size_t value = 0; value < nvalues; value++)
              f (static_cast<V> (value));
          }
        else
          {
            for (std::size_t value = nvalues; value-- > 0; )
              f (static_cast<V> (value));
          }
      }
  }

  template <typename T>
  void
  count_values (const T *data, octave_idx_type nel, octave_idx_type *counts)
  {
    using traits = integer_traits<T>;

    for (octave_idx_type i = 0; i < nel; i++)
      counts[bucket_index (traits::value (data[i]))]++;
  }

  template <typename T, std::size_t nvalues>
  void
  emit_counted_values (T *data, const octave_idx_type *counts, bool ascending)
  {
    using value_type = integer_value_type<T>;

    octave_idx_type pos = 0;

    for_each_value<value_type, nvalues>
      (ascending, [&] (value_type value)
       {
         const octave_idx_type count = counts[bucket_index (value)];

         if (count > 0)
           {
             std::fill_n (data + pos, count, T (value));
             pos += count;
           }
       });
  }

  template <typename T, std::size_t nvalues>
  void
  counts_to_offsets (octave_idx_type *counts, bool ascending)
  {
    using value_type = integer_value_type<T>;

    octave_idx_type pos = 0;

    for_each_value<value_type, nvalues>
      (ascending, [&] (value_type value)
       {
         const std::size_t bucket = bucket_index (value);

         const octave_idx_type count = counts[bucket];

         counts[bucket] = pos;
         pos += count;
       });
  }

  template <typename T, std::size_t nvalues>
  void
  emit_offset_values (T *data, const octave_idx_type *end_offsets,
                      bool ascending)
  {
    using value_type = integer_value_type<T>;

    octave_idx_type pos = 0;

    for_each_value<value_type, nvalues>
      (ascending, [&] (value_type value)
       {
         const octave_idx_type end = end_offsets[bucket_index (value)];

         std::fill (data + pos, data + end, T (value));
         pos = end;
       });
  }

  template <typename T, std::size_t nvalues>
  void
  counting_sort (T *data, octave_idx_type nel, bool ascending)
  {
    auto counts_owner
      = std::make_unique_for_overwrite<octave_idx_type []> (nvalues);
    octave_idx_type *counts = counts_owner.get ();

    std::fill_n (counts, nvalues, 0);
    count_values (data, nel, counts);
    emit_counted_values<T, nvalues> (data, counts, ascending);
  }

  template <typename T, std::size_t nvalues>
  void
  counting_sort (T *data, octave_idx_type *idx, octave_idx_type nel,
                 bool ascending)
  {
    using traits = integer_traits<T>;

    auto counts_owner
      = std::make_unique_for_overwrite<octave_idx_type []> (nvalues);
    auto sorted_idx_owner
      = std::make_unique_for_overwrite<octave_idx_type []> (nel);

    octave_idx_type *counts = counts_owner.get ();
    octave_idx_type *sorted_idx = sorted_idx_owner.get ();

    std::fill_n (counts, nvalues, 0);
    count_values (data, nel, counts);
    counts_to_offsets<T, nvalues> (counts, ascending);

    // Stable distribution advances every bucket's start offset to its end
    // offset.  Those end offsets also describe the sorted output values.
    for (octave_idx_type i = 0; i < nel; i++)
      sorted_idx[counts[bucket_index (traits::value (data[i]))]++] = idx[i];

    std::copy_n (sorted_idx, nel, idx);
    emit_offset_values<T, nvalues> (data, counts, ascending);
  }

  template <typename T, typename Comp, typename Sort>
  bool
  dispatch_counting_sort (octave_idx_type nel, const Comp&, Sort sort)
  {
    constexpr standard_order order = order_for<T, Comp> ();

    if constexpr (order != standard_order::none && byte_counting_sortable<T>)
      {
        if (nel >= COUNTING_SORT_8BIT_THRESHOLD)
          {
            sort.template operator()<NUM_8BIT_VALUES>
              (order == standard_order::ascending);
            return true;
          }
      }
    else if constexpr (order != standard_order::none
                       && word_counting_sortable<T>)
      {
        if (nel >= COUNTING_SORT_16BIT_THRESHOLD)
          {
            sort.template operator()<NUM_16BIT_VALUES>
              (order == standard_order::ascending);
            return true;
          }
      }

    return false;
  }

  template <typename T, typename Comp>
  bool
  maybe_counting_sort (T *data, octave_idx_type nel, const Comp& comp)
  {
    return dispatch_counting_sort<T>
      (nel, comp,
       [data, nel]<std::size_t nvalues> (bool ascending)
       {
         counting_sort<T, nvalues> (data, nel, ascending);
       });
  }

  template <typename T, typename Comp>
  bool
  maybe_counting_sort (T *data, octave_idx_type *idx, octave_idx_type nel,
                       const Comp& comp)
  {
    return dispatch_counting_sort<T>
      (nel, comp,
       [data, idx, nel]<std::size_t nvalues> (bool ascending)
       {
         counting_sort<T, nvalues> (data, idx, nel, ascending);
       });
  }

  template <typename T, typename Comp>
  bool
  handle_ordered_input (T *data, octave_idx_type nel, Comp& comp)
  {
    if (std::is_sorted (data, data + nel, std::ref (comp)))
      return true;

    auto reverse_comp = [&comp] (const T& a, const T& b)
    {
      return comp (b, a);
    };

    if (! std::is_sorted (data, data + nel, reverse_comp))
      return false;

    std::reverse (data, data + nel);
    return true;
  }

  template <typename T, typename Comp>
  bool
  handle_ordered_input (T *data, octave_idx_type *idx, octave_idx_type nel,
                        Comp& comp)
  {
    if (std::is_sorted (data, data + nel, std::ref (comp)))
      return true;

    auto reverse_comp = [&comp] (const T& a, const T& b)
    {
      return comp (b, a);
    };

    if (! std::is_sorted (data, data + nel, reverse_comp))
      return false;

    std::reverse (data, data + nel);
    std::reverse (idx, idx + nel);

    // Reversing the whole range also reverses each equivalent-value run.
    // Reverse those runs again to preserve indexed-sort stability.
    for (octave_idx_type first = 0; first < nel; )
      {
        octave_idx_type last = first + 1;

        while (last < nel
               && ! comp (data[first], data[last])
               && ! comp (data[last], data[first]))
          last++;

        std::reverse (data + first, data + last);
        std::reverse (idx + first, idx + last);
        first = last;
      }

    return true;
  }

  // Find an upper bound in a sorted tail by probing exponentially farther
  // from FIRST, then searching only the final window.  Dense lookups retain
  // linear behavior while sparse monotone lookups avoid scanning long gaps.
  template <typename T, typename Comp>
  const T *
  galloping_upper_bound (const T *first, const T *last, const T& value,
                         Comp& comp)
  {
    const octave_idx_type nel = last - first;

    if (nel == 0)
      return last;

    octave_idx_type lo = 0;
    octave_idx_type hi = 1;

    while (true)
      {
        const octave_idx_type probe = hi - 1;

        if (comp (value, first[probe]))
          return std::upper_bound (first + lo, first + probe, value,
                                   std::ref (comp));

        if (hi == nel)
          return last;

        lo = hi;
        hi = hi > nel / 2 ? nel : 2 * hi;
      }
  }

}

template <typename T>
octave_sort<T>::octave_sort ()
  : m_compare (ascending_compare)
{ }

template <typename T>
octave_sort<T>::octave_sort (const compare_fcn_type& comp)
  : m_compare (comp)
{ }

template <typename T>
void
octave_sort<T>::set_compare (sortmode mode)
{
  if (mode == ASCENDING)
    m_compare = ascending_compare;
  else if (mode == DESCENDING)
    m_compare = descending_compare;
  else
    m_compare = compare_fcn_type ();
}

template <typename T>
template <typename Comp>
void
octave_sort<T>::sort (T *data, octave_idx_type nel, Comp comp)
{
  if (nel <= 1)
    return;

  if (octave_sort_detail::handle_ordered_input (data, nel, comp))
    return;

  if (octave_sort_detail::maybe_counting_sort (data, nel, comp))
    return;

  std::sort (data, data + nel, comp);
}

template <typename T>
template <typename Comp>
void
octave_sort<T>::sort (T *data, octave_idx_type *idx, octave_idx_type nel,
                      Comp comp)
{
  if (nel <= 1)
    return;

  if (octave_sort_detail::handle_ordered_input (data, idx, nel, comp))
    return;

  if (octave_sort_detail::maybe_counting_sort (data, idx, nel, comp))
    return;

  struct sort_pair
  {
    T m_value;
    octave_idx_type m_index;
  };

  // Keeping values and indices together costs more scratch space than sorting
  // indices alone, but avoids cache-hostile indirect comparisons through data.
  auto pairs_owner = std::make_unique_for_overwrite<sort_pair []> (nel);
  sort_pair *pairs = pairs_owner.get ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      pairs[i].m_value = data[i];
      pairs[i].m_index = idx[i];
    }

  std::stable_sort (pairs, pairs + nel,
                    [&comp] (const sort_pair& a, const sort_pair& b)
                    {
                      return comp (a.m_value, b.m_value);
                    });

  for (octave_idx_type i = 0; i < nel; i++)
    {
      data[i] = pairs[i].m_value;
      idx[i] = pairs[i].m_index;
    }
}

template <typename T>
void
octave_sort<T>::sort (T *data, octave_idx_type nel)
{
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    sort (data, nel, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      sort (data, nel, std::greater<T> ());
    else
#endif
      if (m_compare)
        sort (data, nel, m_compare);
}

template <typename T>
void
octave_sort<T>::sort (T *data, octave_idx_type *idx, octave_idx_type nel)
{
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    sort (data, idx, nel, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      sort (data, idx, nel, std::greater<T> ());
    else
#endif
      if (m_compare)
        sort (data, idx, nel, m_compare);
}

template <typename T>
template <typename Comp>
bool
octave_sort<T>::issorted (const T *data, octave_idx_type nel, Comp comp)
{
  bool sorted = true;

  if (nel > 1)
    {
      for (octave_idx_type i = 1; i < nel; i++)
        {
          if (comp (data[i], data[i-1]))
            {
              sorted = false;
              break;
            }
        }
    }

  return sorted;
}

template <typename T>
bool
octave_sort<T>::issorted (const T *data, octave_idx_type nel)
{
  bool retval = false;
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    retval = issorted (data, nel, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      retval = issorted (data, nel, std::greater<T> ());
    else
#endif
      if (m_compare)
        retval = issorted (data, nel, m_compare);

  return retval;
}

struct sortrows_run_t
{
public:
  sortrows_run_t (octave_idx_type c, octave_idx_type o, octave_idx_type n)
    : col (c), ofs (o), nel (n) { }
  //--------
  octave_idx_type col, ofs, nel;
};

template <typename T>
template <typename Comp>
void
octave_sort<T>::sort_rows (const T *data, octave_idx_type *idx,
                           octave_idx_type rows, octave_idx_type cols,
                           Comp comp)
{
  for (octave_idx_type i = 0; i < rows; i++)
    idx[i] = i;

  if (cols == 0 || rows <= 1)
    return;

  auto buf_owner = std::make_unique_for_overwrite<T []> (rows);
  T *buf = buf_owner.get ();

  // This is a breadth-first traversal.
  typedef sortrows_run_t run_t;
  std::stack<run_t> runs;

  runs.push (run_t (0, 0, rows));

  while (! runs.empty ())
    {
      octave_idx_type col = runs.top ().col;
      octave_idx_type ofs = runs.top ().ofs;
      octave_idx_type nel = runs.top ().nel;
      runs.pop ();
      liboctave_panic_unless (nel > 1);

      T *lbuf = buf + ofs;
      const T *ldata = data + rows*col;
      octave_idx_type *lidx = idx + ofs;

      // Gather.
      for (octave_idx_type i = 0; i < nel; i++)
        lbuf[i] = ldata[lidx[i]];

      // Sort.
      sort (lbuf, lidx, nel, comp);

      // Identify constant runs and schedule subsorts.
      if (col < cols-1)
        {
          octave_idx_type lst = 0;
          for (octave_idx_type i = 0; i < nel; i++)
            {
              if (comp (lbuf[lst], lbuf[i]))
                {
                  if (i > lst + 1)
                    runs.push (run_t (col+1, ofs + lst, i - lst));
                  lst = i;
                }
            }
          if (nel > lst + 1)
            runs.push (run_t (col+1, ofs + lst, nel - lst));
        }
    }
}

template <typename T>
void
octave_sort<T>::sort_rows (const T *data, octave_idx_type *idx,
                           octave_idx_type rows, octave_idx_type cols)
{
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    sort_rows (data, idx, rows, cols, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      sort_rows (data, idx, rows, cols, std::greater<T> ());
    else
#endif
      if (m_compare)
        sort_rows (data, idx, rows, cols, m_compare);
}

template <typename T>
template <typename Comp>
bool
octave_sort<T>::is_sorted_rows (const T *data, octave_idx_type rows,
                                octave_idx_type cols, Comp comp)
{
  if (rows <= 1 || cols == 0)
    return true;

  // This is a breadth-first traversal.
  const T *lastrow = data + rows*(cols - 1);
  typedef std::pair<const T *, octave_idx_type> run_t;
  std::stack<run_t> runs;

  bool sorted = true;
  runs.push (run_t (data, rows));
  while (sorted && ! runs.empty ())
    {
      const T *lo = runs.top ().first;
      octave_idx_type n = runs.top ().second;
      runs.pop ();
      if (lo < lastrow)
        {
          // Not the final column.
          liboctave_panic_unless (n > 1);
          const T *hi = lo + n;
          const T *lst = lo;
          for (lo++; lo < hi; lo++)
            {
              if (comp (*lst, *lo))
                {
                  if (lo > lst + 1)
                    runs.push (run_t (lst + rows, lo - lst));
                  lst = lo;
                }
              else if (comp (*lo, *lst))
                break;

            }
          if (lo == hi)
            {
              if (lo > lst + 1)
                runs.push (run_t (lst + rows, lo - lst));
            }
          else
            {
              sorted = false;
              break;
            }
        }
      else
        // The final column - use fast code.
        sorted = issorted (lo, n, comp);
    }

  return sorted;
}

template <typename T>
bool
octave_sort<T>::is_sorted_rows (const T *data, octave_idx_type rows,
                                octave_idx_type cols)
{
  bool retval = false;
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    retval = is_sorted_rows (data, rows, cols, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      retval = is_sorted_rows (data, rows, cols, std::greater<T> ());
    else
#endif
      if (m_compare)
        retval = is_sorted_rows (data, rows, cols, m_compare);

  return retval;
}

template <typename T>
template <typename Comp>
octave_idx_type
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T& value, Comp comp)
{
  const T *ptr = std::upper_bound (data, data + nel, value, comp);

  return ptr - data;
}

template <typename T>
octave_idx_type
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T& value)
{
  octave_idx_type retval = 0;
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    retval = lookup (data, nel, value, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      retval = lookup (data, nel, value, std::greater<T> ());
    else
#endif
      if (m_compare)
        retval = lookup (data, nel, value, m_compare);

  return retval;
}

template <typename T>
template <typename Comp>
void
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T *values, octave_idx_type nvalues,
                        octave_idx_type *idx, Comp comp)
{
  // Use a sequence of binary lookups.
  // FIXME: Can this be sped up generally?  The sorted merge case is dealt with
  // elsewhere.
  for (octave_idx_type j = 0; j < nvalues; j++)
    idx[j] = lookup (data, nel, values[j], comp);
}

template <typename T>
void
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T *values, octave_idx_type nvalues,
                        octave_idx_type *idx)
{
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    lookup (data, nel, values, nvalues, idx, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      lookup (data, nel, values, nvalues, idx, std::greater<T> ());
    else
#endif
      if (m_compare)
        lookup (data, nel, values, nvalues, idx, m_compare);
}

template <typename T>
template <typename Comp>
void
octave_sort<T>::lookup_sorted (const T *data, octave_idx_type nel,
                               const T *values, octave_idx_type nvalues,
                               octave_idx_type *idx, bool rev, Comp comp)
{
  if (rev)
    {
      octave_idx_type i = 0;
      octave_idx_type j = nvalues - 1;

      if (nvalues > 0 && nel > 0)
        {
          while (true)
            {
              if (comp (values[j], data[i]))
                {
                  idx[j] = i;
                  if (--j < 0)
                    break;
                }
              else
                {
                  const T *ptr = octave_sort_detail::galloping_upper_bound
                    (data + i + 1, data + nel, values[j], comp);
                  i = ptr - data;
                  idx[j] = i;
                  if (--j < 0 || i == nel)
                    break;
                }
            }
        }

      for (; j >= 0; j--)
        idx[j] = i;
    }
  else
    {
      octave_idx_type i = 0;
      octave_idx_type j = 0;

      if (nvalues > 0 && nel > 0)
        {
          while (true)
            {
              if (comp (values[j], data[i]))
                {
                  idx[j] = i;
                  if (++j == nvalues)
                    break;
                }
              else
                {
                  const T *ptr = octave_sort_detail::galloping_upper_bound
                    (data + i + 1, data + nel, values[j], comp);
                  i = ptr - data;
                  idx[j] = i;
                  if (++j == nvalues || i == nel)
                    break;
                }
            }
        }

      for (; j != nvalues; j++)
        idx[j] = i;
    }
}

template <typename T>
void
octave_sort<T>::lookup_sorted (const T *data, octave_idx_type nel,
                               const T *values, octave_idx_type nvalues,
                               octave_idx_type *idx, bool rev)
{
#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    lookup_sorted (data, nel, values, nvalues, idx, rev, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      lookup_sorted (data, nel, values, nvalues, idx, rev, std::greater<T> ());
    else
#endif
      if (m_compare)
        lookup_sorted (data, nel, values, nvalues, idx, rev, m_compare);
}

template <typename T>
template <typename Comp>
void
octave_sort<T>::nth_element (T *data, octave_idx_type nel,
                             octave_idx_type lo, octave_idx_type up,
                             Comp comp)
{
  if (octave_sort_detail::maybe_counting_sort (data, nel, comp))
    return;

  // Simply wrap the STL algorithms.
  // FIXME: this will fail if we attempt to inline <,> for Complex.
  if (up == lo+1)
    std::nth_element (data, data + lo, data + nel, comp);
  else if (lo == 0)
    std::partial_sort (data, data + up, data + nel, comp);
  else
    {
      std::nth_element (data, data + lo, data + nel, comp);
      if (up == lo + 2)
        {
          // Finding two subsequent elements.
          std::swap (data[lo+1],
                     *std::min_element (data + lo + 1, data + nel, comp));
        }
      else
        std::partial_sort (data + lo + 1, data + up, data + nel, comp);
    }
}

template <typename T>
void
octave_sort<T>::nth_element (T *data, octave_idx_type nel,
                             octave_idx_type lo, octave_idx_type up)
{
  if (up < 0)
    up = lo + 1;

#if defined (INLINE_ASCENDING_SORT)
  if (octave_sort_detail::is_ascending_compare<T> (m_compare))
    nth_element (data, nel, lo, up, std::less<T> ());
  else
#endif
#if defined (INLINE_DESCENDING_SORT)
    if (octave_sort_detail::is_descending_compare<T> (m_compare))
      nth_element (data, nel, lo, up, std::greater<T> ());
    else
#endif
      if (m_compare)
        nth_element (data, nel, lo, up, m_compare);
}

template <typename T>
bool
octave_sort<T>::ascending_compare (typename ref_param<T>::type x,
                                   typename ref_param<T>::type y)
{
  return x < y;
}

template <typename T>
bool
octave_sort<T>::descending_compare (typename ref_param<T>::type x,
                                    typename ref_param<T>::type y)
{
  return x > y;
}
