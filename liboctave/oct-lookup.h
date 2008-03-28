/*

Copyright (C) 2008 VZLU Prague, a.s., Czech Republic

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Author: Jaroslav Hajek <highegg@gmail.com>

#if !defined (octave_oct_lookup)
#define octave_oct_lookup 1

#include <algorithm>
#include <functional>

#include "oct-types.h"

// a simple binary lookup
template<typename T, typename bpred>
octave_idx_type
bin_lookup (const T *table, octave_idx_type size, 
            const T& val,
            bpred comp)
{
  return std::upper_bound (table, table + size, val, comp) - table;
}

// version using < operator
template<typename T>
octave_idx_type
bin_lookup (const T *table, octave_idx_type size,
            const T& val)
{
  return std::upper_bound (table, table + size, val) - table;
}

// a unary functor that checks whether a value is outside [a,b) range
template<class T, class bpred>
class out_range : public std::unary_function<T, bool>
{
public:
  out_range (const T& aa, const T& bb, const bpred& ccomp) 
    : a(aa), b(bb), comp(ccomp) { }

  bool operator() (const T& x) { return comp (x, a) || ! comp (x, b); }

private:
  T a;
  T b;

  bpred comp;
};

// conveniently constructs the above functor
// NOTE: with SGI extensions, this can be written as
// compose2 (logical_and<bool>(), 
//           bind2nd (less<T>(), a),
//           not1 (bind2nd (less<T>(), b)))
template<class T, class bpred>
out_range<T, bpred> 
chk_out_range (const T& a, const T& b, bpred comp)
{
  return out_range<T, bpred> (a, b, comp);
}

template<typename T, typename bpred>
void 
seq_lookup (const T *table, octave_idx_type offset, octave_idx_type size,
            const T *vals, octave_idx_type nvals,
            octave_idx_type *idx, bpred comp)
{
  const T *begin = table + offset;

  if (size == 0)
    // the trivial case of empty table
    std::fill_n (idx, nvals, offset);
  else
    {
      const T *vcur = vals;
      const T *vend = vals + nvals;

      const T *cur = begin;
      const T *end = begin + size;

      while (vcur < vend)
        {
          // determine the enclosing interval for next value, trying
          // ++cur as a special case;
          if (cur == end || comp (*vcur, *cur))
            cur = std::upper_bound (begin, cur, *vcur, comp);
          else
            {
              ++cur;
              if (cur < end && ! comp (*vcur, *cur))
                cur = std::upper_bound (cur + 1, end, *vcur, comp);
            }

          // store index of the current interval.
          *(idx++) = (cur - table);
          ++vcur;

          // find first value not in current subrange
          const T *vnew;
          if (cur < end)
            if (cur > begin)
              // inner interval
              vnew = std::find_if (vcur, vend,
                                   chk_out_range (*(cur-1), *cur, comp));

            else
              // special case: lowermost range (-Inf, min) 
              vnew = std::find_if (vcur, vend,
                                   not1 (bind2nd (comp, *cur)));
          else
            // special case: uppermost range [max, Inf)
            vnew = std::find_if (vcur, vend,
                                 bind2nd (comp, *(cur-1)));

          // store index of the current interval.
          idx = std::fill_n (idx, vnew - vcur, cur - table);
          vcur = vnew;

        }
    }
}

// overload using < operator
template<typename T, typename bpred>
void 
seq_lookup (const T *table, octave_idx_type offset, octave_idx_type size,
            const T *vals, octave_idx_type nvals,
            octave_idx_type *idx)
{
  seq_lookup (table, offset, size, vals, nvals, idx, std::less<T>());
}

// helper functions - determine whether an array is descending
template<typename T>
bool 
is_descending (const T *table, octave_idx_type size)
{
  return size > 1 && table[size-1] < table[0];
}

template<typename T, typename bpred>
bool 
is_descending (const T *table, octave_idx_type size,
                    bpred comp)
{
  return size > 1 && comp (table[size-1], table[0]);
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
