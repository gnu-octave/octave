/*

Copyright (C) 1993-2017 John W. Eaton

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

#if ! defined (octave_Range_h)
#define octave_Range_h 1

#include "octave-config.h"

#include <iosfwd>

#include "dMatrix.h"
#include "oct-sort.h"

class
OCTAVE_API
Range
{
public:

  Range (void)
    : rng_base (0), rng_limit (0), rng_inc (0), rng_numel (0), cache (1, 0) { }

  Range (const Range& r)
    : rng_base (r.rng_base), rng_limit (r.rng_limit), rng_inc (r.rng_inc),
      rng_numel (r.rng_numel), cache (r.cache) { }

  Range (double b, double l)
    : rng_base (b), rng_limit (l), rng_inc (1),
      rng_numel (numel_internal ()), cache ()
  {
    rng_limit = limit_internal ();
  }

  Range (double b, double l, double i)
    : rng_base (b), rng_limit (l), rng_inc (i),
      rng_numel (numel_internal ()), cache ()
  {
    rng_limit = limit_internal ();
  }

  // For operators' usage (to preserve element count).
  Range (double b, double i, octave_idx_type n)
    : rng_base (b), rng_limit (b + (n-1) * i), rng_inc (i),
      rng_numel (n), cache ()
  {
    if (! octave::math::isfinite (b) || ! octave::math::isfinite (i)
        || ! octave::math::isfinite (rng_limit))
      rng_numel = -2;
    else
      {
        // Code below is only needed if the resulting range must be 100%
        // correctly constructed.  If the Range object created is only
        // a temporary one used by operators this may be unnecessary.

        rng_limit = limit_internal ();
      }
  }

  double base (void) const { return rng_base; }
  double limit (void) const { return rng_limit; }
  double inc (void) const { return rng_inc; }

  OCTAVE_DEPRECATED ("use 'numel' instead")
  octave_idx_type nelem (void) const { return numel (); }

  octave_idx_type numel (void) const { return rng_numel; }

  bool isempty (void) const { return numel () == 0; }

  OCTAVE_DEPRECATED ("use 'isempty' instead")
  bool is_empty (void) const
  { return isempty (); }

  bool all_elements_are_ints (void) const;

  Matrix matrix_value (void) const;

  double min (void) const;
  double max (void) const;

  void sort_internal (bool ascending = true);
  void sort_internal (Array<octave_idx_type>& sidx, bool ascending = true);

  Matrix diag (octave_idx_type k = 0) const;

  Range sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;

  Range sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
              sortmode mode = ASCENDING) const;

  sortmode is_sorted (sortmode mode = ASCENDING) const;

  // Support for single-index subscripting, without generating matrix cache.

  double checkelem (octave_idx_type i) const;

  double elem (octave_idx_type i) const;

  double operator () (octave_idx_type i) const { return elem (i); }

  Array<double> index (const idx_vector& i) const;

  void set_base (double b);

  void set_limit (double l);

  void set_inc (double i);

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const Range& r);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, Range& r);

  friend OCTAVE_API Range operator - (const Range& r);
  friend OCTAVE_API Range operator + (double x, const Range& r);
  friend OCTAVE_API Range operator + (const Range& r, double x);
  friend OCTAVE_API Range operator - (double x, const Range& r);
  friend OCTAVE_API Range operator - (const Range& r, double x);
  friend OCTAVE_API Range operator * (double x, const Range& r);
  friend OCTAVE_API Range operator * (const Range& r, double x);

private:

  double rng_base;
  double rng_limit;
  double rng_inc;

  octave_idx_type rng_numel;

  mutable Matrix cache;

  octave_idx_type numel_internal (void) const;

  double limit_internal (void) const;

  void init (void);

  void clear_cache (void) const { cache.resize (0, 0); }

protected:

  // For operators' usage (to allow all values to be set directly).
  Range (double b, double l, double i, octave_idx_type n)
    : rng_base (b), rng_limit (l), rng_inc (i),
      rng_numel (n), cache ()
  {
    if (! octave::math::isfinite (b) || ! octave::math::isfinite (i)
        || ! octave::math::isfinite (l))
      rng_numel = -2;
  }
};

extern OCTAVE_API Range operator - (const Range& r);

extern OCTAVE_API Range operator + (double x, const Range& r);

extern OCTAVE_API Range operator + (const Range& r, double x);

extern OCTAVE_API Range operator - (double x, const Range& r);

extern OCTAVE_API Range operator - (const Range& r, double x);

extern OCTAVE_API Range operator * (double x, const Range& r);

extern OCTAVE_API Range operator * (const Range& r, double x);

#endif
