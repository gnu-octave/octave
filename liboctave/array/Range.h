////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2020 The Octave Project Developers
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

#if ! defined (octave_Range_h)
#define octave_Range_h 1

#include "octave-config.h"

#include <iosfwd>

#include "dMatrix.h"
#include "dim-vector.h"
#include "oct-sort.h"

class
OCTAVE_API
Range
{
public:

  Range (void)
    : m_base (0), m_limit (0), m_inc (0), m_numel (0)
  { }

  Range (const Range& r) = default;

  Range& operator = (const Range& r) = default;

  ~Range (void) = default;

  Range (double b, double l)
    : m_base (b), m_limit (l), m_inc (1), m_numel (numel_internal ())
  {
    if (! octave::math::isinf (m_limit))
      m_limit = limit_internal ();
  }

  Range (double b, double l, double i)
    : m_base (b), m_limit (l), m_inc (i), m_numel (numel_internal ())
  {
    if (! octave::math::isinf (m_limit))
      m_limit = limit_internal ();
  }

  // NOTE: The following constructor may be deprecated and removed after
  // the arithmetic operators are removed.

  // For operators' usage (to preserve element count).

  Range (double b, double i, octave_idx_type n)
    : m_base (b), m_limit (b + (n-1) * i), m_inc (i), m_numel (n)
  {
    if (! octave::math::isinf (m_limit))
      m_limit = limit_internal ();
  }

  // The range has a finite number of elements.
  bool ok (void) const
  {
    return (octave::math::isfinite (m_limit)
            && (m_numel >= 0 || m_numel == -2));
  }

  double base (void) const { return m_base; }
  double limit (void) const { return m_limit; }
  double inc (void) const { return m_inc; }

  octave_idx_type numel (void) const { return m_numel; }

  dim_vector dims (void) const { return dim_vector (1, m_numel); }

  octave_idx_type rows (void) const { return 1; }

  octave_idx_type cols (void) const { return numel (); }
  octave_idx_type columns (void) const { return numel (); }

  bool isempty (void) const { return numel () == 0; }

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

  sortmode issorted (sortmode mode = ASCENDING) const;

  octave_idx_type nnz (void) const;

  // Support for single-index subscripting, without generating matrix cache.

  double checkelem (octave_idx_type i) const;
  double checkelem (octave_idx_type i, octave_idx_type j) const;

  double elem (octave_idx_type i) const;
  double elem (octave_idx_type /* i */, octave_idx_type j) const
  { return elem (j); }

  double operator () (octave_idx_type i) const { return elem (i); }
  double operator () (octave_idx_type i, octave_idx_type j) const
  { return elem (i, j); }

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

  double m_base;
  double m_limit;
  double m_inc;

  octave_idx_type m_numel;

  octave_idx_type numel_internal (void) const;

  double limit_internal (void) const;

  void init (void);

protected:

  // NOTE: The following constructor may be removed when the arithmetic
  // operators are removed.

  // For operators' usage (to allow all values to be set directly).
  Range (double b, double l, double i, octave_idx_type n)
    : m_base (b), m_limit (l), m_inc (i), m_numel (n)
  { }
};

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator - (const Range& r);

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator + (double x, const Range& r);

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator + (const Range& r, double x);

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator - (double x, const Range& r);

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator - (const Range& r, double x);

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator * (double x, const Range& r);

OCTAVE_DEPRECATED (7, "arithmetic operations on Range objects are unreliable")
extern OCTAVE_API Range operator * (const Range& r, double x);

#endif
