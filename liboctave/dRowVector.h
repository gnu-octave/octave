/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_RowVector_h)
#define octave_RowVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
RowVector : public MArray<double>
{
public:

  RowVector (void) : MArray<double> () { }

  explicit RowVector (octave_idx_type n) : MArray<double> (n) { }

  RowVector (octave_idx_type n, double val) : MArray<double> (n, val) { }

  RowVector (const RowVector& a) : MArray<double> (a) { }

  RowVector (const MArray<double>& a) : MArray<double> (a) { }

  RowVector& operator = (const RowVector& a)
    {
      MArray<double>::operator = (a);
      return *this;
    }

  bool operator == (const RowVector& a) const;
  bool operator != (const RowVector& a) const;

  // destructive insert/delete/reorder operations

  RowVector& insert (const RowVector& a, octave_idx_type c);

  RowVector& fill (double val);
  RowVector& fill (double val, octave_idx_type c1, octave_idx_type c2);

  RowVector append (const RowVector& a) const;

  ColumnVector transpose (void) const;

  friend OCTAVE_API RowVector real (const ComplexRowVector& a);
  friend OCTAVE_API RowVector imag (const ComplexRowVector& a);

  // resize is the destructive equivalent for this one

  RowVector extract (octave_idx_type c1, octave_idx_type c2) const;

  RowVector extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by matrix -> row vector

  friend OCTAVE_API RowVector operator * (const RowVector& a, const Matrix& b);

  // other operations

  typedef double (*dmapper) (double);
  typedef Complex (*cmapper) (const Complex&);

  RowVector map (dmapper fcn) const;
  ComplexRowVector map (cmapper fcn) const;

  double min (void) const;
  double max (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const RowVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, RowVector& a);

private:

  RowVector (double *d, octave_idx_type l) : MArray<double> (d, l) { }
};

// row vector by column vector -> scalar

double OCTAVE_API operator * (const RowVector& a, const ColumnVector& b);

Complex OCTAVE_API operator * (const RowVector& a, const ComplexColumnVector& b);

// other operations

OCTAVE_API RowVector linspace (double x1, double x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, RowVector, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
