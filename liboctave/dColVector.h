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

#if !defined (octave_ColumnVector_h)
#define octave_ColumnVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
ColumnVector : public MArray<double>
{
public:

  ColumnVector (void) : MArray<double> () { }

  explicit ColumnVector (octave_idx_type n) : MArray<double> (n) { }

  ColumnVector (octave_idx_type n, double val) : MArray<double> (n, val) { }

  ColumnVector (const ColumnVector& a) : MArray<double> (a) { }

  ColumnVector (const MArray<double>& a) : MArray<double> (a) { }

  ColumnVector& operator = (const ColumnVector& a)
    {
      MArray<double>::operator = (a);
      return *this;
    }

  bool operator == (const ColumnVector& a) const;
  bool operator != (const ColumnVector& a) const;

  // destructive insert/delete/reorder operations

  ColumnVector& insert (const ColumnVector& a, octave_idx_type r);

  ColumnVector& fill (double val);
  ColumnVector& fill (double val, octave_idx_type r1, octave_idx_type r2);

  ColumnVector stack (const ColumnVector& a) const;

  RowVector transpose (void) const;

  friend OCTAVE_API ColumnVector real (const ComplexColumnVector& a);
  friend OCTAVE_API ColumnVector imag (const ComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  ColumnVector extract (octave_idx_type r1, octave_idx_type r2) const;

  ColumnVector extract_n (octave_idx_type r1, octave_idx_type n) const;

  // matrix by column vector -> column vector operations

  friend OCTAVE_API ColumnVector operator * (const Matrix& a, const ColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend OCTAVE_API ColumnVector operator * (const DiagMatrix& a, const ColumnVector& b);

  // other operations

  typedef double (*dmapper) (double);
  typedef Complex (*cmapper) (const Complex&);

  ColumnVector map (dmapper fcn) const;
  ComplexColumnVector map (cmapper fcn) const;

  double min (void) const;
  double max (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const ColumnVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, ColumnVector& a);

private:

  ColumnVector (double *d, octave_idx_type l) : MArray<double> (d, l) { }
};

// Publish externally used friend functions.

extern OCTAVE_API ColumnVector real (const ComplexColumnVector& a);
extern OCTAVE_API ColumnVector imag (const ComplexColumnVector& a);

MARRAY_FORWARD_DEFS (MArray, ColumnVector, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
