/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_ColumnVector_h)
#define octave_ColumnVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
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

  friend ColumnVector real (const ComplexColumnVector& a);
  friend ColumnVector imag (const ComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  ColumnVector extract (octave_idx_type r1, octave_idx_type r2) const;

  ColumnVector extract_n (octave_idx_type r1, octave_idx_type n) const;

  // matrix by column vector -> column vector operations

  friend ColumnVector operator * (const Matrix& a, const ColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend ColumnVector operator * (const DiagMatrix& a, const ColumnVector& b);

  // other operations

  ColumnVector map (d_d_Mapper f) const;

  ColumnVector& apply (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const ColumnVector& a);
  friend std::istream& operator >> (std::istream& is, ColumnVector& a);

private:

  ColumnVector (double *d, octave_idx_type l) : MArray<double> (d, l) { }
};

MARRAY_FORWARD_DEFS (MArray, ColumnVector, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
