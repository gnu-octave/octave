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

#if !defined (octave_RowVector_h)
#define octave_RowVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
RowVector : public MArray<double>
{
public:

  RowVector (void) : MArray<double> () { }

  explicit RowVector (int n) : MArray<double> (n) { }

  RowVector (int n, double val) : MArray<double> (n, val) { }

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

  RowVector& insert (const RowVector& a, int c);

  RowVector& fill (double val);
  RowVector& fill (double val, int c1, int c2);

  RowVector append (const RowVector& a) const;

  ColumnVector transpose (void) const;

  friend RowVector real (const ComplexRowVector& a);
  friend RowVector imag (const ComplexRowVector& a);

  // resize is the destructive equivalent for this one

  RowVector extract (int c1, int c2) const;

  RowVector extract_n (int c1, int n) const;

  // row vector by matrix -> row vector

  friend RowVector operator * (const RowVector& a, const Matrix& b);

  // other operations

  RowVector map (d_d_Mapper f) const;

  RowVector& apply (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const RowVector& a);
  friend std::istream& operator >> (std::istream& is, RowVector& a);

private:

  RowVector (double *d, int l) : MArray<double> (d, l) { }
};

// row vector by column vector -> scalar

double operator * (const RowVector& a, const ColumnVector& b);

Complex operator * (const RowVector& a, const ComplexColumnVector& b);

// other operations

RowVector linspace (double x1, double x2, int n);

MARRAY_FORWARD_DEFS (MArray, RowVector, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
