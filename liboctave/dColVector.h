//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if defined (__GNUG__)
#pragma interface
#endif

#include "MArray.h"

#include "mx-defs.h"

class ColumnVector : public MArray<double>
{
friend class Matrix;
friend class RowVector;

public:

  ColumnVector (void) : MArray<double> () { }
  ColumnVector (int n) : MArray<double> (n) { }
  ColumnVector (int n, double val) : MArray<double> (n, val) { }
  ColumnVector (const MArray<double>& a) : MArray<double> (a) { }
  ColumnVector (const ColumnVector& a) : MArray<double> (a) { }

  ColumnVector& operator = (const ColumnVector& a)
    {
      MArray<double>::operator = (a);
      return *this;
    }

  int operator == (const ColumnVector& a) const;
  int operator != (const ColumnVector& a) const;

// destructive insert/delete/reorder operations

  ColumnVector& insert (const ColumnVector& a, int r);

  ColumnVector& fill (double val);
  ColumnVector& fill (double val, int r1, int r2);

  ColumnVector stack (const ColumnVector& a) const;

  RowVector transpose (void) const;

  friend ColumnVector real (const ComplexColumnVector& a);
  friend ColumnVector imag (const ComplexColumnVector& a);

// resize is the destructive equivalent for this one

  ColumnVector extract (int r1, int r2) const;

// column vector by column vector -> column vector operations

  ColumnVector& operator += (const ColumnVector& a);
  ColumnVector& operator -= (const ColumnVector& a);

// matrix by column vector -> column vector operations

  friend ColumnVector operator * (const Matrix& a, const ColumnVector& b);

// diagonal matrix by column vector -> column vector operations

  friend ColumnVector operator * (const DiagMatrix& a, const ColumnVector& b);

// other operations

  friend ColumnVector map (d_d_Mapper f, const ColumnVector& a);
  friend ColumnVector map (d_c_Mapper f, const ComplexColumnVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ColumnVector& a);
  friend istream& operator >> (istream& is, ColumnVector& a);

private:

  ColumnVector (double *d, int l) : MArray<double> (d, l) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
