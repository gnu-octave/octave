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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_ColumnVector_h)
#define octave_ColumnVector_h 1

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class ColumnVector : public Array<double>
{
friend class Matrix;
friend class RowVector;

public:

  ColumnVector (void) : Array<double> () { }
  ColumnVector (int n) : Array<double> (n) { }
  ColumnVector (int n, double val) : Array<double> (n, val) { }
  ColumnVector (const Array<double>& a) : Array<double> (a) { }
  ColumnVector (const ColumnVector& a) : Array<double> (a) { }

  ColumnVector& operator = (const ColumnVector& a)
    {
      Array<double>::operator = (a);
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

#define KLUDGE_VECTORS
#define TYPE double
#define KL_VEC_TYPE ColumnVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  ColumnVector (double *d, int l) : Array<double> (d, l) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
