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

#if !defined (octave_DiagMatrix_h)
#define octave_DiagMatrix_h 1

#include "MArray.h"

#include "dRowVector.h"
#include "dColVector.h"

#include "mx-defs.h"

extern "C++" {

class DiagMatrix : public MDiagArray<double>
{
friend class SVD;
friend class ComplexSVD;

public:

  DiagMatrix (void) : MDiagArray<double> () { }
  DiagMatrix (int n) : MDiagArray<double> (n) { }
  DiagMatrix (int n, double val) : MDiagArray<double> (n, val) { }
  DiagMatrix (int r, int c) : MDiagArray<double> (r, c) { }
  DiagMatrix (int r, int c, double val) : MDiagArray<double> (r, c, val) { }
  DiagMatrix (const RowVector& a) : MDiagArray<double> (a) { }
  DiagMatrix (const ColumnVector& a) : MDiagArray<double> (a) { }
  DiagMatrix (const MDiagArray<double>& a) : MDiagArray<double> (a) { }
  DiagMatrix (const DiagMatrix& a) : MDiagArray<double> (a) { }
//  DiagMatrix (double a) : MDiagArray<double> (1, a) { }

  DiagMatrix& operator = (const DiagMatrix& a)
    {
      MDiagArray<double>::operator = (a);
      return *this;
    }

  int operator == (const DiagMatrix& a) const;
  int operator != (const DiagMatrix& a) const;

  DiagMatrix& fill (double val);
  DiagMatrix& fill (double val, int beg, int end);
  DiagMatrix& fill (const ColumnVector& a);
  DiagMatrix& fill (const RowVector& a);
  DiagMatrix& fill (const ColumnVector& a, int beg);
  DiagMatrix& fill (const RowVector& a, int beg);

  DiagMatrix transpose (void) const;

  friend DiagMatrix real (const ComplexDiagMatrix& a);
  friend DiagMatrix imag (const ComplexDiagMatrix& a);

// resize is the destructive analog for this one

  Matrix extract (int r1, int c1, int r2, int c2) const;

// extract row or column i.

  RowVector row (int i) const;
  RowVector row (char *s) const;

  ColumnVector column (int i) const;
  ColumnVector column (char *s) const;

  DiagMatrix inverse (void) const;
  DiagMatrix inverse (int& info) const;

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  DiagMatrix& operator += (const DiagMatrix& a);
  DiagMatrix& operator -= (const DiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  friend DiagMatrix operator * (const DiagMatrix& a,
				const DiagMatrix& b);

// other operations

  ColumnVector diag (void) const;
  ColumnVector diag (int k) const;

// i/o

  friend ostream& operator << (ostream& os, const DiagMatrix& a);

private:

  DiagMatrix (double *d, int nr, int nc) : MDiagArray<double> (d, nr, nc) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
