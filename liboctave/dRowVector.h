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

#if !defined (octave_RowVector_h)
#define octave_RowVector_h 1

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class RowVector : public Array<double>
{
friend class ColumnVector;
friend class ComplexRowVector;

public:

  RowVector (void) : Array<double> () { }
  RowVector (int n) : Array<double> (n) { }
  RowVector (int n, double val) : Array<double> (n, val) { }
  RowVector (const Array<double>& a) : Array<double> (a) { }
  RowVector (const RowVector& a) : Array<double> (a) { }
//  RowVector (double a) : Array<double> (1, a) { }

  RowVector& operator = (const RowVector& a)
    {
      Array<double>::operator = (a);
      return *this;
    }

//  operator Array<double>& () const { return *this; }

  int operator == (const RowVector& a) const;
  int operator != (const RowVector& a) const;

// destructive insert/delete/reorder operations

  RowVector& insert (const RowVector& a, int c);

  RowVector& fill (double val);
  RowVector& fill (double val, int c1, int c2);

  RowVector append (const RowVector& a) const;

  ColumnVector transpose (void) const;

// resize is the destructive equivalent for this one

  RowVector extract (int c1, int c2) const;

// row vector by row vector -> row vector operations

  RowVector& operator += (const RowVector& a);
  RowVector& operator -= (const RowVector& a);

// row vector by scalar -> row vector operations

  friend ComplexRowVector operator + (const RowVector& a, const Complex& s);
  friend ComplexRowVector operator - (const RowVector& a, const Complex& s);
  friend ComplexRowVector operator * (const RowVector& a, const Complex& s);
  friend ComplexRowVector operator / (const RowVector& a, const Complex& s);

// scalar by row vector -> row vector operations

  friend ComplexRowVector operator + (const Complex& s, const RowVector& a);
  friend ComplexRowVector operator - (const Complex& s, const RowVector& a);
  friend ComplexRowVector operator * (const Complex& s, const RowVector& a);
  friend ComplexRowVector operator / (const Complex& s, const RowVector& a);

// row vector by column vector -> scalar

  friend double operator * (const RowVector& a, const ColumnVector& b);

  friend Complex operator * (const RowVector& a, const ComplexColumnVector& b);

// row vector by matrix -> row vector

  friend RowVector operator * (const RowVector& a, const Matrix& b);

  friend ComplexRowVector operator * (const RowVector& a,
				      const ComplexMatrix& b);

// row vector by row vector -> row vector operations

  friend ComplexRowVector operator + (const RowVector& a,
				      const ComplexRowVector& b);
  friend ComplexRowVector operator - (const RowVector& a,
				      const ComplexRowVector& b);

  friend ComplexRowVector product (const RowVector& a,
				   const ComplexRowVector& b);
  friend ComplexRowVector quotient (const RowVector& a,
				    const ComplexRowVector& b);

// other operations

  friend RowVector map (d_d_Mapper f, const RowVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const RowVector& a);
  friend istream& operator >> (istream& is, RowVector& a);

#define KLUDGE_VECTORS
#define TYPE double
#define KL_VEC_TYPE RowVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  RowVector (double *d, int l) : Array<double> (d, l) { }
};

RowVector linspace (double x1, double x2, int n);

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
