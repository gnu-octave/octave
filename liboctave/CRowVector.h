//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_ComplexRowVector_h)
#define octave_ComplexRowVector_h 1

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class ComplexRowVector : public Array<Complex>
{
friend class ComplexColumnVector;
friend class RowVector;

public:

  ComplexRowVector (void) : Array<Complex> () { }
  ComplexRowVector (int n) : Array<Complex> (n) { }
  ComplexRowVector (int n, const Complex& val) : Array<Complex> (n, val) { }
  ComplexRowVector (const RowVector& a);
  ComplexRowVector (const Array<Complex>& a) : Array<Complex> (a) { }
  ComplexRowVector (const ComplexRowVector& a) : Array<Complex> (a) { }
//  ComplexRowVector (double a) : Array<Complex> (1, a) { }
//  ComplexRowVector (const Complex& a) : Array<Complex> (1, a) { }

  ComplexRowVector& operator = (const ComplexRowVector& a)
    {
      Array<Complex>::operator = (a);
      return *this;
    }

//  operator Array<Complex>& () const { return *this; }

  int operator == (const ComplexRowVector& a) const;
  int operator != (const ComplexRowVector& a) const;

// destructive insert/delete/reorder operations

  ComplexRowVector& insert (const RowVector& a, int c);
  ComplexRowVector& insert (const ComplexRowVector& a, int c);

  ComplexRowVector& fill (double val);
  ComplexRowVector& fill (const Complex& val);
  ComplexRowVector& fill (double val, int c1, int c2);
  ComplexRowVector& fill (const Complex& val, int c1, int c2);

  ComplexRowVector append (const RowVector& a) const;
  ComplexRowVector append (const ComplexRowVector& a) const;

  ComplexColumnVector hermitian (void) const;  // complex conjugate transpose.
  ComplexColumnVector transpose (void) const;

  friend RowVector real (const ComplexRowVector& a);
  friend RowVector imag (const ComplexRowVector& a);
  friend ComplexRowVector conj (const ComplexRowVector& a);

// resize is the destructive equivalent for this one

  ComplexRowVector extract (int c1, int c2) const;

// row vector by row vector -> row vector operations

  ComplexRowVector& operator += (const RowVector& a);
  ComplexRowVector& operator -= (const RowVector& a);

  ComplexRowVector& operator += (const ComplexRowVector& a);
  ComplexRowVector& operator -= (const ComplexRowVector& a);

// row vector by scalar -> row vector operations

  friend ComplexRowVector operator + (const ComplexRowVector& a, double s);
  friend ComplexRowVector operator - (const ComplexRowVector& a, double s);
  friend ComplexRowVector operator * (const ComplexRowVector& a, double s);
  friend ComplexRowVector operator / (const ComplexRowVector& a, double s);

// scalar by row vector -> row vector operations

  friend ComplexRowVector operator + (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator - (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator * (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator / (double s, const ComplexRowVector& a);

// row vector by column vector -> scalar

  friend Complex operator * (const ComplexRowVector& a, const ColumnVector& b);

  friend Complex operator * (const ComplexRowVector& a,
			     const ComplexColumnVector& b);

// row vector by matrix -> row vector

  friend ComplexRowVector operator * (const ComplexRowVector& a,
				      const ComplexMatrix& b);

// row vector by row vector -> row vector operations

  friend ComplexRowVector operator + (const ComplexRowVector& a,
				      const RowVector& b);
  friend ComplexRowVector operator - (const ComplexRowVector& a,
				      const RowVector& b);

  friend ComplexRowVector product (const ComplexRowVector& a,
				   const RowVector& b);
  friend ComplexRowVector quotient (const ComplexRowVector& a,
				    const RowVector& b);

// other operations

  friend ComplexRowVector map (c_c_Mapper f, const ComplexRowVector& a);
  friend RowVector map (d_c_Mapper f, const ComplexRowVector& a);
  void map (c_c_Mapper f);

  Complex min (void) const;
  Complex max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexRowVector& a);
  friend istream& operator >> (istream& is, ComplexRowVector& a);

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexRowVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  ComplexRowVector (Complex *d, int l) : Array<Complex> (d, l) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
