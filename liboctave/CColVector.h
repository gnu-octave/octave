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

#if !defined (octave_ComplexColumnVector_h)
#define octave_ComplexColumnVector_h 1

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class ComplexColumnVector : public Array<Complex>
{
friend class ColumnVector;
friend class ComplexRowVector;
friend class ComplexMatrix;

public:

  ComplexColumnVector (void) : Array<Complex> () { }
  ComplexColumnVector (int n) : Array<Complex> (n) { }
  ComplexColumnVector (int n, const Complex& val)
    : Array<Complex> (n, val) { }
  ComplexColumnVector (const ColumnVector& a);
  ComplexColumnVector (const Array<Complex>& a) : Array<Complex> (a) { }
  ComplexColumnVector (const ComplexColumnVector& a) : Array<Complex> (a) { }
//  ComplexColumnVector (double a) : Array<Complex> (1, a) { }
//  ComplexColumnVector (const Complex& a) : Array<Complex> (1, a) { }

  ComplexColumnVector& operator = (const ComplexColumnVector& a)
    {
      Array<Complex>::operator = (a);
      return *this;
    }

//  operator Array<Complex>& () const { return *this; }

  int operator == (const ComplexColumnVector& a) const;
  int operator != (const ComplexColumnVector& a) const;

// destructive insert/delete/reorder operations

  ComplexColumnVector& insert (const ColumnVector& a, int r);
  ComplexColumnVector& insert (const ComplexColumnVector& a, int r);

  ComplexColumnVector& fill (double val);
  ComplexColumnVector& fill (const Complex& val);
  ComplexColumnVector& fill (double val, int r1, int r2);
  ComplexColumnVector& fill (const Complex& val, int r1, int r2);

  ComplexColumnVector stack (const ColumnVector& a) const;
  ComplexColumnVector stack (const ComplexColumnVector& a) const;

  ComplexRowVector hermitian (void) const;  // complex conjugate transpose.
  ComplexRowVector transpose (void) const;

  friend ColumnVector real (const ComplexColumnVector& a);
  friend ColumnVector imag (const ComplexColumnVector& a);
  friend ComplexColumnVector conj (const ComplexColumnVector& a);

// resize is the destructive equivalent for this one

  ComplexColumnVector extract (int r1, int r2) const;

// column vector by column vector -> column vector operations

  ComplexColumnVector& operator += (const ColumnVector& a);
  ComplexColumnVector& operator -= (const ColumnVector& a);

  ComplexColumnVector& operator += (const ComplexColumnVector& a);
  ComplexColumnVector& operator -= (const ComplexColumnVector& a);

// column vector by scalar -> column vector operations

  friend ComplexColumnVector operator + (const ComplexColumnVector& a,
					 double s);
  friend ComplexColumnVector operator - (const ComplexColumnVector& a,
					 double s);
  friend ComplexColumnVector operator * (const ComplexColumnVector& a,
					 double s);
  friend ComplexColumnVector operator / (const ComplexColumnVector& a,
					 double s);

// scalar by column vector -> column vector operations

  friend ComplexColumnVector operator + (double s,
					 const ComplexColumnVector& a); 
  friend ComplexColumnVector operator - (double s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator * (double s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator / (double s,
					 const ComplexColumnVector& a);

// column vector by row vector -> matrix operations

  friend ComplexMatrix operator * (const ComplexColumnVector& a,
				   const ComplexRowVector& b);

// column vector by column vector -> column vector operations

  friend ComplexColumnVector operator + (const ComplexColumnVector& a,
					 const ColumnVector& b);
  friend ComplexColumnVector operator - (const ComplexColumnVector& a,
					 const ColumnVector& b);

  friend ComplexColumnVector product (const ComplexColumnVector& a,
				      const ColumnVector& b);
  friend ComplexColumnVector quotient (const ComplexColumnVector& a,
				       const ColumnVector& b);

// other operations

  friend ComplexColumnVector map (c_c_Mapper f, const ComplexColumnVector& a);
  friend ColumnVector map (d_c_Mapper f, const ComplexColumnVector& a);
  void map (c_c_Mapper f);

  Complex min (void) const;
  Complex max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexColumnVector& a);
  friend istream& operator >> (istream& is, ComplexColumnVector& a);

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexColumnVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

// private:
// XXX FIXME XXX -- why does it not work to make this private, with
// ColumnVector declared as a friend of ComplexColumnVector?  It seems
// to work for the similar case with Matrix/ComplexMatrix.  Hmm...
  ComplexColumnVector (Complex *d, int l) : Array<Complex> (d, l) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
