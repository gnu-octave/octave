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

#if !defined (octave_ColumnVector_h)
#define octave_ColumnVector_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class ColumnVector : public Array<double>
{
friend class RowVector;

public:

  ColumnVector (void) : Array<double> () { }
  ColumnVector (int n) : Array<double> (n) { }
  ColumnVector (int n, double val) : Array<double> (n, val) { }
  ColumnVector (const Array<double>& a) : Array<double> (a) { }
  ColumnVector (const ColumnVector& a) : Array<double> (a) { }
//  ColumnVector (double a) : Array<double> (1, a) { }

  ColumnVector& operator = (const ColumnVector& a)
    {
      Array<double>::operator = (a);
      return *this;
    }

//  operator Array<double>& () const { return *this; }

  int operator == (const ColumnVector& a) const;
  int operator != (const ColumnVector& a) const;

// destructive insert/delete/reorder operations

  ColumnVector& insert (const ColumnVector& a, int r);

  ColumnVector& fill (double val);
  ColumnVector& fill (double val, int r1, int r2);

  ColumnVector stack (const ColumnVector& a) const;

  RowVector transpose (void) const;

// resize is the destructive equivalent for this one

  ColumnVector extract (int r1, int r2) const;

// column vector by column vector -> column vector operations

  ColumnVector& operator += (const ColumnVector& a);
  ColumnVector& operator -= (const ColumnVector& a);

// column vector by scalar -> column vector operations

  friend ComplexColumnVector operator + (const ColumnVector& a,
					 const Complex& s);  
  friend ComplexColumnVector operator - (const ColumnVector& a,
					 const Complex& s);
  friend ComplexColumnVector operator * (const ColumnVector& a,
					 const Complex& s);
  friend ComplexColumnVector operator / (const ColumnVector& a,
					 const Complex& s);

// scalar by column vector -> column vector operations

  friend ComplexColumnVector operator + (const Complex& s,
					 const ColumnVector& a); 
  friend ComplexColumnVector operator - (const Complex& s,
					 const ColumnVector& a);
  friend ComplexColumnVector operator * (const Complex& s,
					 const ColumnVector& a);
  friend ComplexColumnVector operator / (const Complex& s,
					 const ColumnVector& a);

// column vector by row vector -> matrix operations

  friend Matrix operator * (const ColumnVector& a, const RowVector& a);

  friend ComplexMatrix operator * (const ColumnVector& a,
				   const ComplexRowVector& b);

// column vector by column vector -> column vector operations

  friend ComplexColumnVector operator + (const ComplexColumnVector& a,
					 const ComplexColumnVector& b);

  friend ComplexColumnVector operator - (const ComplexColumnVector& a,
					 const ComplexColumnVector& b); 

  friend ComplexColumnVector product (const ComplexColumnVector& a,
				      const ComplexColumnVector& b); 

  friend ComplexColumnVector quotient (const ComplexColumnVector& a,
				       const ComplexColumnVector& b); 

// other operations

  friend ColumnVector map (d_d_Mapper f, const ColumnVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ColumnVector& a);
  friend ostream& operator >> (ostream& is, ColumnVector& a);

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
