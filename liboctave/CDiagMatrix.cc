// DiagMatrix manipulations.                             -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>

#include <Complex.h>

#include "mx-base.h"
#include "mx-inlines.cc"
#include "lo-error.h"

/*
 * Complex Diagonal Matrix class
 */

#define KLUDGE_DIAG_MATRICES
#define TYPE Complex
#define KL_DMAT_TYPE ComplexDiagMatrix
#include "mx-kludge.cc"
#undef KLUDGE_DIAG_MATRICES
#undef TYPE
#undef KL_DMAT_TYPE

ComplexDiagMatrix::ComplexDiagMatrix (const RowVector& a)
  : DiagArray<Complex> (a.length ())
{
  for (int i = 0; i < length (); i++)
    elem (i, i) = a.elem (i);
}

ComplexDiagMatrix::ComplexDiagMatrix (const ColumnVector& a)
  : DiagArray<Complex> (a.length ())
{
  for (int i = 0; i < length (); i++)
    elem (i, i) = a.elem (i);
}

ComplexDiagMatrix::ComplexDiagMatrix (const DiagMatrix& a)
  : DiagArray<Complex> (a.rows (), a.cols ())
{
  for (int i = 0; i < length (); i++)
    elem (i, i) = a.elem (i, i);
}

#if 0
ComplexDiagMatrix&
ComplexDiagMatrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  Complex *new_data = 0;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::resize (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  Complex *new_data = 0;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];

      for (i = min_len; i < new_len; i++)
	new_data[i] = val;
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::resize (int r, int c, const Complex& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  Complex *new_data = 0;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];

      for (i = min_len; i < new_len; i++)
	new_data[i] = val;
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}
#endif

int
ComplexDiagMatrix::operator == (const ComplexDiagMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return equal (data (), a.data (), length ());
}

int
ComplexDiagMatrix::operator != (const ComplexDiagMatrix& a) const
{
  return !(*this == a);
}

ComplexDiagMatrix
ComplexDiagMatrix::hermitian (void) const
{
  return ComplexDiagMatrix (conj_dup (data (), length ()), cols (), rows ());
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (double val)
{
  for (int i = 0; i < length (); i++)
    elem (i, i) = val;
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const Complex& val)
{
  for (int i = 0; i < length (); i++)
    elem (i, i) = val;
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (double val, int beg, int end)
{
  if (beg < 0 || end >= length () || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = beg; i < end; i++)
    elem (i, i) = val;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const Complex& val, int beg, int end)
{
  if (beg < 0 || end >= length () || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = beg; i < end; i++)
    elem (i, i) = val;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ColumnVector& a)
{
  int len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexColumnVector& a)
{
  int len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const RowVector& a)
{
  int len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexRowVector& a)
{
  int len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ColumnVector& a, int beg)
{
  int a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexColumnVector& a, int beg)
{
  int a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const RowVector& a, int beg)
{
  int a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexRowVector& a, int beg)
{
  int a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

ComplexDiagMatrix
ComplexDiagMatrix::transpose (void) const
{
  return ComplexDiagMatrix (dup (data (), length ()), cols (), rows ());
}

DiagMatrix
real (const ComplexDiagMatrix& a)
{
  DiagMatrix retval;
  int a_len = a.length ();
  if (a_len > 0)
    retval = DiagMatrix (real_dup (a.data (), a_len), a.rows (),
			 a.cols ());
  return retval;
}

DiagMatrix
imag (const ComplexDiagMatrix& a)
{
  DiagMatrix retval;
  int a_len = a.length ();
  if (a_len > 0)
    retval = DiagMatrix (imag_dup (a.data (), a_len), a.rows (),
			 a.cols ());
  return retval;
}

ComplexDiagMatrix
conj (const ComplexDiagMatrix& a)
{
  ComplexDiagMatrix retval;
  int a_len = a.length ();
  if (a_len > 0)
    retval = ComplexDiagMatrix (conj_dup (a.data (), a_len),
				a.rows (), a.cols ());
  return retval;
}

// resize is the destructive analog for this one

ComplexMatrix
ComplexDiagMatrix::extract (int r1, int c1, int r2, int c2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_r = r2 - r1 + 1;
  int new_c = c2 - c1 + 1;

  ComplexMatrix result (new_r, new_c);

  for (int j = 0; j < new_c; j++)
    for (int i = 0; i < new_r; i++)
      result.elem (i, j) = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

ComplexRowVector
ComplexDiagMatrix::row (int i) const
{
  int nr = rows ();
  int nc = cols ();
  if (i < 0 || i >= nr)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector (); 
    }

  ComplexRowVector retval (nc, 0.0);
  if (nr <= nc || (nr > nc && i < nc))
    retval.elem (i) = elem (i, i);

  return retval;
}

ComplexRowVector
ComplexDiagMatrix::row (char *s) const
{
  if (! s)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector (); 
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (rows () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector ();
    }
}

ComplexColumnVector
ComplexDiagMatrix::column (int i) const
{
  int nr = rows ();
  int nc = cols ();
  if (i < 0 || i >= nc)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }

  ComplexColumnVector retval (nr, 0.0);
  if (nr >= nc || (nr < nc && i < nr))
    retval.elem (i) = elem (i, i);

  return retval;
}

ComplexColumnVector
ComplexDiagMatrix::column (char *s) const
{
  if (! s)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (cols () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }
}

ComplexDiagMatrix
ComplexDiagMatrix::inverse (void) const
{
  int info;
  return inverse (info);
}

ComplexDiagMatrix
ComplexDiagMatrix::inverse (int& info) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != nc)
    {
      (*current_liboctave_error_handler) ("inverse requires square matrix");
      return DiagMatrix ();
    }

  ComplexDiagMatrix retval (nr, nc);

  info = 0;
  for (int i = 0; i < length (); i++)
    {
      if (elem (i, i) == 0.0)
	{
	  info = -1;
	  return *this;
	}
      else
	retval.elem (i, i) = 1.0 / elem (i, i);
    }

  return *this;
}

// diagonal matrix by diagonal matrix -> diagonal matrix operations

ComplexDiagMatrix&
ComplexDiagMatrix::operator += (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), length ());
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator -= (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), length ());
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator += (const ComplexDiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), length ());
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator -= (const ComplexDiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), length ());
  return *this;
}

// diagonal matrix by scalar -> matrix operations

ComplexMatrix
operator + (const ComplexDiagMatrix& a, double s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return a + tmp;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& a, double s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), -s);
  return a + tmp;
}

ComplexMatrix
operator + (const ComplexDiagMatrix& a, const Complex& s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return a + tmp;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& a, const Complex& s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), -s);
  return a + tmp;
}

// diagonal matrix by scalar -> diagonal matrix operations

ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, double s)
{
  return ComplexDiagMatrix (multiply (a.data (), a.length (), s),
			    a.rows (), a.cols ());
}

ComplexDiagMatrix
operator / (const ComplexDiagMatrix& a, double s)
{
  return ComplexDiagMatrix (divide (a.data (), a.length (), s),
			    a.rows (), a.cols ());
}

// scalar by diagonal matrix -> matrix operations

ComplexMatrix
operator + (double s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp + a;
}

ComplexMatrix
operator - (double s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp - a;
}

ComplexMatrix
operator + (const Complex& s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp + a;
}

ComplexMatrix
operator - (const Complex& s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp - a;
}

// scalar by diagonal matrix -> diagonal matrix operations

ComplexDiagMatrix
operator * (double s, const ComplexDiagMatrix& a)
{
  return ComplexDiagMatrix (multiply (a.data (), a.length (), s),
			    a.rows (), a.cols ());
}

// diagonal matrix by column vector -> column vector operations

ComplexColumnVector
operator * (const ComplexDiagMatrix& m, const ColumnVector& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_len = a.length ();
  if (nc != a_len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix muliplication attempted");
      return ComplexColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  ComplexColumnVector result (nr);

  for (int i = 0; i < a_len; i++)
    result.elem (i) = a.elem (i) * m.elem (i, i);

  for (i = a_len; i < nr; i++)
    result.elem (i) = 0.0;

  return result;
}

ComplexColumnVector
operator * (const ComplexDiagMatrix& m, const ComplexColumnVector& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_len = a.length ();
  if (nc != a_len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix muliplication attempted");
      return ComplexColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  ComplexColumnVector result (nr);

  for (int i = 0; i < a_len; i++)
    result.elem (i) = a.elem (i) * m.elem (i, i);

  for (i = a_len; i < nr; i++)
    result.elem (i) = 0.0;

  return result;
}

// diagonal matrix by diagonal matrix -> diagonal matrix operations

ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const ComplexDiagMatrix& b)
{
  int nr_a = a.rows ();
  int nc_a = a.cols ();
  int nr_b = b.rows ();
  int nc_b = b.cols ();
  if (nc_a != nr_b)
    {
      (*current_liboctave_error_handler)
        ("nonconformant matrix multiplication attempted");
      return ComplexDiagMatrix ();
    }

  if (nr_a == 0 || nc_a == 0 || nc_b == 0)
    return ComplexDiagMatrix (nr_a, nc_a, 0.0);

  ComplexDiagMatrix c (nr_a, nc_b);

  int len = nr_a < nc_b ? nr_a : nc_b;

  for (int i = 0; i < len; i++)
    {
      Complex a_element = a.elem (i, i);
      Complex b_element = b.elem (i, i);

      if (a_element == 0.0 || b_element == 0.0)
        c.elem (i, i) = 0.0;
      else if (a_element == 1.0)
        c.elem (i, i) = b_element;
      else if (b_element == 1.0)
        c.elem (i, i) = a_element;
      else
        c.elem (i, i) = a_element * b_element;
    }

  return c;
}

ComplexDiagMatrix
operator + (const ComplexDiagMatrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (add (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexDiagMatrix
operator - (const ComplexDiagMatrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (subtract (m.data (), a.data (), m.length ()),
			    nr, nc);
}

ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const DiagMatrix& b)
{
  int nr_a = a.rows ();
  int nc_a = a.cols ();
  int nr_b = b.rows ();
  int nc_b = b.cols ();
  if (nc_a != nr_b)
    {
      (*current_liboctave_error_handler)
        ("nonconformant matrix multiplication attempted");
      return ComplexDiagMatrix ();
    }

  if (nr_a == 0 || nc_a == 0 || nc_b == 0)
    return ComplexDiagMatrix (nr_a, nc_a, 0.0);

  ComplexDiagMatrix c (nr_a, nc_b);

  int len = nr_a < nc_b ? nr_a : nc_b;

  for (int i = 0; i < len; i++)
    {
      Complex a_element = a.elem (i, i);
      double b_element = b.elem (i, i);

      if (a_element == 0.0 || b_element == 0.0)
        c.elem (i, i) = 0.0;
      else if (a_element == 1.0)
        c.elem (i, i) = b_element;
      else if (b_element == 1.0)
        c.elem (i, i) = a_element;
      else
        c.elem (i, i) = a_element * b_element;
    }

  return c;
}

ComplexDiagMatrix
product (const ComplexDiagMatrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (m.data (), a.data (), m.length ()),
			    nr, nc);
}

// diagonal matrix by matrix -> matrix operations

ComplexMatrix
operator + (const ComplexDiagMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (-a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const ComplexDiagMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (nc != a_nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return ComplexMatrix (nr, a_nc, 0.0);

  ComplexMatrix c (nr, a_nc);

  for (int i = 0; i < m.length (); i++)
    {
      if (m.elem (i, i) == 1.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (m.elem (i, i) == 0.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = m.elem (i, i) * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a_nc; j++)
	for (int i = a_nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

ComplexMatrix
operator + (const ComplexDiagMatrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (-a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const ComplexDiagMatrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (nc != a_nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return ComplexMatrix (nr, a_nc, 0.0);

  ComplexMatrix c (nr, a_nc);

  for (int i = 0; i < m.length (); i++)
    {
      if (m.elem (i, i) == 1.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (m.elem (i, i) == 0.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = m.elem (i, i) * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a_nc; j++)
	for (int i = a_nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

// other operations

ComplexColumnVector
ComplexDiagMatrix::diag (void) const
{
  return diag (0);
}

// Could be optimized...

ComplexColumnVector
ComplexDiagMatrix::diag (int k) const
{
  int nnr = rows ();
  int nnc = cols ();
  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  ComplexColumnVector d;

  if (nnr > 0 && nnc > 0)
    {
      int ndiag = (nnr < nnc) ? nnr : nnc;

      d.resize (ndiag);

      if (k > 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i+k);
	}
      else if ( k < 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i-k, i);
	}
      else
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i);
	}
    }
  else
    cerr << "diag: requested diagonal out of range\n";

  return d;
}

// i/o

ostream&
operator << (ostream& os, const ComplexDiagMatrix& a)
{
  Complex ZERO (0.0);
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.rows (); i++)
    {
      for (int j = 0; j < a.cols (); j++)
	{
	  if (i == j)
	    os << " " /* setw (field_width) */ << a.elem (i, i);
	  else
	    os << " " /* setw (field_width) */ << ZERO;
	}
      os << "\n";
    }
  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
