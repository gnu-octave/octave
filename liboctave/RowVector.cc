// RowVector manipulations.                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

// I\'m not sure how this is supposed to work if the .h file declares
// several classes, each of which is defined in a separate file...
//
// #ifdef __GNUG__
// #pragma implementation "Matrix.h"
// #endif

#include "Matrix.h"
#include "mx-inlines.cc"
#include "lo-error.h"

/*
 * Row Vector class.
 */

RowVector::RowVector (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create vector with negative dimension");
      len = 0;
      data = (double *) NULL;
      return;
    }

  len = n;
  if (len > 0)
    data = new double [len];
  else
    data = (double *) NULL;
}

RowVector::RowVector (int n, double val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create vector with negative dimension");
      len = 0;
      data = (double *) NULL;
      return;
    }

  len = n;
  if (len > 0)
    {
      data = new double [len];
      copy (data, len, val);
    }
  else
    data = (double *) NULL;
}

RowVector::RowVector (const RowVector& a)
{
  len = a.len;
  if (len > 0)
    {
      data = new double [len];
      copy (data, a.data, len);
    }
  else
    data = (double *) NULL;
}

RowVector::RowVector (double a)
{
  len = 1;
  data = new double [1];
  data[0] = a;
}

RowVector&
RowVector::operator = (const RowVector& a)
{
  if (this != &a)
    {
      delete [] data;
      len = a.len;
      if (len > 0)
	{
	  data = new double [len];
	  copy (data, a.data, len);
	}
      else
	data = (double *) NULL;
    }
  return *this;
}

double&
RowVector::checkelem (int n)
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    {
      (*current_liboctave_error_handler) ("range error");
      static double foo = 0.0;
      return foo;
    }
#endif

  return elem (n);
}

double
RowVector::checkelem (int n) const
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    {
      (*current_liboctave_error_handler) ("range error");
      return 0.0;
    }
#endif

  return elem (n);
}

RowVector&
RowVector::resize (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return *this;
    }

  double *new_data = (double *) NULL;
  if (n > 0)
    {
      new_data = new double [n];
      int min_len = len < n ? len : n;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];
    }

  delete [] data;
  len = n;
  data = new_data;

  return *this;
}

RowVector&
RowVector::resize (int n, double val)
{
  int old_len = len;
  resize (n);
  for (int i = old_len; i < len; i++)
    data[i] = val;

  return *this;
}

int
RowVector::operator == (const RowVector& a) const
{
  if (len != a.len)
    return 0;
  return equal (data, a.data, len);
}

int
RowVector::operator != (const RowVector& a) const
{
  if (len != a.len)
    return 1;
  return !equal (data, a.data, len);
}

RowVector&
RowVector::insert (const RowVector& a, int c)
{
  if (c < 0 || c + a.len - 1 > len)
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a.len; i++)
    data[c+i] = a.data[i];

  return *this;
}

RowVector&
RowVector::fill (double val)
{
  if (len > 0)
    copy (data, len, val);
  return *this;
}

RowVector&
RowVector::fill (double val, int c1, int c2)
{
  if (c1 < 0 || c2 < 0 || c1 >= len || c2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int i = c1; i <= c2; i++)
    data[i] = val;

  return *this;
}

RowVector
RowVector::append (const RowVector& a) const
{
  int nc_insert = len;
  RowVector retval (len + a.len);
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ColumnVector
RowVector::transpose (void) const
{
  return ColumnVector (dup (data, len), len);
}

RowVector
RowVector::extract (int c1, int c2) const
{
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_c = c2 - c1 + 1;

  RowVector result (new_c);

  for (int i = 0; i < new_c; i++)
    result.data[i] = elem (c1+i);

  return result;
}

// row vector by scalar -> row vector operations

RowVector
RowVector::operator + (double s) const
{
  return RowVector (add (data, len, s), len);
}

RowVector
RowVector::operator - (double s) const
{
  return RowVector (subtract (data, len, s), len);
}

RowVector
RowVector::operator * (double s) const
{
  return RowVector (multiply (data, len, s), len);
}

RowVector
RowVector::operator / (double s) const
{
  return RowVector (divide (data, len, s), len);
}

ComplexRowVector
RowVector::operator + (const Complex& s) const
{
  return ComplexRowVector (add (data, len, s), len);
}

ComplexRowVector
RowVector::operator - (const Complex& s) const
{
  return ComplexRowVector (subtract (data, len, s), len);
}

ComplexRowVector
RowVector::operator * (const Complex& s) const
{
  return ComplexRowVector (multiply (data, len, s), len);
}

ComplexRowVector
RowVector::operator / (const Complex& s) const
{
  return ComplexRowVector (divide (data, len, s), len);
}

// scalar by row vector -> row vector operations

RowVector
operator + (double s, const RowVector& a)
{
  return RowVector (add (a.data, a.len, s), a.len);
}

RowVector
operator - (double s, const RowVector& a)
{
  return RowVector (subtract (s, a.data, a.len), a.len);
}

RowVector
operator * (double s, const RowVector& a)
{
  return RowVector (multiply (a.data, a.len, s), a.len);
}

RowVector
operator / (double s, const RowVector& a)
{
  return RowVector (divide (s, a.data, a.len), a.len);
}

// row vector by column vector -> scalar

double
RowVector::operator * (const ColumnVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return 0.0;
    }

  int i_one = 1;
  return F77_FCN (ddot) (&len, data, &i_one, a.data, &i_one);
}

Complex
RowVector::operator * (const ComplexColumnVector& a) const
{
  ComplexRowVector tmp (*this);
  return tmp * a;
}

// row vector by matrix -> row vector

RowVector
RowVector::operator * (const Matrix& a) const
{
  if (a.nr != len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return RowVector ();
    }

  if (len == 0 || a.nc == 0)
    return RowVector (0);

// Transpose A to form A'*x == (x'*A)'

  int anr = a.nr;
  int anc = a.nc;

  char trans = 'T';
  int ld = anr;
  double alpha = 1.0;
  double beta  = 0.0;
  int i_one = 1;

  double *y = new double [len];

  F77_FCN (dgemv) (&trans, &anc, &anr, &alpha, a.data, &ld, data,
		   &i_one, &beta, y, &i_one, 1L); 

  return RowVector (y, len);
}

ComplexRowVector
RowVector::operator * (const ComplexMatrix& a) const
{
  ComplexRowVector tmp (*this);
  return tmp * a;
}

// row vector by row vector -> row vector operations

RowVector
RowVector::operator + (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector addition attempted");
      return RowVector ();
    }

  if (len == 0)
    return RowVector (0);

  return RowVector (add (data, a.data, len), len);
}

RowVector
RowVector::operator - (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector subtraction attempted");
      return RowVector ();
    }

  if (len == 0)
    return RowVector (0);

  return RowVector (subtract (data, a.data, len), len);
}

ComplexRowVector
RowVector::operator + (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector addition attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (add (data, a.data, len), len);
}

ComplexRowVector
RowVector::operator - (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector subtraction attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (subtract (data, a.data, len), len);
}

RowVector
RowVector::product (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector product attempted");
      return RowVector ();
    }

  if (len == 0)
    return RowVector (0);

  return RowVector (multiply (data, a.data, len), len);
}

RowVector
RowVector::quotient (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector quotient attempted");
      return RowVector ();
    }

  if (len == 0)
    return RowVector (0);

  return RowVector (divide (data, a.data, len), len);
}

ComplexRowVector
RowVector::product (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector product attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (multiply (data, a.data, len), len);
}

ComplexRowVector
RowVector::quotient (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector quotient attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (divide (data, a.data, len), len);
}

RowVector&
RowVector::operator += (const RowVector& a)
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector += operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

RowVector&
RowVector::operator -= (const RowVector& a)
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector -= operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

// unary operations

RowVector
RowVector::operator - (void) const
{
  if (len == 0)
    return RowVector (0);

  return RowVector (negate (data, len), len);
}

RowVector
map (d_d_Mapper f, const RowVector& a)
{
  RowVector b (a);
  b.map (f);
  return b;
}

void
RowVector::map (d_d_Mapper f)
{
  for (int i = 0; i < len; i++)
    data[i] = f (data[i]);
}

double
RowVector::min (void) const
{
  if (len == 0)
    return 0;

  double res = data[0];

  for (int i = 1; i < len; i++)
    if (data[i] < res)
      res = data[i];

  return res;
}

double
RowVector::max (void) const
{
  if (len == 0)
    return 0;

  double res = data[0];

  for (int i = 1; i < len; i++)
    if (data[i] > res)
      res = data[i];

  return res;
}

ostream&
operator << (ostream& os, const RowVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.len; i++)
    os << " " /* setw (field_width) */ << a.data[i];
  return os;
}

/*
 * Complex Row Vector class
 */

ComplexRowVector::ComplexRowVector (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create vector with negative dimension");
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  len = n;
  if (len > 0)
    data = new Complex [len];
  else
    data = (Complex *) NULL;
}

ComplexRowVector::ComplexRowVector (int n, double val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create vector with negative dimension");
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  len = n;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexRowVector::ComplexRowVector (int n, const Complex& val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create vector with negative dimension");
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  len = n;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexRowVector::ComplexRowVector (const RowVector& a)
{
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexRowVector::ComplexRowVector (const ComplexRowVector& a)
{
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexRowVector::ComplexRowVector (double a)
{
  len = 1;
  data = new Complex [1];
  data[0] = a;
}

ComplexRowVector::ComplexRowVector (const Complex& a)
{
  len = 1;
  data = new Complex [1];
  data[0] = Complex (a);
}

ComplexRowVector&
ComplexRowVector::operator = (const RowVector& a)
{
  delete [] data;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;

  return *this;
}

ComplexRowVector&
ComplexRowVector::operator = (const ComplexRowVector& a)
{
  if (this != &a)
    {
      delete [] data;
      len = a.len;
      if (len > 0)
	{
	  data = new Complex [len];
	  copy (data, a.data, len);
	}
      else
	data = (Complex *) NULL;
    }
  return *this;
}

Complex&
ComplexRowVector::checkelem (int n)
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    {
      (*current_liboctave_error_handler) ("range error");
      static Complex foo (0.0);
      return foo;
    }
#endif

  return elem (n);
}

Complex
ComplexRowVector::checkelem (int n) const
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    {
      (*current_liboctave_error_handler) ("range error");
      return Complex (0.0);
    }
#endif

  return elem (n);
}

ComplexRowVector&
ComplexRowVector::resize (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return *this;
    }

  Complex *new_data = (Complex *) NULL;
  if (n > 0)
    {
      new_data = new Complex [n];
      int min_len = len < n ? len : n;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];
    }

  delete [] data;
  len = n;
  data = new_data;

  return *this;
}

ComplexRowVector&
ComplexRowVector::resize (int n, double val)
{
  int old_len = len;
  resize (n);
  for (int i = old_len; i < len; i++)
    data[i] = val;

  return *this;
}

ComplexRowVector&
ComplexRowVector::resize (int n, const Complex& val)
{
  int old_len = len;
  resize (n);
  for (int i = old_len; i < len; i++)
    data[i] = val;

  return *this;
}

int
ComplexRowVector::operator == (const ComplexRowVector& a) const
{
  if (len != a.len)
    return 0;
  return equal (data, a.data, len);
}

int
ComplexRowVector::operator != (const ComplexRowVector& a) const
{
  if (len != a.len)
    return 1;
  return !equal (data, a.data, len);
}

// destructive insert/delete/reorder operations

ComplexRowVector&
ComplexRowVector::insert (const RowVector& a, int c)
{
  if (c < 0 || c + a.len - 1 > len)
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a.len; i++)
    data[c+i] = a.data[i];

  return *this;
}

ComplexRowVector&
ComplexRowVector::insert (const ComplexRowVector& a, int c)
{
  if (c < 0 || c + a.len - 1 > len)
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a.len; i++)
    data[c+i] = a.data[i];

  return *this;
}

ComplexRowVector&
ComplexRowVector::fill (double val)
{
  if (len > 0)
    copy (data, len, val);
  return *this;
}

ComplexRowVector&
ComplexRowVector::fill (const Complex& val)
{
  if (len > 0)
    copy (data, len, val);
  return *this;
}

ComplexRowVector&
ComplexRowVector::fill (double val, int c1, int c2)
{
  if (c1 < 0 || c2 < 0 || c1 >= len || c2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int i = c1; i <= c2; i++)
    data[i] = val;

  return *this;
}

ComplexRowVector&
ComplexRowVector::fill (const Complex& val, int c1, int c2)
{
  if (c1 < 0 || c2 < 0 || c1 >= len || c2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int i = c1; i <= c2; i++)
    data[i] = val;

  return *this;
}

ComplexRowVector
ComplexRowVector::append (const RowVector& a) const
{
  int nc_insert = len;
  ComplexRowVector retval (len + a.len);
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ComplexRowVector
ComplexRowVector::append (const ComplexRowVector& a) const
{
  int nc_insert = len;
  ComplexRowVector retval (len + a.len);
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ComplexColumnVector
ComplexRowVector::hermitian (void) const
{
  return ComplexColumnVector (conj_dup (data, len), len);
}

ComplexColumnVector
ComplexRowVector::transpose (void) const
{
  return ComplexColumnVector (dup (data, len), len);
}

RowVector
real (const ComplexRowVector& a)
{
  RowVector retval;
  if (a.len > 0)
    retval = RowVector (real_dup (a.data, a.len), a.len);
  return retval;
}

RowVector
imag (const ComplexRowVector& a)
{
  RowVector retval;
  if (a.len > 0)
    retval = RowVector (imag_dup (a.data, a.len), a.len);
  return retval;
}

ComplexRowVector
conj (const ComplexRowVector& a)
{
  ComplexRowVector retval;
  if (a.len > 0)
    retval = ComplexRowVector (conj_dup (a.data, a.len), a.len);
  return retval;
}

// resize is the destructive equivalent for this one

ComplexRowVector
ComplexRowVector::extract (int c1, int c2) const
{
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_c = c2 - c1 + 1;

  ComplexRowVector result (new_c);

  for (int i = 0; i < new_c; i++)
    result.data[i] = elem (c1+i);

  return result;
}

// row vector by scalar -> row vector operations

ComplexRowVector
ComplexRowVector::operator + (double s) const
{
  return ComplexRowVector (add (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator - (double s) const
{
  return ComplexRowVector (subtract (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator * (double s) const
{
  return ComplexRowVector (multiply (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator / (double s) const
{
  return ComplexRowVector (divide (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator + (const Complex& s) const
{
  return ComplexRowVector (add (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator - (const Complex& s) const
{
  return ComplexRowVector (subtract (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator * (const Complex& s) const
{
  return ComplexRowVector (multiply (data, len, s), len);
}

ComplexRowVector
ComplexRowVector::operator / (const Complex& s) const
{
  return ComplexRowVector (divide (data, len, s), len);
}

// scalar by row vector -> row vector operations

ComplexRowVector
operator + (double s, const ComplexRowVector& a)
{
  return ComplexRowVector (add (a.data, a.len, s), a.len);
}

ComplexRowVector
operator - (double s, const ComplexRowVector& a)
{
  return ComplexRowVector (subtract (s, a.data, a.len), a.len);
}

ComplexRowVector
operator * (double s, const ComplexRowVector& a)
{
  return ComplexRowVector (multiply (a.data, a.len, s), a.len);
}

ComplexRowVector
operator / (double s, const ComplexRowVector& a)
{
  return ComplexRowVector (divide (s, a.data, a.len), a.len);
}

ComplexRowVector
operator + (const Complex& s, const ComplexRowVector& a)
{
  return ComplexRowVector (add (a.data, a.len, s), a.len);
}

ComplexRowVector
operator - (const Complex& s, const ComplexRowVector& a)
{
  return ComplexRowVector (subtract (s, a.data, a.len), a.len);
}

ComplexRowVector
operator * (const Complex& s, const ComplexRowVector& a)
{
  return ComplexRowVector (multiply (a.data, a.len, s), a.len);
}

ComplexRowVector
operator / (const Complex& s, const ComplexRowVector& a)
{
  return ComplexRowVector (divide (s, a.data, a.len), a.len);
}

// row vector by column vector -> scalar

Complex
ComplexRowVector::operator * (const ColumnVector& a) const
{
  ComplexColumnVector tmp (a);
  return *this * tmp;
}

Complex
ComplexRowVector::operator * (const ComplexColumnVector& a) const
{
// XXX FIXME XXX -- need function body
  assert (0);
  return Complex (0.0, 0.0);
}

// row vector by matrix -> row vector

ComplexRowVector
ComplexRowVector::operator * (const Matrix& a) const
{
  ComplexMatrix tmp (a);
  return *this * tmp;
}

ComplexRowVector
ComplexRowVector::operator * (const ComplexMatrix& a) const
{
  if (a.nr != len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return ComplexRowVector ();
    }

  if (len == 0 || a.nc == 0)
    return ComplexRowVector (0);

// Transpose A to form A'*x == (x'*A)'

  int anr = a.nr;
  int anc = a.nc;

  char trans = 'T';
  int ld = anr;
  Complex alpha (1.0);
  Complex beta (0.0);
  int i_one = 1;

  Complex *y = new Complex [len];

  F77_FCN (zgemv) (&trans, &anc, &anr, &alpha, a.data, &ld, data,
		   &i_one, &beta, y, &i_one, 1L); 

  return ComplexRowVector (y, len);
}

// row vector by row vector -> row vector operations

ComplexRowVector
ComplexRowVector::operator + (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector addition attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (add (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::operator - (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector subtraction attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (subtract (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::operator + (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector addition attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (add (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::operator - (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector subtraction attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (subtract (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::product (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector product attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (multiply (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::quotient (const RowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector quotient attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (divide (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::product (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector product attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (multiply (data, a.data, len), len);
}

ComplexRowVector
ComplexRowVector::quotient (const ComplexRowVector& a) const
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector quotient attempted");
      return ComplexRowVector ();
    }

  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (divide (data, a.data, len), len);
}

ComplexRowVector&
ComplexRowVector::operator += (const RowVector& a)
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector += operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexRowVector&
ComplexRowVector::operator -= (const RowVector& a)
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector -= operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

ComplexRowVector&
ComplexRowVector::operator += (const ComplexRowVector& a)
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector += operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexRowVector&
ComplexRowVector::operator -= (const ComplexRowVector& a)
{
  if (len != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector -= operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

// unary operations

ComplexRowVector
ComplexRowVector::operator - (void) const
{
  if (len == 0)
    return ComplexRowVector (0);

  return ComplexRowVector (negate (data, len), len);
}

ComplexRowVector
map (c_c_Mapper f, const ComplexRowVector& a)
{
  ComplexRowVector b (a);
  b.map (f);
  return b;
}

RowVector
map (d_c_Mapper f, const ComplexRowVector& a)
{
  RowVector b (a.len);
  for (int i = 0; i < a.len; i++)
    b.elem (i) = f (a.elem (i));
  return b;
}

void
ComplexRowVector::map (c_c_Mapper f)
{
  for (int i = 0; i < len; i++)
    data[i] = f (data[i]);
}

Complex
ComplexRowVector::min (void) const
{
  if (len == 0)
    return Complex (0.0);

  Complex res = data[0];
  double absres = abs (res);

  for (int i = 1; i < len; i++)
    if (abs (data[i]) < absres)
      {
	res = data[i];
	absres = abs (res);
      }

  return res;
}

Complex
ComplexRowVector::max (void) const
{
  if (len == 0)
    return Complex (0.0);

  Complex res = data[0];
  double absres = abs (res);

  for (int i = 1; i < len; i++)
    if (abs (data[i]) > absres)
      {
	res = data[i];
	absres = abs (res);
      }

  return res;
}

// i/o

ostream&
operator << (ostream& os, const ComplexRowVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.len; i++)
    os << " " /* setw (field_width) */ << a.data[i];
  return os;
}


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
