// ColumnVector manipulations.                            -*- C++ -*-
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

/*
 * Column Vector class.
 */

ColumnVector::ColumnVector (int n)
{
  if (n < 0)
    FAIL;

  len = n;
  if (n > 0)
    data = new double [len];
  else
    data = (double *) NULL;
}

ColumnVector::ColumnVector (int n, double val)
{
  if (n < 0)
    FAIL;

  len = n;
  if (n > 0)
    {
      data = new double [len];
      copy (data, len, val);
    }
  else
    data = (double *) NULL;
}

ColumnVector::ColumnVector (const ColumnVector& a)
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

ColumnVector::ColumnVector (double a)
{
  len = 1;
  data = new double [1];
  data[0] = a;
}

ColumnVector&
ColumnVector::operator = (const ColumnVector& a)
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

ColumnVector&
ColumnVector::resize (int n)
{
  if (n < 0)
    FAIL;

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

ColumnVector&
ColumnVector::resize (int n, double val)
{
  int old_len = len;
  resize (n);
  for (int i = old_len; i < len; i++)
    data[i] = val;

  return *this;
}

int
ColumnVector::operator == (const ColumnVector& a) const
{
  if (len != a.len)
    return 0;
  return equal (data, a.data, len);
}

int
ColumnVector::operator != (const ColumnVector& a) const
{
  if (len != a.len)
    return 1;
  return !equal (data, a.data, len);
}

ColumnVector&
ColumnVector::insert (const ColumnVector& a, int r)
{
  if (r < 0 || r + a.len - 1 > len)
    FAIL;

  for (int i = 0; i < a.len; i++)
    data[r+i] = a.data[i];

  return *this;
}

ColumnVector&
ColumnVector::fill (double val)
{
  if (len > 0)
    copy (data, len, val);
  return *this;
}

ColumnVector&
ColumnVector::fill (double val, int r1, int r2)
{
  if (r1 < 0 || r2 < 0 || r1 >= len || r2 >= len)
    FAIL;

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  for (int i = r1; i <= r2; i++)
    data[i] = val;

  return *this;
}

ColumnVector
ColumnVector::stack (const ColumnVector& a) const
{
  int nr_insert = len;
  ColumnVector retval (len + a.len);
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

RowVector
ColumnVector::transpose (void) const
{
  return RowVector (dup (data, len), len);
}

// resize is the destructive equivalent for this one

ColumnVector
ColumnVector::extract (int r1, int r2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  int new_r = r2 - r1 + 1;

  ColumnVector result (new_r);

  for (int i = 0; i < new_r; i++)
    result.data[i] = elem (r1+i);

  return result;
}

// column vector by scalar -> column vector operations

ColumnVector
ColumnVector::operator + (double s) const
{
  return ColumnVector (add (data, len, s), len);
}

ColumnVector
ColumnVector::operator - (double s) const
{
  return ColumnVector (subtract (data, len, s), len);
}

ColumnVector
ColumnVector::operator * (double s) const
{
  return ColumnVector (multiply (data, len, s), len);
}

ColumnVector
ColumnVector::operator / (double s) const
{
  return ColumnVector (divide (data, len, s), len);
}

// scalar by column vector -> column vector operations

ColumnVector
operator + (double s, const ColumnVector& a)
{
  return ColumnVector (add (a.data, a.len, s), a.len);
}

ColumnVector
operator - (double s, const ColumnVector& a)
{
  return ColumnVector (subtract (s, a.data, a.len), a.len);
}

ColumnVector
operator * (double s, const ColumnVector& a)
{
  return ColumnVector (multiply (a.data, a.len, s), a.len);
}

ColumnVector
operator / (double s, const ColumnVector& a)
{
  return ColumnVector (divide (s, a.data, a.len), a.len);
}

ComplexColumnVector
ColumnVector::operator + (const Complex& s) const
{
  return ComplexColumnVector (add (data, len, s), len);
}

ComplexColumnVector
ColumnVector::operator - (const Complex& s) const
{
  return ComplexColumnVector (subtract (data, len, s), len);
}

ComplexColumnVector
ColumnVector::operator * (const Complex& s) const
{
  return ComplexColumnVector (multiply (data, len, s), len);
}

ComplexColumnVector
ColumnVector::operator / (const Complex& s) const
{
  return ComplexColumnVector (divide (data, len, s), len);
}

// column vector by row vector -> matrix operations

Matrix
ColumnVector::operator * (const RowVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return Matrix (len, len, 0.0);

  char transa = 'N';
  char transb = 'N';
  double alpha = 1.0;
  double beta  = 0.0;
  int anr = 1;
  int anc = a.len;

  double *c = new double [len * a.len];

  F77_FCN (dgemm) (&transa, &transb, &len, &anc, &anr, &alpha, data,
		   &len, a.data, &anr, &beta, c, &len, 1L, 1L);

  return Matrix (c, len, a.len);
}

ComplexMatrix
ColumnVector::operator * (const ComplexRowVector& a) const
{
  ComplexColumnVector tmp (*this);
  return tmp * a;
}

// column vector by column vector -> column vector operations

ColumnVector
ColumnVector::operator + (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ColumnVector (0);

  return ColumnVector (add (data, a.data, len), len);
}

ColumnVector
ColumnVector::operator - (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ColumnVector (0);

  return ColumnVector (subtract (data, a.data, len), len);
}

ComplexColumnVector
ColumnVector::operator + (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (add (data, a.data, len), len);
}

ComplexColumnVector
ColumnVector::operator - (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (subtract (data, a.data, len), len);
}

ColumnVector
ColumnVector::product (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ColumnVector (0);

  return ColumnVector (multiply (data, a.data, len), len);
}

ColumnVector
ColumnVector::quotient (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ColumnVector (0);

  return ColumnVector (divide (data, a.data, len), len);
}

ComplexColumnVector
ColumnVector::product (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (multiply (data, a.data, len), len);
}

ComplexColumnVector
ColumnVector::quotient (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (divide (data, a.data, len), len);
}

ColumnVector&
ColumnVector::operator += (const ColumnVector& a)
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ColumnVector&
ColumnVector::operator -= (const ColumnVector& a)
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

// unary operations

ColumnVector
ColumnVector::operator - (void) const
{
  if (len == 0)
    return ColumnVector (0);

  return ColumnVector (negate (data, len), len);
}

ColumnVector
map (d_d_Mapper f, const ColumnVector& a)
{
  ColumnVector b (a);
  b.map (f);
  return b;
}

void
ColumnVector::map (d_d_Mapper f)
{
  for (int i = 0; i < len; i++)
    data[i] = f (data[i]);
}

double
ColumnVector::min (void) const
{
  if (len == 0)
    return 0.0;

  double res = data[0];

  for (int i = 1; i < len; i++)
    if (data[i] < res)
      res = data[i];

  return res;
}

double
ColumnVector::max (void) const
{
  if (len == 0)
    return 0.0;

  double res = data[0];

  for (int i = 1; i < len; i++)
    if (data[i] > res)
      res = data[i];

  return res;
}

ostream&
operator << (ostream& os, const ColumnVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.len; i++)
    os << /* setw (field_width) << */ a.data[i] << "\n";
  return os;
}

/*
 * Complex Column Vector class
 */

ComplexColumnVector::ComplexColumnVector (int n)
{
  if (n < 0)
    FAIL;

  len = n;
  if (n > 0)
    data = new Complex [len];
  else
    data = (Complex *) NULL;
}

ComplexColumnVector::ComplexColumnVector (int n, double val)
{
  if (n < 0)
    FAIL;

  len = n;
  if (n > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexColumnVector::ComplexColumnVector (int n, const Complex& val)
{
  if (n < 0)
    FAIL;

  len = n;
  if (n > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexColumnVector::ComplexColumnVector (const ColumnVector& a)
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

ComplexColumnVector::ComplexColumnVector (const ComplexColumnVector& a)
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

ComplexColumnVector::ComplexColumnVector (double a)
{
  len = 1;
  data = new Complex [1];
  data[0] = a;
}

ComplexColumnVector::ComplexColumnVector (const Complex& a)
{
  len = 1;
  data = new Complex [1];
  data[0] = Complex (a);
}

ComplexColumnVector&
ComplexColumnVector::operator = (const ColumnVector& a)
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

ComplexColumnVector&
ComplexColumnVector::operator = (const ComplexColumnVector& a)
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

ComplexColumnVector&
ComplexColumnVector::resize (int n)
{
  if (n < 0)
    FAIL;

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

ComplexColumnVector&
ComplexColumnVector::resize (int n, double val)
{
  int old_len = len;
  resize (n);
  for (int i = old_len; i < len; i++)
    data[i] = val;

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::resize (int n, const Complex& val)
{
  int old_len = len;
  resize (n);
  for (int i = old_len; i < len; i++)
    data[i] = val;

  return *this;
}

int
ComplexColumnVector::operator == (const ComplexColumnVector& a) const
{
  if (len != a.len)
    return 0;
  return equal (data, a.data, len);
}

int
ComplexColumnVector::operator != (const ComplexColumnVector& a) const
{
  if (len != a.len)
    return 0;
  return !equal (data, a.data, len);
}

// destructive insert/delete/reorder operations

ComplexColumnVector&
ComplexColumnVector::insert (const ColumnVector& a, int r)
{
  if (r < 0 || r + a.len - 1 > len)
    FAIL;

  for (int i = 0; i < a.len; i++)
    data[r+i] = a.data[i];

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::insert (const ComplexColumnVector& a, int r)
{
  if (r < 0 || r + a.len - 1 > len)
    FAIL;

  for (int i = 0; i < a.len; i++)
    data[r+i] = a.data[i];

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (double val)
{
  if (len > 0)
    copy (data, len, val);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (const Complex& val)
{
  if (len > 0)
    copy (data, len, val);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (double val, int r1, int r2)
{
  if (r1 < 0 || r2 < 0 || r1 >= len || r2 >= len)
    FAIL;

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  for (int i = r1; i <= r2; i++)
    data[i] = val;

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (const Complex& val, int r1, int r2)
{
  if (r1 < 0 || r2 < 0 || r1 >= len || r2 >= len)
    FAIL;

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  for (int i = r1; i <= r2; i++)
    data[i] = val;

  return *this;
}

ComplexColumnVector
ComplexColumnVector::stack (const ColumnVector& a) const
{
  int nr_insert = len;
  ComplexColumnVector retval (len + a.len);
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

ComplexColumnVector
ComplexColumnVector::stack (const ComplexColumnVector& a) const
{
  int nr_insert = len;
  ComplexColumnVector retval (len + a.len);
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

ComplexRowVector
ComplexColumnVector::hermitian (void) const
{
  return ComplexRowVector (conj_dup (data, len), len);
}

ComplexRowVector
ComplexColumnVector::transpose (void) const
{
  return ComplexRowVector (dup (data, len), len);
}

ColumnVector
real (const ComplexColumnVector& a)
{
  ColumnVector retval;
  if (a.len > 0)
    retval = ColumnVector (real_dup (a.data, a.len), a.len);
  return retval;
}

ColumnVector
imag (const ComplexColumnVector& a)
{
  ColumnVector retval;
  if (a.len > 0)
    retval = ColumnVector (imag_dup (a.data, a.len), a.len);
  return retval;
}

ComplexColumnVector
conj (const ComplexColumnVector& a)
{
  ComplexColumnVector retval;
  if (a.len > 0)
    retval = ComplexColumnVector (conj_dup (a.data, a.len), a.len);
  return retval;
}

// resize is the destructive equivalent for this one

ComplexColumnVector
ComplexColumnVector::extract (int r1, int r2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  int new_r = r2 - r1 + 1;

  ComplexColumnVector result (new_r);

  for (int i = 0; i < new_r; i++)
    result.data[i] = elem (r1+i);

  return result;
}

// column vector by scalar -> column vector operations

ComplexColumnVector
ComplexColumnVector::operator + (double s) const
{
  return ComplexColumnVector (add (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator - (double s) const
{
  return ComplexColumnVector (subtract (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator * (double s) const
{
  return ComplexColumnVector (multiply (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator / (double s) const
{
  return ComplexColumnVector (divide (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator + (const Complex& s) const
{
  return ComplexColumnVector (add (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator - (const Complex& s) const
{
  return ComplexColumnVector (subtract (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator * (const Complex& s) const
{
  return ComplexColumnVector (multiply (data, len, s), len);
}

ComplexColumnVector
ComplexColumnVector::operator / (const Complex& s) const
{
  return ComplexColumnVector (divide (data, len, s), len);
}

// scalar by column vector -> column vector operations

ComplexColumnVector
operator + (double s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (add (a.data, a.len, s), a.len);
}

ComplexColumnVector
operator - (double s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (subtract (s, a.data, a.len), a.len);
}

ComplexColumnVector
operator * (double s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (multiply (a.data, a.len, s), a.len);
}

ComplexColumnVector
operator / (double s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (divide (s, a.data, a.len), a.len);
}

ComplexColumnVector
operator + (const Complex& s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (add (a.data, a.len, s), a.len);
}

ComplexColumnVector
operator - (const Complex& s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (subtract (s, a.data, a.len), a.len);
}

ComplexColumnVector
operator * (const Complex& s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (multiply (a.data, a.len, s), a.len);
}

ComplexColumnVector
operator / (const Complex& s, const ComplexColumnVector& a)
{
  return ComplexColumnVector (divide (s, a.data, a.len), a.len);
}

// column vector by row vector -> matrix operations

ComplexMatrix
ComplexColumnVector::operator * (const RowVector& a) const
{
  ComplexRowVector tmp (a);
  return *this * tmp;
}

ComplexMatrix
ComplexColumnVector::operator * (const ComplexRowVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexMatrix (len, len, 0.0);

  char transa = 'N';
  char transb = 'N';
  Complex alpha (1.0);
  Complex beta (0.0);
  int anr = 1;
  int anc = a.len;

  Complex *c = new Complex [len * a.len];

  F77_FCN (zgemm) (&transa, &transb, &len, &anc, &anr, &alpha, data,
		   &len, a.data, &anr, &beta, c, &len, 1L, 1L);

  return ComplexMatrix (c, len, a.len);
}

// column vector by column vector -> column vector operations

ComplexColumnVector
ComplexColumnVector::operator + (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (add (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::operator - (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (subtract (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::operator + (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (add (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::operator - (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (subtract (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::product (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (multiply (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::quotient (const ColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (divide (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::product (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (multiply (data, a.data, len), len);
}

ComplexColumnVector
ComplexColumnVector::quotient (const ComplexColumnVector& a) const
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (divide (data, a.data, len), len);
}

ComplexColumnVector&
ComplexColumnVector::operator += (const ColumnVector& a)
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::operator -= (const ColumnVector& a)
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::operator += (const ComplexColumnVector& a)
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::operator -= (const ComplexColumnVector& a)
{
  if (len != a.len)
    FAIL;

  if (len == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

// unary operations

ComplexColumnVector
ComplexColumnVector::operator - (void) const
{
  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (negate (data, len), len);
}

ComplexColumnVector
map (c_c_Mapper f, const ComplexColumnVector& a)
{
  ComplexColumnVector b (a);
  b.map (f);
  return b;
}

ColumnVector
map (d_c_Mapper f, const ComplexColumnVector& a)
{
  ColumnVector b (a.len);
  for (int i = 0; i < a.len; i++)
    b.elem (i) = f (a.elem (i));
  return b;
}

void
ComplexColumnVector::map (c_c_Mapper f)
{
  for (int i = 0; i < len; i++)
    data[i] = f (data[i]);
}

Complex
ComplexColumnVector::min (void) const
{
  if (len == 0)
    return 0.0;

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
ComplexColumnVector::max (void) const
{
  if (len == 0)
    return 0.0;

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
operator << (ostream& os, const ComplexColumnVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.len; i++)
    os << /* setw (field_width) << */ a.data[i] << "\n";
  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
