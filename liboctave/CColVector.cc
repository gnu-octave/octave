// ColumnVector manipulations.                            -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>

#include <Complex.h>

#include "mx-base.h"
#include "mx-inlines.cc"
#include "f77-uscore.h"
#include "lo-error.h"

// Fortran functions we call.

extern "C"
{
  int F77_FCN (zgemm) (const char*, const char*, const int*,
		       const int*, const int*, const Complex*,
		       const Complex*, const int*, const Complex*,
		       const int*, const Complex*, Complex*, const int*,
		       long, long);
}

/*
 * Complex Column Vector class
 */

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexColumnVector
#include "mx-kludge.cc"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

ComplexColumnVector::ComplexColumnVector (const ColumnVector& a)
   : Array<Complex> (a.length ())
{
  for (int i = 0; i < length (); i++)
    elem (i) = a.elem (i);
}

#if 0
ComplexColumnVector&
ComplexColumnVector::resize (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return *this;
    }

  Complex *new_data = 0;
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
#endif

int
ComplexColumnVector::operator == (const ComplexColumnVector& a) const
{
  int len = length ();
  if (len != a.length ())
    return 0;
  return equal (data (), a.data (), len);
}

int
ComplexColumnVector::operator != (const ComplexColumnVector& a) const
{
  return !(*this == a);
}

// destructive insert/delete/reorder operations

ComplexColumnVector&
ComplexColumnVector::insert (const ColumnVector& a, int r)
{
  int a_len = a.length ();
  if (r < 0 || r + a_len - 1 > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r+i) = a.elem (i);

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::insert (const ComplexColumnVector& a, int r)
{
  int a_len = a.length ();
  if (r < 0 || r + a_len - 1 > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r+i) = a.elem (i);

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (double val)
{
  int len = length ();
  if (len > 0)
    for (int i = 0; i < len; i++)
      elem (i) = val;
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (const Complex& val)
{
  int len = length ();
  if (len > 0)
    for (int i = 0; i < len; i++)
      elem (i) = val;
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (double val, int r1, int r2)
{
  int len = length ();
  if (r1 < 0 || r2 < 0 || r1 >= len || r2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  for (int i = r1; i <= r2; i++)
    elem (i) = val;

  return *this;
}

ComplexColumnVector&
ComplexColumnVector::fill (const Complex& val, int r1, int r2)
{
  int len = length ();
  if (r1 < 0 || r2 < 0 || r1 >= len || r2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  for (int i = r1; i <= r2; i++)
    elem (i) = val;

  return *this;
}

ComplexColumnVector
ComplexColumnVector::stack (const ColumnVector& a) const
{
  int len = length ();
  int nr_insert = len;
  ComplexColumnVector retval (len + a.length ());
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

ComplexColumnVector
ComplexColumnVector::stack (const ComplexColumnVector& a) const
{
  int len = length ();
  int nr_insert = len;
  ComplexColumnVector retval (len + a.length ());
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

ComplexRowVector
ComplexColumnVector::hermitian (void) const
{
  int len = length ();
  return ComplexRowVector (conj_dup (data (), len), len);
}

ComplexRowVector
ComplexColumnVector::transpose (void) const
{
  int len = length ();
  return ComplexRowVector (dup (data (), len), len);
}

ColumnVector
real (const ComplexColumnVector& a)
{
  int a_len = a.length ();
  ColumnVector retval;
  if (a_len > 0)
    retval = ColumnVector (real_dup (a.data (), a_len), a_len);
  return retval;
}

ColumnVector
imag (const ComplexColumnVector& a)
{
  int a_len = a.length ();
  ColumnVector retval;
  if (a_len > 0)
    retval = ColumnVector (imag_dup (a.data (), a_len), a_len);
  return retval;
}

ComplexColumnVector
conj (const ComplexColumnVector& a)
{
  int a_len = a.length ();
  ComplexColumnVector retval;
  if (a_len > 0)
    retval = ComplexColumnVector (conj_dup (a.data (), a_len), a_len);
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
    result.elem (i) = elem (r1+i);

  return result;
}

// column vector by column vector -> column vector operations

ComplexColumnVector&
ComplexColumnVector::operator += (const ColumnVector& a)
{
  int len = length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector += operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), len);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::operator -= (const ColumnVector& a)
{
  int len = length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector -= operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), len);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::operator += (const ComplexColumnVector& a)
{
  int len = length ();

  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector += operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), len);
  return *this;
}

ComplexColumnVector&
ComplexColumnVector::operator -= (const ComplexColumnVector& a)
{
  int len = length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector -= operation attempted");
      return *this;
    }

  if (len == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), len);
  return *this;
}

// column vector by scalar -> column vector operations

ComplexColumnVector
operator + (const ComplexColumnVector& v, double s)
{
  int len = v.length ();
  return ComplexColumnVector (add (v.data (), len, s), len);
}

ComplexColumnVector
operator - (const ComplexColumnVector& v, double s)
{
  int len = v.length ();
  return ComplexColumnVector (subtract (v.data (), len, s), len);
}

ComplexColumnVector
operator * (const ComplexColumnVector& v, double s)
{
  int len = v.length ();
  return ComplexColumnVector (multiply (v.data (), len, s), len);
}

ComplexColumnVector
operator / (const ComplexColumnVector& v, double s)
{
  int len = v.length ();
  return ComplexColumnVector (divide (v.data (), len, s), len);
}

// scalar by column vector -> column vector operations

ComplexColumnVector
operator + (double s, const ComplexColumnVector& a)
{
  int a_len = a.length ();
  return ComplexColumnVector (add (a.data (), a_len, s), a_len);
}

ComplexColumnVector
operator - (double s, const ComplexColumnVector& a)
{
  int a_len = a.length ();
  return ComplexColumnVector (subtract (s, a.data (), a_len), a_len);
}

ComplexColumnVector
operator * (double s, const ComplexColumnVector& a)
{
  int a_len = a.length ();
  return ComplexColumnVector (multiply (a.data (), a_len, s), a_len);
}

ComplexColumnVector
operator / (double s, const ComplexColumnVector& a)
{
  int a_len = a.length ();
  return ComplexColumnVector (divide (s, a.data (), a_len), a_len);
}

// column vector by row vector -> matrix operations

ComplexMatrix
operator * (const ComplexColumnVector& v, const ComplexRowVector& a)
{
  int len = v.length ();
  int a_len = a.length ();
  if (len != a_len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return ComplexMatrix ();
    }

  if (len == 0)
    return ComplexMatrix (len, len, 0.0);

  char transa = 'N';
  char transb = 'N';
  Complex alpha (1.0);
  Complex beta (0.0);
  int anr = 1;

  Complex *c = new Complex [len * a_len];

  F77_FCN (zgemm) (&transa, &transb, &len, &a_len, &anr, &alpha,
		   v.data (), &len, a.data (), &anr, &beta, c, &len,
		   1L, 1L);

  return ComplexMatrix (c, len, a_len);
}

// column vector by column vector -> column vector operations

ComplexColumnVector
operator + (const ComplexColumnVector& v, const ColumnVector& a)
{
  int len = v.length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector addition attempted");
      return ComplexColumnVector ();
    }

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (add (v.data (), a.data (), len), len);
}

ComplexColumnVector
operator - (const ComplexColumnVector& v, const ColumnVector& a)
{
  int len = v.length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector subtraction attempted");
      return ComplexColumnVector ();
    }

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (subtract (v.data (), a.data (), len), len);
}

ComplexColumnVector
product (const ComplexColumnVector& v, const ColumnVector& a)
{
  int len = v.length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector product attempted");
      return ComplexColumnVector ();
    }

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (multiply (v.data (), a.data (), len), len);
}

ComplexColumnVector
quotient (const ComplexColumnVector& v, const ColumnVector& a)
{
  int len = v.length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector quotient attempted");
      return ComplexColumnVector ();
    }

  if (len == 0)
    return ComplexColumnVector (0);

  return ComplexColumnVector (divide (v.data (), a.data (), len), len);
}

// other operations

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
  int a_len = a.length ();
  ColumnVector b (a_len);
  for (int i = 0; i < a_len; i++)
    b.elem (i) = f (a.elem (i));
  return b;
}

void
ComplexColumnVector::map (c_c_Mapper f)
{
  for (int i = 0; i < length (); i++)
    elem (i) = f (elem (i));
}

Complex
ComplexColumnVector::min (void) const
{
  int len = length ();
  if (len == 0)
    return 0.0;

  Complex res = elem (0);
  double absres = abs (res);

  for (int i = 1; i < len; i++)
    if (abs (elem (i)) < absres)
      {
	res = elem (i);
	absres = abs (res);
      }

  return res;
}

Complex
ComplexColumnVector::max (void) const
{
  int len = length ();
  if (len == 0)
    return 0.0;

  Complex res = elem (0);
  double absres = abs (res);

  for (int i = 1; i < len; i++)
    if (abs (elem (i)) > absres)
      {
	res = elem (i);
	absres = abs (res);
      }

  return res;
}

// i/o

ostream&
operator << (ostream& os, const ComplexColumnVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.length (); i++)
    os << /* setw (field_width) << */ a.elem (i) << "\n";
  return os;
}

istream&
operator >> (istream& is, ComplexColumnVector& a)
{
  int len = a.length();

  if (len < 1)
    is.clear (ios::badbit);
  else
    {
      double tmp;
      for (int i = 0; i < len; i++)
        {
          is >> tmp;
          if (is)
            a.elem (i) = tmp;
          else
            break;
        }
    }
  return is;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
