// RowVector manipulations.                              -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include <Complex.h>

#include "f77-uscore.h"
#include "lo-error.h"
#include "mx-base.h"
#include "mx-inlines.cc"

// Fortran functions we call.

extern "C"
{
  int F77_FCN (dgemv, DGEMV) (const char*, const int&, const int&,
			      const double&, const double*,
			      const int&, const double*, const int&,
			      const double&, double*, const int&,
			      long);

  double F77_FCN (ddot, DDOT) (const int&, const double*, const int&,
			       const double*, const int&);
}

// Row Vector class.

int
RowVector::operator == (const RowVector& a) const
{
  int len = length ();
  if (len != a.length ())
    return 0;
  return equal (data (), a.data (), len);
}

int
RowVector::operator != (const RowVector& a) const
{
  return !(*this == a);
}

RowVector&
RowVector::insert (const RowVector& a, int c)
{
  int a_len = a.length ();
  if (c < 0 || c + a_len - 1 > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (c+i) = a.elem (i);

  return *this;
}

RowVector&
RowVector::fill (double val)
{
  int len = length ();
  if (len > 0)
    for (int i = 0; i < len; i++)
      elem (i) = val;
  return *this;
}

RowVector&
RowVector::fill (double val, int c1, int c2)
{
  int len = length ();
  if (c1 < 0 || c2 < 0 || c1 >= len || c2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int i = c1; i <= c2; i++)
    elem (i) = val;

  return *this;
}

RowVector
RowVector::append (const RowVector& a) const
{
  int len = length ();
  int nc_insert = len;
  RowVector retval (len + a.length ());
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ColumnVector
RowVector::transpose (void) const
{
  int len = length ();
  return ColumnVector (dup (data (), len), len);
}

RowVector
real (const ComplexRowVector& a)
{
  int a_len = a.length ();
  RowVector retval;
  if (a_len > 0)
    retval = RowVector (real_dup (a.data (), a_len), a_len);
  return retval;
}

RowVector
imag (const ComplexRowVector& a)
{
  int a_len = a.length ();
  RowVector retval;
  if (a_len > 0)
    retval = RowVector (imag_dup (a.data (), a_len), a_len);
  return retval;
}

RowVector
RowVector::extract (int c1, int c2) const
{
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_c = c2 - c1 + 1;

  RowVector result (new_c);

  for (int i = 0; i < new_c; i++)
    result.elem (i) = elem (c1+i);

  return result;
}

// row vector by row vector -> row vector operations

RowVector&
RowVector::operator += (const RowVector& a)
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

  double *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), len);
  return *this;
}

RowVector&
RowVector::operator -= (const RowVector& a)
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

  double *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), len);
  return *this;
}

// row vector by matrix -> row vector

RowVector
operator * (const RowVector& v, const Matrix& a)
{
  int len = v.length ();
  if (a.rows () != len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return RowVector ();
    }

  if (len == 0 || a.cols () == 0)
    return RowVector (0);

  // Transpose A to form A'*x == (x'*A)'

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  int ld = a_nr;

  double *y = new double [len];

  F77_FCN (dgemv, DGEMV) ("T", a_nc, a_nr, 1.0, a.data (), ld,
			  v.data (), 1, 0.0, y, 1, 1L);

  return RowVector (y, len);
}

// other operations

RowVector
map (d_d_Mapper f, const RowVector& a)
{
  RowVector b (a);
  b.map (f);
  return b;
}

RowVector
map (d_c_Mapper f, const ComplexRowVector& a)
{
  int a_len = a.length ();
  RowVector b (a_len);
  for (int i = 0; i < a_len; i++)
    b.elem (i) = f (a.elem (i));
  return b;
}

void
RowVector::map (d_d_Mapper f)
{
  for (int i = 0; i < length (); i++)
    elem (i) = f (elem (i));
}

double
RowVector::min (void) const
{
  int len = length ();
  if (len == 0)
    return 0;

  double res = elem (0);

  for (int i = 1; i < len; i++)
    if (elem (i) < res)
      res = elem (i);

  return res;
}

double
RowVector::max (void) const
{
  int len = length ();
  if (len == 0)
    return 0;

  double res = elem (0);

  for (int i = 1; i < len; i++)
    if (elem (i) > res)
      res = elem (i);

  return res;
}

ostream&
operator << (ostream& os, const RowVector& a)
{
//  int field_width = os.precision () + 7;

  for (int i = 0; i < a.length (); i++)
    os << " " /* setw (field_width) */ << a.elem (i);
  return os;
}

istream&
operator >> (istream& is, RowVector& a)
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

// other operations

RowVector
linspace (double x1, double x2, int n)
{
  RowVector retval;

  if (n > 0)
    {
      retval.resize (n);
      double delta = (x2 - x1) / (n - 1);
      retval.elem (0) = x1;
      for (int i = 1; i < n-1; i++)
	retval.elem (i) = x1 + i * delta;
      retval.elem (n-1) = x2;
    }

  return retval;
}

// row vector by column vector -> scalar

double
operator * (const RowVector& v, const ColumnVector& a)
{
  int len = v.length ();
  if (len != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return 0.0;
    }

  return F77_FCN (ddot, DDOT) (len, v.data (), 1, a.data (), 1);
}

Complex
operator * (const RowVector& v, const ComplexColumnVector& a)
{
  ComplexRowVector tmp (v);
  return tmp * a;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
