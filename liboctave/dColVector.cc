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
#include <config.h>
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
  int F77_FCN (dgemv) (const char*, const int*, const int*,
		       const double*, const double*, const int*,
		       const double*, const int*, const double*,
		       double*, const int*, long);
}

/*
 * Column Vector class.
 */

int
ColumnVector::operator == (const ColumnVector& a) const
{
  int len = length ();
  if (len != a.length ())
    return 0;
  return equal (data (), a.data (), len);
}

int
ColumnVector::operator != (const ColumnVector& a) const
{
  return !(*this == a);
}

ColumnVector&
ColumnVector::insert (const ColumnVector& a, int r)
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

ColumnVector&
ColumnVector::fill (double val)
{
  int len = length ();
  if (len > 0)
    for (int i = 0; i < len; i++)
      elem (i) = val;
  return *this;
}

ColumnVector&
ColumnVector::fill (double val, int r1, int r2)
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

ColumnVector
ColumnVector::stack (const ColumnVector& a) const
{
  int len = length ();
  int nr_insert = len;
  ColumnVector retval (len + a.length ());
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

RowVector
ColumnVector::transpose (void) const
{
  int len = length ();
  return RowVector (dup (data (), len), len);
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

// resize is the destructive equivalent for this one

ColumnVector
ColumnVector::extract (int r1, int r2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }

  int new_r = r2 - r1 + 1;

  ColumnVector result (new_r);

  for (int i = 0; i < new_r; i++)
    result.elem (i) = elem (r1+i);

  return result;
}

// column vector by column vector -> column vector operations

ColumnVector&
ColumnVector::operator += (const ColumnVector& a)
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

ColumnVector&
ColumnVector::operator -= (const ColumnVector& a)
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

// matrix by column vector -> column vector operations

ColumnVector
operator * (const Matrix& m, const ColumnVector& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ColumnVector ();
    }

  if (nr == 0 || nc == 0)
    return ColumnVector (0);

  char trans = 'N';
  int ld = nr;
  double alpha = 1.0;
  double beta  = 0.0;
  int i_one = 1;

  double *y = new double [nr];

  F77_FCN (dgemv) (&trans, &nr, &nc, &alpha, m.data (), &ld, a.data (),
		   &i_one, &beta, y, &i_one, 1L); 

  return ColumnVector (y, nr);
}

// diagonal matrix by column vector -> column vector operations

ColumnVector
operator * (const DiagMatrix& m, const ColumnVector& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_len = a.length ();
  if (nc != a_len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ColumnVector (0);

  ColumnVector result (nr);

  for (int i = 0; i < a_len; i++)
    result.elem (i) = a.elem (i) * m.elem (i, i);

  for (i = a_len; i < nr; i++)
    result.elem (i) = 0.0;

  return result;
}

// other operations

ColumnVector
map (d_d_Mapper f, const ColumnVector& a)
{
  ColumnVector b (a);
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
ColumnVector::map (d_d_Mapper f)
{
  for (int i = 0; i < length (); i++)
    elem (i) = f (elem (i));
}

double
ColumnVector::min (void) const
{
  int len = length ();
  if (len == 0)
    return 0.0;

  double res = elem (0);

  for (int i = 1; i < len; i++)
    if (elem (i) < res)
      res = elem (i);

  return res;
}

double
ColumnVector::max (void) const
{
  int len = length ();
  if (len == 0)
    return 0.0;

  double res = elem (0);

  for (int i = 1; i < len; i++)
    if (elem (i) > res)
      res = elem (i);

  return res;
}

ostream&
operator << (ostream& os, const ColumnVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.length (); i++)
    os << /* setw (field_width) << */ a.elem (i) << "\n";
  return os;
}

istream&
operator >> (istream& is, ColumnVector& a)
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
