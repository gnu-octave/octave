// RowVector manipulations.
/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "Array-util.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"

// Fortran functions we call.

extern "C"
{
  F77_RET_T
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
			   const int&, const int&, const double&,
			   const double*, const int&, const double*,
			   const int&, const double&, double*, const int&
			   F77_CHAR_ARG_LEN_DECL);

  double F77_FUNC (ddot, DDOT) (const int&, const double*, const int&,
				const double*, const int&);
}

// Row Vector class.

bool
RowVector::operator == (const RowVector& a) const
{
  int len = length ();
  if (len != a.length ())
    return 0;
  return mx_inline_equal (data (), a.data (), len);
}

bool
RowVector::operator != (const RowVector& a) const
{
  return !(*this == a);
}

RowVector&
RowVector::insert (const RowVector& a, int c)
{
  int a_len = a.length ();

  if (c < 0 || c + a_len > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (int i = 0; i < a_len; i++)
	xelem (c+i) = a.elem (i);
    }

  return *this;
}

RowVector&
RowVector::fill (double val)
{
  int len = length ();

  if (len > 0)
    {
      make_unique ();

      for (int i = 0; i < len; i++)
	xelem (i) = val;
    }

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

  if (c2 >= c1)
    {
      make_unique ();

      for (int i = c1; i <= c2; i++)
	xelem (i) = val;
    }

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
  return ColumnVector (*this);
}

RowVector
real (const ComplexRowVector& a)
{
  int a_len = a.length ();
  RowVector retval;
  if (a_len > 0)
    retval = RowVector (mx_inline_real_dup (a.data (), a_len), a_len);
  return retval;
}

RowVector
imag (const ComplexRowVector& a)
{
  int a_len = a.length ();
  RowVector retval;
  if (a_len > 0)
    retval = RowVector (mx_inline_imag_dup (a.data (), a_len), a_len);
  return retval;
}

RowVector
RowVector::extract (int c1, int c2) const
{
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_c = c2 - c1 + 1;

  RowVector result (new_c);

  for (int i = 0; i < new_c; i++)
    result.xelem (i) = elem (c1+i);

  return result;
}

RowVector
RowVector::extract_n (int r1, int n) const
{
  RowVector result (n);

  for (int i = 0; i < n; i++)
    result.xelem (i) = elem (r1+i);

  return result;
}

// row vector by matrix -> row vector

RowVector
operator * (const RowVector& v, const Matrix& a)
{
  RowVector retval;

  int len = v.length ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (a_nr != len)
    gripe_nonconformant ("operator *", 1, len, a_nr, a_nc);
  else
    {
      if (len == 0)
	retval.resize (a_nc, 0.0);
      else
	{
	  // Transpose A to form A'*x == (x'*A)'

	  int ld = a_nr;

	  retval.resize (a_nc);
	  double *y = retval.fortran_vec ();

	  F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("T", 1),
				   a_nr, a_nc, 1.0, a.data (),
				   ld, v.data (), 1, 0.0, y, 1
				   F77_CHAR_ARG_LEN (1)));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dgemv");
	}
    }

  return retval;
}

// other operations

RowVector
RowVector::map (d_d_Mapper f) const
{
  RowVector b (*this);
  return b.apply (f);
}

RowVector&
RowVector::apply (d_d_Mapper f)
{
  double *d = fortran_vec (); // Ensures only one reference to my privates!

  for (int i = 0; i < length (); i++)
    d[i] = f (d[i]);

  return *this;
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

std::ostream&
operator << (std::ostream& os, const RowVector& a)
{
//  int field_width = os.precision () + 7;

  for (int i = 0; i < a.length (); i++)
    os << " " /* setw (field_width) */ << a.elem (i);
  return os;
}

std::istream&
operator >> (std::istream& is, RowVector& a)
{
  int len = a.length();

  if (len < 1)
    is.clear (std::ios::badbit);
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

  if (n > 1)
    {
      retval.resize (n);
      double delta = (x2 - x1) / (n - 1);
      retval.elem (0) = x1;
      for (int i = 1; i < n-1; i++)
	retval.elem (i) = x1 + i * delta;
      retval.elem (n-1) = x2;
    }
  else if (n == 1)
    {
      if (x1 == x2)
	{
	  retval.resize (1);
	  retval.elem (0) = x1;
	}
      else
	(*current_liboctave_error_handler)
	  ("linspace: npoints is 1, but x1 != x2");
    }
  else
    (*current_liboctave_error_handler)
      ("linspace: npoints must be greater than 0");

  return retval;
}

// row vector by column vector -> scalar

double
operator * (const RowVector& v, const ColumnVector& a)
{
  double retval = 0.0;

  int len = v.length ();

  int a_len = a.length ();

  if (len != a_len)
    gripe_nonconformant ("operator *", len, a_len);
  else if (len != 0)
    retval = F77_FUNC (ddot, DDOT) (len, v.data (), 1, a.data (), 1);

  return retval;
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
;;; End: ***
*/
