// ColumnVector manipulations.
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
			   const int&, const double&, double*,
			   const int&
			   F77_CHAR_ARG_LEN_DECL);
}

// Column Vector class.

bool
ColumnVector::operator == (const ColumnVector& a) const
{
  int len = length ();
  if (len != a.length ())
    return 0;
  return mx_inline_equal (data (), a.data (), len);
}

bool
ColumnVector::operator != (const ColumnVector& a) const
{
  return !(*this == a);
}

ColumnVector&
ColumnVector::insert (const ColumnVector& a, int r)
{
  int a_len = a.length ();

  if (r < 0 || r + a_len > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (int i = 0; i < a_len; i++)
	xelem (r+i) = a.elem (i);
    }

  return *this;
}

ColumnVector&
ColumnVector::fill (double val)
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

  if (r2 >= r1)
    {
      make_unique ();

      for (int i = r1; i <= r2; i++)
	xelem (i) = val;
    }

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
  return RowVector (*this);
}

ColumnVector
real (const ComplexColumnVector& a)
{
  int a_len = a.length ();
  ColumnVector retval;
  if (a_len > 0)
    retval = ColumnVector (mx_inline_real_dup (a.data (), a_len), a_len);
  return retval;
}

ColumnVector
imag (const ComplexColumnVector& a)
{
  int a_len = a.length ();
  ColumnVector retval;
  if (a_len > 0)
    retval = ColumnVector (mx_inline_imag_dup (a.data (), a_len), a_len);
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
    result.xelem (i) = elem (r1+i);

  return result;
}

ColumnVector
ColumnVector::extract_n (int r1, int n) const
{
  ColumnVector result (n);

  for (int i = 0; i < n; i++)
    result.xelem (i) = elem (r1+i);

  return result;
}

// matrix by column vector -> column vector operations

ColumnVector
operator * (const Matrix& m, const ColumnVector& a)
{
  ColumnVector retval;

  int nr = m.rows ();
  int nc = m.cols ();

  int a_len = a.length ();

  if (nc != a_len)
    gripe_nonconformant ("operator *", nr, nc, a_len, 1);
  else
    {
      if (nr == 0 || nc == 0)
	retval.resize (nr, 0.0);
      else
	{
	  int ld = nr;

	  retval.resize (nr);
	  double *y = retval.fortran_vec ();

	  F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
				   nr, nc, 1.0, m.data (), ld,
				   a.data (), 1, 0.0, y, 1
				   F77_CHAR_ARG_LEN (1)));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dgemv");
	}
    }

  return retval;
}

// diagonal matrix by column vector -> column vector operations

ColumnVector
operator * (const DiagMatrix& m, const ColumnVector& a)
{
  ColumnVector retval;

  int nr = m.rows ();
  int nc = m.cols ();

  int a_len = a.length ();

  if (nc != a_len)
    gripe_nonconformant ("operator *", nr, nc, a_len, 1);
  else
    {
      if (nr == 0 || nc == 0)
	retval.resize (nr, 0.0);
      else
	{
	  retval.resize (nr);

	  for (int i = 0; i < a_len; i++)
	    retval.elem (i) = a.elem (i) * m.elem (i, i);

	  for (int i = a_len; i < nr; i++)
	    retval.elem (i) = 0.0;
	}
    }

  return retval;
}

// other operations

ColumnVector
ColumnVector::map (d_d_Mapper f) const
{
  ColumnVector b (*this);
  return b.apply (f);
}

ColumnVector&
ColumnVector::apply (d_d_Mapper f)
{
  double *d = fortran_vec (); // Ensures only one reference to my privates!

  for (int i = 0; i < length (); i++)
    d[i] = f (d[i]);

  return *this;
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

std::ostream&
operator << (std::ostream& os, const ColumnVector& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.length (); i++)
    os << /* setw (field_width) << */ a.elem (i) << "\n";
  return os;
}

std::istream&
operator >> (std::istream& is, ColumnVector& a)
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
