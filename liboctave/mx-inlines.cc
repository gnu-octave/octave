// Helper functions for matrix classes.
/*

Copyright (C) 1996 John W. Eaton

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

#include "oct-cmplx.h"

// But first, some helper functions...

// XXX FIXME XXX -- these need to be done with templates...

static inline int
equal (const char *x, const char *y, int len)
{
  for (int i = 0; i < len; i++)
    if (x[i] != y[i])
      return 0;
  return 1;
}

static inline double *
add (const double *d, int len, double s)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] + s;
    }
  return result;
}

static inline double *
subtract (const double *d, int len, double s)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] - s;
    }
  return result;
}

static inline double *
subtract (double s, const double *d, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = s - d[i];
    }
  return result;
}

static inline double *
multiply (const double *d, int len, double s)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] * s;
    }
  return result;
}

static inline double *
divide (const double *d, int len, double s)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] / s;
    }
  return result;
}

static inline double *
divide (double s, const double *d, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = s / d[i];
    }
  return result;
}

static inline double *
add (const double *x, const double *y, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] + y[i];
    }
  return result;
}

static inline double *
subtract (const double *x, const double *y, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] - y[i];
    }
  return result;
}

static inline double *
multiply (const double *x, const double *y, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] * y[i];
    }
  return result;
}

static inline double *
divide (const double *x, const double *y, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] / y[i];
    }
  return result;
}

static inline double *
add2 (double *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] += y[i];
  return x;
}

static inline double *
subtract2 (double *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] -= y[i];
  return x;
}

static inline double *
negate (const double *d, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = -d[i];
    }
  return result;
}

static inline void
copy (double *d, int len, double s)
{
  for (int i = 0; i < len; i++)
    d[i] = s;
}

static inline void
copy (double *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] = y[i];
}

static inline double *
dup (const double *x, int len)
{
  double *retval = 0;
  if (len > 0)
    {
      retval = new double [len];
      for (int i = 0; i < len; i++)
	retval[i] = x[i];
    }
  return retval;
}

static inline int
equal (const double *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    if (x[i] != y[i])
      return 0;
  return 1;
}

// And some for Complex too...

static inline Complex *
add (const Complex *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] + s;
    }
  return result;
}

static inline Complex *
add (Complex s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s + d[i];
    }
  return result;
}

static inline Complex *
subtract (const Complex *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] - s;
    }
  return result;
}

static inline Complex *
subtract (Complex s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s - d[i];
    }
  return result;
}

static inline Complex *
multiply (const Complex *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] * s;
    }
  return result;
}

static inline Complex *
multiply (Complex s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s * d[i];
    }
  return result;
}

static inline Complex *
divide (const Complex *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] / s;
    }
  return result;
}

static inline Complex *
divide (Complex s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s / d[i];
    }
  return result;
}

static inline Complex *
add (const Complex *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] + y[i];
    }
  return result;
}

static inline Complex *
subtract (const Complex *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] - y[i];
    }
  return result;
}

static inline Complex *
multiply (const Complex *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] * y[i];
    }
  return result;
}

static inline Complex *
divide (const Complex *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] / y[i];
    }
  return result;
}

static inline Complex *
add2 (Complex *x, const Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] += y[i];
  return x;
}

static inline Complex *
subtract2 (Complex *x, const Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] -= y[i];
  return x;
}

static inline Complex *
negate (const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = -d[i];
    }
  return result;
}

static inline double *
not (const Complex *d, int len)
{
  double *result = 0;
  if (len > 0)
    {
      result = new double [len];
      for (int i = 0; i < len; i++)
	result[i] = (d[i] == 0.0);
    }
  return result;
}

static inline void
copy (Complex *d, int len, Complex s)
{
  for (int i = 0; i < len; i++)
    d[i] = s;
}

static inline void
copy (Complex *x, const Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] = y[i];
}

static inline Complex *
dup (const Complex *x, int len)
{
  Complex *retval = 0;
  if (len > 0)
    {
      retval = new Complex [len];
      for (int i = 0; i < len; i++)
	retval[i] = x[i];
    }
  return retval;
}

static inline Complex *
make_complex (const double *x, int len)
{
  Complex *retval = 0;
  if (len > 0)
    {
      retval = new Complex [len];
      for (int i = 0; i < len; i++)
	retval[i] = x[i];
    }
  return retval;
}

static inline Complex *
conj_dup (const Complex *x, int len)
{
  Complex *retval = 0;
  if (len > 0)
    {
      retval = new Complex [len];
      for (int i = 0; i < len; i++)
	retval[i] = conj (x[i]);
    }
  return retval;
}

static inline double *
real_dup (const Complex *x, int len)
{
  double *retval = 0;
  if (len > 0)
    {
      retval = new double [len];
      for (int i = 0; i < len; i++)
	retval[i] = real (x[i]);
    }
  return retval;
}

static inline double *
imag_dup (const Complex *x, int len)
{
  double *retval = 0;
  if (len > 0)
    {
      retval = new double [len];
      for (int i = 0; i < len; i++)
	retval[i] = imag (x[i]);
    }
  return retval;
}

static inline int
equal (const Complex *x, const Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    if (x[i] != y[i])
      return 0;
  return 1;
}

// And still some more for mixed Complex/double operations...

static inline Complex *
add (const Complex *d, int len, double s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] + s;
    }
  return result;
}

static inline Complex *
add (const double *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] + s;
    }
  return result;
}

static inline Complex *
add (double s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s + d[i];
    }
  return result;
}

static inline Complex *
add (Complex s, const double *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s + d[i];
    }
  return result;
}

static inline Complex *
subtract (const Complex *d, int len, double s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] - s;
    }
  return result;
}

static inline Complex *
subtract (const double *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] - s;
    }
  return result;
}

static inline Complex *
subtract (double s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s - d[i];
    }
  return result;
}

static inline Complex *
subtract (Complex s, const double *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s - d[i];
    }
  return result;
}

static inline Complex *
multiply (const Complex *d, int len, double s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] * s;
    }
  return result;
}

static inline Complex *
multiply (const double *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] * s;
    }
  return result;
}

static inline Complex *
divide (const Complex *d, int len, double s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] / s;
    }
  return result;
}

static inline Complex *
divide (const double *d, int len, Complex s)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = d[i] / s;
    }
  return result;
}

static inline Complex *
divide (double s, const Complex *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s / d[i];
    }
  return result;
}

static inline Complex *
divide (Complex s, const double *d, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = s / d[i];
    }
  return result;
}

static inline Complex *
add (const Complex *x, const double *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] + y[i];
    }
  return result;
}

static inline Complex *
add (const double *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] + y[i];
    }
  return result;
}

static inline Complex *
subtract (const Complex *x, const double *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] - y[i];
    }
  return result;
}

static inline Complex *
subtract (const double *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] - y[i];
    }
  return result;
}

static inline Complex *
multiply (const Complex *x, const double *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] * y[i];
    }
  return result;
}

static inline Complex *
multiply (const double *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] * y[i];
    }
  return result;
}

static inline Complex *
divide (const Complex *x, const double *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] / y[i];
    }
  return result;
}

static inline Complex *
divide (const double *x, const Complex *y, int len)
{
  Complex *result = 0;
  if (len > 0)
    {
      result = new Complex [len];
      for (int i = 0; i < len; i++)
	result[i] = x[i] / y[i];
    }
  return result;
}

static inline Complex *
add2 (Complex *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] += y[i];
  return x;
}

static inline Complex *
subtract2 (Complex *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] -= y[i];
  return x;
}

static inline void
copy (Complex *d, int len, double s)
{
  for (int i = 0; i < len; i++)
    d[i] = s;
}

static inline void
copy (Complex *x, const double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] = y[i];
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
