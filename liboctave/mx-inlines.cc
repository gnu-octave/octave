// Helper functions for matrix classes.                 -*- C++ -*-
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

#ifdef __GNUG__
#pragma implementation
#endif

// But first, some helper functions...

static inline double *
add (double *d, int len, double s)
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
subtract (double *d, int len, double s)
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
subtract (double s, double *d, int len)
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
multiply (double *d, int len, double s)
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
divide (double *d, int len, double s)
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
divide (double s, double *d, int len)
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
add (double *x, double *y, int len)
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
subtract (double *x, double *y, int len)
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
multiply (double *x, double *y, int len)
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
divide (double *x, double *y, int len)
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
add2 (double *x, double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] += y[i];
  return x;
}

static inline double *
subtract2 (double *x, double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] -= y[i];
  return x;
}

static inline double *
negate (double *d, int len)
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
copy (double *x, double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] = y[i];
}

static inline double *
dup (double *x, int len)
{
  double *retval = (double *) NULL;
  if (len > 0)
    {
      retval = new double [len];
      for (int i = 0; i < len; i++)
	retval[i] = x[i];
    }
  return retval;
}

static inline int
equal (double *x, double *y, int len)
{
  for (int i = 0; i < len; i++)
    if (x[i] != y[i])
      return 0;
  return 1;
}

// And some for Complex too...

static inline Complex *
add (Complex *d, int len, Complex s)
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
add (Complex s, Complex *d, int len)
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
subtract (Complex *d, int len, Complex s)
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
subtract (Complex s, Complex *d, int len)
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
multiply (Complex *d, int len, Complex s)
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
multiply (Complex s, Complex *d, int len)
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
divide (Complex *d, int len, Complex s)
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
divide (Complex s, Complex *d, int len)
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
add (Complex *x, Complex *y, int len)
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
subtract (Complex *x, Complex *y, int len)
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
multiply (Complex *x, Complex *y, int len)
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
divide (Complex *x, Complex *y, int len)
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
add2 (Complex *x, Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] += y[i];
  return x;
}

static inline Complex *
subtract2 (Complex *x, Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] -= y[i];
  return x;
}

static inline Complex *
negate (Complex *d, int len)
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
not (Complex *d, int len)
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
copy (Complex *x, Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] = y[i];
}

static inline Complex *
dup (Complex *x, int len)
{
  Complex *retval = (Complex *) NULL;
  if (len > 0)
    {
      retval = new Complex [len];
      for (int i = 0; i < len; i++)
	retval[i] = x[i];
    }
  return retval;
}

static inline Complex *
make_complex (double *x, int len)
{
  Complex *retval = (Complex *) NULL;
  if (len > 0)
    {
      retval = new Complex [len];
      for (int i = 0; i < len; i++)
	retval[i] = x[i];
    }
  return retval;
}

static inline Complex *
conj_dup (Complex *x, int len)
{
  Complex *retval = (Complex *) NULL;
  if (len > 0)
    {
      retval = new Complex [len];
      for (int i = 0; i < len; i++)
	retval[i] = conj (x[i]);
    }
  return retval;
}

static inline double *
real_dup (Complex *x, int len)
{
  double *retval = (double *) NULL;
  if (len > 0)
    {
      retval = new double [len];
      for (int i = 0; i < len; i++)
	retval[i] = real (x[i]);
    }
  return retval;
}

static inline double *
imag_dup (Complex *x, int len)
{
  double *retval = (double *) NULL;
  if (len > 0)
    {
      retval = new double [len];
      for (int i = 0; i < len; i++)
	retval[i] = imag (x[i]);
    }
  return retval;
}

static inline int
equal (Complex *x, Complex *y, int len)
{
  for (int i = 0; i < len; i++)
    if (x[i] != y[i])
      return 0;
  return 1;
}

// And still some more for mixed Complex/double operations...

static inline Complex *
add (Complex *d, int len, double s)
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
add (double *d, int len, Complex s)
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
add (double s, Complex *d, int len)
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
add (Complex s, double *d, int len)
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
subtract (Complex *d, int len, double s)
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
subtract (double *d, int len, Complex s)
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
subtract (double s, Complex *d, int len)
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
subtract (Complex s, double *d, int len)
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
multiply (Complex *d, int len, double s)
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
multiply (double *d, int len, Complex s)
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
divide (Complex *d, int len, double s)
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
divide (double *d, int len, Complex s)
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
divide (double s, Complex *d, int len)
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
divide (Complex s, double *d, int len)
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
add (Complex *x, double *y, int len)
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
add (double *x, Complex *y, int len)
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
subtract (Complex *x, double *y, int len)
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
subtract (double *x, Complex *y, int len)
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
multiply (Complex *x, double *y, int len)
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
multiply (double *x, Complex *y, int len)
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
divide (Complex *x, double *y, int len)
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
divide (double *x, Complex *y, int len)
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
add2 (Complex *x, double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] += y[i];
  return x;
}

static inline Complex *
subtract2 (Complex *x, double *y, int len)
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
copy (Complex *x, double *y, int len)
{
  for (int i = 0; i < len; i++)
    x[i] = y[i];
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
