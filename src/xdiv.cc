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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include "CMatrix.h"
#include "dMatrix.h"
#include "oct-cmplx.h"

#include "error.h"
#include "xdiv.h"

static inline int
result_ok (int info, double rcond, int warn = 1)
{
  assert (info != -1);

  if (info == -2)
    {
      if (warn)
	warning ("matrix singular to machine precision, rcond = %g", rcond);
      else
	error ("matrix singular to machine precision, rcond = %g", rcond);

      return 0;
    }
  else
    return 1;
}

template <class T1, class T2>
bool
mx_leftdiv_conform (T1 a, T2 b)
{
  int a_nr = a.rows ();
  int b_nr = b.rows ();

  if (a_nr != b_nr)
    {
      int a_nc = a.cols ();
      int b_nc = b.cols ();

      gripe_nonconformant ("operator \\", a_nr, a_nc, b_nr, b_nc);
      return false;
    }

  return true;
}

template bool mx_leftdiv_conform (const Matrix&, const Matrix&);
template bool mx_leftdiv_conform (const Matrix&, const ComplexMatrix&);
template bool mx_leftdiv_conform (const ComplexMatrix&, const ComplexMatrix&);
template bool mx_leftdiv_conform (const ComplexMatrix&, const Matrix&);

template <class T1, class T2>
bool
mx_div_conform (T1 a, T2 b)
{
  int a_nc = a.cols ();
  int b_nc = b.cols ();

  if (a_nc != b_nc)
    {
      int a_nr = a.rows ();
      int b_nr = b.rows ();

      gripe_nonconformant ("operator /", a_nr, a_nc, b_nr, b_nc);
      return false;
    }

  return true;
}

template bool mx_div_conform (const Matrix&, const Matrix&);
template bool mx_div_conform (const Matrix&, const ComplexMatrix&);
template bool mx_div_conform (const ComplexMatrix&, const ComplexMatrix&);
template bool mx_div_conform (const ComplexMatrix&, const Matrix&);

// Right division functions.
//
//       op2 / op1:   m   cm
//            +--   +---+----+
//   matrix         | 1 |  3 |
//                  +---+----+
//   complex_matrix | 2 |  4 |
//                  +---+----+

// -*- 1 -*-
Matrix
xdiv (const Matrix& a, const Matrix& b)
{
  if (! mx_div_conform (a, b))
    return Matrix ();

  Matrix atmp = a.transpose ();
  Matrix btmp = b.transpose ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      Matrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return Matrix (result.transpose ());
    }

  int rank;
  Matrix result = btmp.lssolve (atmp, info, rank);

  return result.transpose ();
}

// -*- 2 -*-
ComplexMatrix
xdiv (const Matrix& a, const ComplexMatrix& b)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  Matrix atmp = a.transpose ();
  ComplexMatrix btmp = b.hermitian ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return result.hermitian ();
    }

  int rank;
  ComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return result.hermitian ();
}

// -*- 3 -*-
ComplexMatrix
xdiv (const ComplexMatrix& a, const Matrix& b)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  Matrix btmp = b.transpose ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return result.hermitian ();
    }

  int rank;
  ComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return result.hermitian ();
}

// -*- 4 -*-
ComplexMatrix
xdiv (const ComplexMatrix& a, const ComplexMatrix& b)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  ComplexMatrix btmp = b.hermitian ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return result.hermitian ();
    }

  int rank;
  ComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return result.hermitian ();
}

// Funny element by element division operations.
//
//       op2 \ op1:   s   cs
//            +--   +---+----+
//   matrix         | 1 |  3 |
//                  +---+----+
//   complex_matrix | 2 |  4 |
//                  +---+----+

Matrix
x_el_div (double a, const Matrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = a / b (i, j);

  return result;
}

ComplexMatrix
x_el_div (double a, const ComplexMatrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = a / b (i, j);

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const Matrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = a / b (i, j);

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const ComplexMatrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = a / b (i, j);

  return result;
}

// Left division functions.
//
//       op2 \ op1:   m   cm
//            +--   +---+----+
//   matrix         | 1 |  3 |
//                  +---+----+
//   complex_matrix | 2 |  4 |
//                  +---+----+

// -*- 1 -*-
Matrix
xleftdiv (const Matrix& a, const Matrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return Matrix ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      Matrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return result;
    }

  int rank;
  return a.lssolve (b, info, rank);
}

// -*- 2 -*-
ComplexMatrix
xleftdiv (const Matrix& a, const ComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return result;
    }

  int rank;
  return a.lssolve (b, info, rank);
}

// -*- 3 -*-
ComplexMatrix
xleftdiv (const ComplexMatrix& a, const Matrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return result;
    }

  int rank;
  return a.lssolve (b, info, rank);
}

// -*- 4 -*-
ComplexMatrix
xleftdiv (const ComplexMatrix& a, const ComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return result;
    }

  int rank;
  return a.lssolve (b, info, rank);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
