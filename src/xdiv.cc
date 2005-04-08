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

#include <cassert>

#include "Array-util.h"
#include "CMatrix.h"
#include "dMatrix.h"
#include "CNDArray.h"
#include "dNDArray.h"
#include "oct-cmplx.h"
#include "quit.h"

#include "error.h"
#include "xdiv.h"

static inline bool
result_ok (octave_idx_type info)
{
  assert (info != -1);

  return (info != -2);
}

static void
solve_singularity_warning (double rcond)
{
  warning ("matrix singular to machine precision, rcond = %g", rcond);
  warning ("attempting to find minimum norm solution");
}

template <class T1, class T2>
bool
mx_leftdiv_conform (const T1& a, const T2& b)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type b_nr = b.rows ();

  if (a_nr != b_nr)
    {
      octave_idx_type a_nc = a.cols ();
      octave_idx_type b_nc = b.cols ();

      gripe_nonconformant ("operator \\", a_nr, a_nc, b_nr, b_nc);
      return false;
    }

  return true;
}

#define INSTANTIATE_MX_LEFTDIV_CONFORM(T1, T2) \
  template bool mx_leftdiv_conform (const T1&, const T2&)

INSTANTIATE_MX_LEFTDIV_CONFORM (Matrix, Matrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (Matrix, ComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (ComplexMatrix, Matrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (ComplexMatrix, ComplexMatrix);

template <class T1, class T2>
bool
mx_div_conform (const T1& a, const T2& b)
{
  octave_idx_type a_nc = a.cols ();
  octave_idx_type b_nc = b.cols ();

  if (a_nc != b_nc)
    {
      octave_idx_type a_nr = a.rows ();
      octave_idx_type b_nr = b.rows ();

      gripe_nonconformant ("operator /", a_nr, a_nc, b_nr, b_nc);
      return false;
    }

  return true;
}

#define INSTANTIATE_MX_DIV_CONFORM(T1, T2) \
  template bool mx_div_conform (const T1&, const T2&)

INSTANTIATE_MX_DIV_CONFORM (Matrix, Matrix);
INSTANTIATE_MX_DIV_CONFORM (Matrix, ComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (ComplexMatrix, Matrix);
INSTANTIATE_MX_DIV_CONFORM (ComplexMatrix, ComplexMatrix);

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

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      Matrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return Matrix (result.transpose ());
    }

  octave_idx_type rank;
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

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      ComplexMatrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result.hermitian ();
    }

  octave_idx_type rank;
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

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      ComplexMatrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result.hermitian ();
    }

  octave_idx_type rank;
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

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      ComplexMatrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result.hermitian ();
    }

  octave_idx_type rank;
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
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.columns ();

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = a / b (i, j);
      }

  return result;
}

ComplexMatrix
x_el_div (double a, const ComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = a / b (i, j);
      }

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const Matrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = a / b (i, j);
      }

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const ComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = a / b (i, j);
      }

  return result;
}

// Funny element by element division operations.
//
//          op2 \ op1:   s   cs
//               +--   +---+----+
//   N-d array         | 1 |  3 |
//                     +---+----+
//   complex N-d array | 2 |  4 |
//                     +---+----+

NDArray
x_el_div (double a, const NDArray& b)
{
  NDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      OCTAVE_QUIT;
      result (i) = a / b (i);
    }

  return result;
}

ComplexNDArray
x_el_div (double a, const ComplexNDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      OCTAVE_QUIT;
      result (i) = a / b (i);
    }

  return result;
}

ComplexNDArray
x_el_div (const Complex a, const NDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      OCTAVE_QUIT;
      result (i) = a / b (i);
    }

  return result;
}

ComplexNDArray
x_el_div (const Complex a, const ComplexNDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      OCTAVE_QUIT;
      result (i) = a / b (i);
    }

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

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      Matrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

// -*- 2 -*-
ComplexMatrix
xleftdiv (const Matrix& a, const ComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      ComplexMatrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

// -*- 3 -*-
ComplexMatrix
xleftdiv (const ComplexMatrix& a, const Matrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      ComplexMatrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

// -*- 4 -*-
ComplexMatrix
xleftdiv (const ComplexMatrix& a, const ComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      ComplexMatrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
