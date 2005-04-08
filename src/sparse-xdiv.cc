/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include "Array-util.h"
#include "oct-cmplx.h"
#include "quit.h"
#include "error.h"

#include "dSparse.h"
#include "CSparse.h"
#include "oct-spparms.h"
#include "sparse-xdiv.h"

static inline bool
result_ok (octave_idx_type info)
{
#ifdef HAVE_LSSOLVE
  return (info != -2 && info != -1);
#else
  // If the matrix is singular, who cares as we don't have QR based solver yet
  return (info != -1);
#endif
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

INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, SparseMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, SparseComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, SparseMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, SparseComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, Matrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, ComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, Matrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, ComplexMatrix);

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

INSTANTIATE_MX_DIV_CONFORM (SparseMatrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseMatrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseComplexMatrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseComplexMatrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (Matrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (Matrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (ComplexMatrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (ComplexMatrix, SparseComplexMatrix);

// Right division functions.
//
//              op2 / op1:   m   cm   sm  scm
//                   +--   +---+----+----+----+
//   sparse matrix         | 1 |  3 |  5 |  7 |
//                         +---+----+----+----+
//   sparse complex_matrix | 2 |  4 |  6 |  8 |
//                         +---+----+----+----+

// -*- 1 -*-
Matrix
xdiv (const Matrix& a, const SparseMatrix& b)
{
  if (! mx_div_conform (a, b))
    return Matrix ();

  Matrix atmp = a.transpose ();
  SparseMatrix btmp = b.transpose ();

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      Matrix result = btmp.solve (atmp, info, rcond, 
				  solve_singularity_warning);

      if (result_ok (info))
	return Matrix (result.transpose ());
    }

  octave_idx_type rank;
  Matrix result = btmp.lssolve (atmp, info, rank);

  return result.transpose ();
}

// -*- 2 -*-
ComplexMatrix
xdiv (const Matrix& a, const SparseComplexMatrix& b)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  Matrix atmp = a.transpose ();
  SparseComplexMatrix btmp = b.hermitian ();

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
xdiv (const ComplexMatrix& a, const SparseMatrix& b)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  SparseMatrix btmp = b.transpose ();

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
xdiv (const ComplexMatrix& a, const SparseComplexMatrix& b)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  SparseComplexMatrix btmp = b.hermitian ();

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

// -*- 5 -*-
SparseMatrix
xdiv (const SparseMatrix& a, const SparseMatrix& b)
{
  if (! mx_div_conform (a, b))
    return SparseMatrix ();

  SparseMatrix atmp = a.transpose ();
  SparseMatrix btmp = b.transpose ();

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      SparseMatrix result = btmp.solve (atmp, info, rcond, 
					solve_singularity_warning);

      if (result_ok (info))
	return SparseMatrix (result.transpose ());
    }

  octave_idx_type rank;
  SparseMatrix result = btmp.lssolve (atmp, info, rank);

  return result.transpose ();
}

// -*- 6 -*-
SparseComplexMatrix
xdiv (const SparseMatrix& a, const SparseComplexMatrix& b)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseMatrix atmp = a.transpose ();
  SparseComplexMatrix btmp = b.hermitian ();

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      SparseComplexMatrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result.hermitian ();
    }

  octave_idx_type rank;
  SparseComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return result.hermitian ();
}

// -*- 7 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseMatrix& b)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseComplexMatrix atmp = a.hermitian ();
  SparseMatrix btmp = b.transpose ();

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      SparseComplexMatrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result.hermitian ();
    }

  octave_idx_type rank;
  SparseComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return result.hermitian ();
}

// -*- 8 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseComplexMatrix atmp = a.hermitian ();
  SparseComplexMatrix btmp = b.hermitian ();

  octave_idx_type info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;

      SparseComplexMatrix result
	= btmp.solve (atmp, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result.hermitian ();
    }

  octave_idx_type rank;
  SparseComplexMatrix result = btmp.lssolve (atmp, info, rank);

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
x_el_div (double a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  Matrix result;
  if (a == 0.)
    result = Matrix (nr, nc, octave_NaN);
  else if (a > 0.)
    result = Matrix (nr, nc, octave_Inf);
  else
    result = Matrix (nr, nc, -octave_Inf);


  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
      {
	OCTAVE_QUIT;
	result.elem (b.ridx(i), j) = a / b.data (i);
      }

  return result;
}

ComplexMatrix
x_el_div (double a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix  result (nr, nc, Complex(octave_NaN, octave_NaN));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
      {
	OCTAVE_QUIT;
	result.elem (b.ridx(i), j) = a / b.data (i);
      }

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc, (a / 0.0));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
      {
	OCTAVE_QUIT;
	result.elem (b.ridx(i), j) = a / b.data (i);
      }

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc, (a / 0.0));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
      {
	OCTAVE_QUIT;
	result.elem (b.ridx(i), j) = a / b.data (i);
      }

  return result;
}

// Left division functions.
//
//              op2 \ op1:   m   cm
//                   +--   +---+----+
//   matrix                | 1 |  5 |
//                         +---+----+
//   complex_matrix        | 2 |  6 |
//                         +---+----+
//   sparse matrix         | 3 |  7 |
//                         +---+----+
//   sparse complex_matrix | 4 |  8 |
//                         +---+----+

// -*- 1 -*-
Matrix
xleftdiv (const SparseMatrix& a, const Matrix& b)
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
xleftdiv (const SparseMatrix& a, const ComplexMatrix& b)
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
SparseMatrix
xleftdiv (const SparseMatrix& a, const SparseMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      SparseMatrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

// -*- 4 -*-
SparseComplexMatrix
xleftdiv (const SparseMatrix& a, const SparseComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      SparseComplexMatrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

// -*- 5 -*-
ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const Matrix& b)
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

// -*- 6 -*-
ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const ComplexMatrix& b)
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

// -*- 7 -*-
SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      SparseComplexMatrix result
	= a.solve (b, info, rcond, solve_singularity_warning);

      if (result_ok (info))
	return result;
    }

  octave_idx_type rank;
  return a.lssolve (b, info, rank);
}

// -*- 8 -*-
SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;

      SparseComplexMatrix result
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
