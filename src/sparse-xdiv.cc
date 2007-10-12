/*

Copyright (C) 2004, 2005, 2006, 2007 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

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
xdiv (const Matrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return Matrix ();

  Matrix atmp = a.transpose ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  Matrix result = btmp.solve (btyp, atmp, info, rcond, 
			      solve_singularity_warning);

  typ = btyp.transpose ();
  return result.transpose ();
}

// -*- 2 -*-
ComplexMatrix
xdiv (const Matrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  Matrix atmp = a.transpose ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  ComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 3 -*-
ComplexMatrix
xdiv (const ComplexMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  ComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 4 -*-
ComplexMatrix
xdiv (const ComplexMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  ComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 5 -*-
SparseMatrix
xdiv (const SparseMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseMatrix ();

  SparseMatrix atmp = a.transpose ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseMatrix result = btmp.solve (btyp, atmp, info, rcond, 
				    solve_singularity_warning);

  typ = btyp.transpose ();
  return result.transpose ();
}

// -*- 6 -*-
SparseComplexMatrix
xdiv (const SparseMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseMatrix atmp = a.transpose ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 7 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseComplexMatrix atmp = a.hermitian ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 8 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseComplexMatrix atmp = a.hermitian ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
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
xleftdiv (const SparseMatrix& a, const Matrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return Matrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 2 -*-
ComplexMatrix
xleftdiv (const SparseMatrix& a, const ComplexMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 3 -*-
SparseMatrix
xleftdiv (const SparseMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 4 -*-
SparseComplexMatrix
xleftdiv (const SparseMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 5 -*-
ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const Matrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 6 -*-
ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const ComplexMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 7 -*-
SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 8 -*-
SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b, 
	  MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
