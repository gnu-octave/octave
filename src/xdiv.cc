// xdiv.cc                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
#include "config.h"
#endif

#include <assert.h>
#include <Complex.h>

#include "xdiv.h"
#include "dMatrix.h"
#include "CMatrix.h"
#include "tree-const.h"
#include "error.h"

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

static inline int
mx_leftdiv_conform (int a_nr, int a_nc, int b_nr, int warn = 1)
{
  if (a_nr != b_nr)
    {
      error ("number of rows must be the same for left division");
      return 0;
    }

  return 1;
}

static inline int
mx_div_conform (int b_nr, int b_nc, int a_nc, int warn = 1)
{
  if (a_nc != b_nc)
    {
      error ("number of columns must be the same for right division");
      return 0;
    }

  return 1;
}

// Right division functions.
//
//       op2 / op1:   m   cm
//            +--   +---+----+
//   matrix         | 1 |  3 |
//                  +---+----+
//   complex_matrix | 2 |  4 |
//                  +---+----+

// -*- 1 -*-
tree_constant
xdiv (const Matrix& a, const Matrix& b)
{
  if (! mx_div_conform (b.rows (), b.columns (), a.columns ()))
    return tree_constant ();

  Matrix atmp = a.transpose ();
  Matrix btmp = b.transpose ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      Matrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result.transpose ());
    }

  int rank;
  Matrix result = btmp.lssolve (atmp, info, rank);

  return tree_constant (result.transpose ());
}

// -*- 2 -*-
tree_constant
xdiv (const Matrix& a, const ComplexMatrix& b)
{
  if (! mx_div_conform (b.rows (), b.columns (), a.columns ()))
    return tree_constant ();

  Matrix atmp = a.transpose ();
  ComplexMatrix btmp = b.hermitian ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result.hermitian ());
    }

  int rank;
  ComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return tree_constant (result.hermitian ());
}

// -*- 3 -*-
tree_constant
xdiv (const ComplexMatrix& a, const Matrix& b)
{
  if (! mx_div_conform (b.rows (), b.columns (), a.columns ()))
    return tree_constant ();

  ComplexMatrix atmp = a.hermitian ();
  Matrix btmp = b.transpose ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result.hermitian ());
    }

  int rank;
  ComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return tree_constant (result.hermitian ());
}

// -*- 4 -*-
tree_constant
xdiv (const ComplexMatrix& a, const ComplexMatrix& b)
{
  if (! mx_div_conform (b.rows (), b.columns (), a.columns ()))
    return tree_constant ();

  ComplexMatrix atmp = a.hermitian ();
  ComplexMatrix btmp = b.hermitian ();

  int info;
  if (btmp.rows () == btmp.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = btmp.solve (atmp, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result.hermitian ());
    }

  int rank;
  ComplexMatrix result = btmp.lssolve (atmp, info, rank);

  return tree_constant (result.hermitian ());
}

// Funny element by element division operations.
//
//       op2 \ op1:   s   cs
//            +--   +---+----+
//   matrix         | 1 |  3 |
//                  +---+----+
//   complex_matrix | 2 |  4 |
//                  +---+----+

tree_constant
x_el_div (double a, const Matrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result.elem (i, j) = a / b.elem (i, j);

  return tree_constant (result);
}

tree_constant
x_el_div (double a, const ComplexMatrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result.elem (i, j) = a / b.elem (i, j);

  return tree_constant (result);
}

tree_constant
x_el_div (const Complex a, const Matrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result.elem (i, j) = a / b.elem (i, j);

  return tree_constant (result);
}

tree_constant
x_el_div (const Complex a, const ComplexMatrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result.elem (i, j) = a / b.elem (i, j);

  return tree_constant (result);
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
tree_constant
xleftdiv (const Matrix& a, const Matrix& b)
{
  if (! mx_leftdiv_conform (a.rows (), a.columns (), b.rows ()))
    return tree_constant ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      Matrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result);
    }

  int rank;
  Matrix result = a.lssolve (b, info, rank);

  return tree_constant (result);
}

// -*- 2 -*-
tree_constant
xleftdiv (const Matrix& a, const ComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a.rows (), a.columns (), b.rows ()))
    return tree_constant ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result);
    }

  int rank;
  ComplexMatrix result = a.lssolve (b, info, rank);

  return tree_constant (result);
}

// -*- 3 -*-
tree_constant
xleftdiv (const ComplexMatrix& a, const Matrix& b)
{
  if (! mx_leftdiv_conform (a.rows (), a.columns (), b.rows ()))
    return tree_constant ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result);
    }

  int rank;
  ComplexMatrix result = a.lssolve (b, info, rank);

  return tree_constant (result);
}

// -*- 4 -*-
tree_constant
xleftdiv (const ComplexMatrix& a, const ComplexMatrix& b)
{
  if (! mx_leftdiv_conform (a.rows (), a.columns (), b.rows ()))
    return tree_constant ();

  int info;
  if (a.rows () == a.columns ())
    {
      double rcond = 0.0;
      ComplexMatrix result = a.solve (b, info, rcond);
      if (result_ok (info, rcond))
	return tree_constant (result);
    }

  int rank;
  ComplexMatrix result = a.lssolve (b, info, rank);

  return tree_constant (result);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
