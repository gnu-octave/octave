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
#include <climits>

#include "CColVector.h"
#include "CDiagMatrix.h"
#include "CMatrix.h"
#include "EIG.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "oct-cmplx.h"

#include "error.h"
#include "pt-const.h"
#include "utils.h"
#include "xpow.h"

// This function also appears in tree-const.cc.  Maybe it should be a
// member function of the Matrix class.

static int
any_element_is_negative (const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a (i, j) < 0.0)
	return 1;
  return 0;
}

static inline int
xisint (double x)
{
  return (D_NINT (x) == x
	  && ((x >= 0 && x < INT_MAX)
	      || (x <= 0 && x > INT_MIN)));
}

// Safer pow functions.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | 1 | 5 |  7 | 11 |
//                  +---+---+----+----+
//   matrix         | 2 | E |  8 |  E |
//                  +---+---+----+----+
//   complex_scalar | 3 | 6 |  9 | 12 |
//                  +---+---+----+----+
//   complex_matrix | 4 | E | 10 |  E |
//                  +---+---+----+----+
//
//   E -> error, trapped in arith-ops.cc.

// -*- 1 -*-
octave_value
xpow (double a, double b)
{
  if (a < 0.0 && (int) b != b)
    {
      Complex atmp (a);
      return pow (atmp, b);
    }
  else
    return pow (a, b);
}

// -*- 2 -*-
octave_value
xpow (double a, const Matrix& b)
{
  octave_value retval;

  int nr = b.rows ();
  int nc = b.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be square");
  else
    {
      EIG b_eig (b);
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.eigenvectors ());

      for (int i = 0; i < nr; i++)
	{
	  Complex elt = lambda (i);
	  if (imag (elt) == 0.0)
	    lambda (i) = pow (a, real (elt));
	  else
	    lambda (i) = pow (a, elt);
	}
      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }

  return retval;
}

// -*- 3 -*-
octave_value
xpow (double a, const Complex& b)
{
  Complex result;
  Complex atmp (a);
  result = pow (atmp, b);
  return result;
}

// -*- 4 -*-
octave_value
xpow (double a, const ComplexMatrix& b)
{
  octave_value retval;

  int nr = b.rows ();
  int nc = b.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for x^A, A must be square");
    }
  else
    {
      EIG b_eig (b);
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.eigenvectors ());

      for (int i = 0; i < nr; i++)
	{
	  Complex elt = lambda (i);
	  if (imag (elt) == 0.0)
	    lambda (i) = pow (a, real (elt));
	  else
	    lambda (i) = pow (a, elt);
	}
      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }

  return retval;
}

// -*- 5 -*-
octave_value
xpow (const Matrix& a, double b)
{
  octave_value retval;

  int nr = a.rows ();
  int nc = a.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for A^b, A must be square");
    }
  else
    {
      if ((int) b == b)
	{
	  int btmp = (int) b;
	  if (btmp == 0)
	    {
	      retval = DiagMatrix (nr, nr, 1.0);
	    }
	  else
	    {
	      // Too much copying?
	      // XXX FIXME XXX -- we shouldn't do this if the exponent is
	      // large...

	      Matrix atmp;
	      if (btmp < 0)
		{
		  btmp = -btmp;

		  int info;
		  double rcond = 0.0;

		  atmp = a.inverse (info, rcond, 1);

		  if (info == -1)
		    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
		}
	      else
		atmp = a;

	      Matrix result (atmp);
	      for (int i = 1; i < btmp; i++)
		result = result * atmp;

	      retval = result;
	    }
	}
      else
	{
	  EIG a_eig (a);
	  ComplexColumnVector lambda (a_eig.eigenvalues ());
	  ComplexMatrix Q (a_eig.eigenvectors ());

	  for (int i = 0; i < nr; i++)
	    lambda (i) = pow (lambda (i), b);

	  ComplexDiagMatrix D (lambda);

	  retval = ComplexMatrix (Q * D * Q.inverse ());
	}
    }

  return retval;
}

// -*- 6 -*-
octave_value
xpow (const Matrix& a, const Complex& b)
{
  octave_value retval;

  int nr = a.rows ();
  int nc = a.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for A^b, A must be square");
    }
  else
    {
      EIG a_eig (a);
      ComplexColumnVector lambda (a_eig.eigenvalues ());
      ComplexMatrix Q (a_eig.eigenvectors ());

      for (int i = 0; i < nr; i++)
	lambda (i) = pow (lambda (i), b);

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }

  return retval;
}

// -*- 7 -*-
octave_value
xpow (const Complex& a, double b)
{
  Complex result;

  if (xisint (b))
    result = pow (a, (int) b);
  else
    result = pow (a, b);

  return result;
}

// -*- 8 -*-
octave_value
xpow (const Complex& a, const Matrix& b)
{
  octave_value retval;

  int nr = b.rows ();
  int nc = b.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for x^A, A must be square");
    }
  else
    {
      EIG b_eig (b);
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.eigenvectors ());

      for (int i = 0; i < nr; i++)
	{
	  Complex elt = lambda (i);
	  if (imag (elt) == 0.0)
	    lambda (i) = pow (a, real (elt));
	  else
	    lambda (i) = pow (a, elt);
	}
      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }

  return retval;
}

// -*- 9 -*-
octave_value
xpow (const Complex& a, const Complex& b)
{
  Complex result;
  result = pow (a, b);
  return result;
}

// -*- 10 -*-
octave_value
xpow (const Complex& a, const ComplexMatrix& b)
{
  octave_value retval;

  int nr = b.rows ();
  int nc = b.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for x^A, A must be square");
    }
  else
    {
      EIG b_eig (b);
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.eigenvectors ());

      for (int i = 0; i < nr; i++)
	{
	  Complex elt = lambda (i);
	  if (imag (elt) == 0.0)
	    lambda (i) = pow (a, real (elt));
	  else
	    lambda (i) = pow (a, elt);
	}
      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }

  return retval;
}

// -*- 11 -*-
octave_value
xpow (const ComplexMatrix& a, double b)
{
  octave_value retval;

  int nr = a.rows ();
  int nc = a.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for A^b, A must be square");
    }
  else
    {
      if ((int) b == b)
	{
	  int btmp = (int) b;
	  if (btmp == 0)
	    {
	      retval = DiagMatrix (nr, nr, 1.0);
	    }
	  else
	    {
	      // Too much copying?
	      // XXX FIXME XXX -- we shouldn't do this if the exponent is
	      // large...

	      ComplexMatrix atmp;
	      if (btmp < 0)
		{
		  btmp = -btmp;

		  int info;
		  double rcond = 0.0;

		  atmp = a.inverse (info, rcond, 1);

		  if (info == -1)
		    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
		}
	      else
		atmp = a;

	      ComplexMatrix result (atmp);
	      for (int i = 1; i < btmp; i++)
		result = result * atmp;

	      retval = result;
	    }
	}
      else
	{
	  EIG a_eig (a);
	  ComplexColumnVector lambda (a_eig.eigenvalues ());
	  ComplexMatrix Q (a_eig.eigenvectors ());

	  for (int i = 0; i < nr; i++)
	    lambda (i) = pow (lambda (i), b);

	  ComplexDiagMatrix D (lambda);

	  retval = ComplexMatrix (Q * D * Q.inverse ());
	}
    }

  return retval;
}

// -*- 12 -*-
octave_value
xpow (const ComplexMatrix& a, const Complex& b)
{
  octave_value retval;

  int nr = a.rows ();
  int nc = a.columns ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      error ("for A^b, A must be square");
    }
  else
    {
      EIG a_eig (a);
      ComplexColumnVector lambda (a_eig.eigenvalues ());
      ComplexMatrix Q (a_eig.eigenvectors ());

      for (int i = 0; i < nr; i++)
	lambda (i) = pow (lambda (i), b);

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }

  return retval;
}

// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// -*- 1 -*-
octave_value
elem_xpow (double a, const Matrix& b)
{
  octave_value retval;

  int nr = b.rows ();
  int nc = b.columns ();

  // For now, assume the worst.

  if (a < 0.0)
    {
      Complex atmp (a);
      ComplexMatrix result (nr, nc);
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result (i, j) = pow (atmp, b (i, j));

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result (i, j) = pow (a, b (i, j)); 

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const ComplexMatrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = pow (a, b (i, j));

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const Matrix& a, double b)
{
  octave_value retval;

  int nr = a.rows ();
  int nc = a.columns ();

  if ((int) b != b && any_element_is_negative (a))
    {
      ComplexMatrix result (nr, nc);
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  {
	    Complex atmp (a (i, j));
	    result (i, j) = pow (atmp, b);
	  }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result (i, j) = pow (a (i, j), b);

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const Matrix& a, const Matrix& b)
{
  octave_value retval;

  int nr = a.rows ();
  int nc = a.columns ();

  assert (nr == b.rows () && nc == b.columns ());

  int convert_to_complex = 0;
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double atmp = a (i, j);
	double btmp = b (i, j);
	if (atmp < 0.0 && (int) btmp != btmp)
	  {
	    convert_to_complex = 1;
	    goto done;
	  }
      }

 done:

  if (convert_to_complex)
    {
      ComplexMatrix complex_result (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  {
	    Complex atmp (a (i, j));
	    Complex btmp (b (i, j));
	    complex_result (i, j) = pow (atmp, btmp);
	  }

      retval = complex_result;
    }
  else
    {
      Matrix result (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result (i, j) = pow (a (i, j), b (i, j));

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const Matrix& a, const Complex& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = pow (a (i, j), b);

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const Matrix& a, const ComplexMatrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  assert (nr == b.rows () && nc == b.columns ());

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = pow (a (i, j), b (i, j));

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const Matrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double btmp = b (i, j);
	if (xisint (btmp))
	  result (i, j) = pow (a, (int) btmp);
	else
	  result (i, j) = pow (a, btmp);
      }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const ComplexMatrix& b)
{
  int nr = b.rows ();
  int nc = b.columns ();

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = pow (a, b (i, j));

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const ComplexMatrix& a, double b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  ComplexMatrix result (nr, nc);

  if (xisint (b))
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result (i, j) = pow (a (i, j), (int) b);
    }
  else
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result (i, j) = pow (a (i, j), b);
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  assert (nr == b.rows () && nc == b.columns ());

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double btmp = b (i, j);
	if (xisint (btmp))
	  result (i, j) = pow (a (i, j), (int) btmp);
	else
	  result (i, j) = pow (a (i, j), btmp);
      }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const Complex& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  ComplexMatrix result (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = pow (a (i, j), b);

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const ComplexMatrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result (i, j) = pow (a (i, j), b (i, j));

  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
