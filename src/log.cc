// f-log.cc                                           -*- C++ -*-
/*

Copyright (C) 1994 John W. Eaton

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

#include "EIG.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "defun-dld.h"

// XXX FIXME XXX -- the next two functions should really be just one...

DEFUN_DLD ("logm", Flogm, Slogm, 2, 1,
  "logm (X): matrix logarithm")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    {
      print_usage ("logm");
      return retval;
    }

  tree_constant tmp = args(1).make_numeric ();;
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("logm", 0);

	  retval.resize (1, Matrix ());
	  return retval;
	}
      else
	gripe_empty_arg ("logm", 1);
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("logm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = log (real (elt));
		else
		  lambda.elem (i) = log (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval(0) = result;
	  }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("logm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = log (real (elt));
		else
		  lambda.elem (i) = log (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval(0) = result;
	  }
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	if (d > 0.0)
	  retval(0) = log (d);
	else
	  {
	    Complex dtmp (d);
	    retval(0) = log (dtmp);
	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval(0) = log (c);
      }
      break;
    default:
      break;
    }
  return retval;
}

DEFUN_DLD ("sqrtm", Fsqrtm, Ssqrtm, 2, 1,
 "sqrtm (X): matrix sqrt")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    {
      print_usage ("sqrtm");
      return retval;
    }

  tree_constant tmp = args(1).make_numeric ();;
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("sqrtm", 0);

	  retval.resize (1, Matrix ());
	  return retval;
	}
      else
	gripe_empty_arg ("sqrtm", 1);
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("sqrtm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = sqrt (real (elt));
		else
		  lambda.elem (i) = sqrt (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval(0) = result;
	  }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("sqrtm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = sqrt (real (elt));
		else
		  lambda.elem (i) = sqrt (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval(0) = result;
	  }
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	if (d > 0.0)
	  retval(0) = sqrt (d);
	else
	  {
	    Complex dtmp (d);
	    retval(0) = sqrt (dtmp);
	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval(0) = log (c);
      }
      break;
    default:
      break;
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
