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

#include "EIG.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "user-prefs.h"
#include "utils.h"

// XXX FIXME XXX -- the next two functions should really be just
// one...

DEFUN_DLD_BUILTIN (logm, args, ,
  "logm (X): matrix logarithm")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("logm");
      return retval;
    }

  tree_constant arg = args(0);

  int arg_is_empty = empty_arg ("logm", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Matrix ();

  if (arg.is_real_scalar ())
    {
      double d = arg.double_value ();
      if (d > 0.0)
	retval(0) = log (d);
      else
	{
	  Complex dtmp (d);
	  retval(0) = log (dtmp);
	}
    }
  else if (arg.is_complex_scalar ())
    {
      Complex c = arg.complex_value ();
      retval(0) = log (c);
    }
  else if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
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
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
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
    }
  else
    {
      gripe_wrong_type_arg ("logm", arg);
    }

  return retval;
}

DEFUN_DLD_BUILTIN (sqrtm, args, ,
 "sqrtm (X): matrix sqrt")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("sqrtm");
      return retval;
    }

  tree_constant arg = args(0);

  int arg_is_empty = empty_arg ("sqrtm", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Matrix ();

  if (arg.is_real_scalar ())
    {
      double d = arg.double_value ();
      if (d > 0.0)
	retval(0) = sqrt (d);
      else
	{
	  Complex dtmp (d);
	  retval(0) = sqrt (dtmp);
	}
    }
  else if (arg.is_complex_scalar ())
    {
      Complex c = arg.complex_value ();
      retval(0) = log (c);
    }
  else if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
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
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
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
    }
  else
    {
      gripe_wrong_type_arg ("sqrtm", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
