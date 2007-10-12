/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2003, 2004,
              2005, 2007 John W. Eaton

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

#include "NLEqn.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"

typedef octave_idx_type (*hybrd1_fcn_ptr) (octave_idx_type*, double*, double*, octave_idx_type*);

typedef octave_idx_type (*hybrj1_fcn_ptr) (octave_idx_type*, double*, double*, double*, octave_idx_type*, octave_idx_type*);

extern "C"
{
  F77_RET_T
  F77_FUNC (hybrd1, HYBRD1) (hybrd1_fcn_ptr, const octave_idx_type&, double*,
			     double*, const double&, octave_idx_type&, double*,
			     const octave_idx_type&);


  F77_RET_T
  F77_FUNC (hybrj1, HYBRJ1) (hybrj1_fcn_ptr, const octave_idx_type&, double*,
			     double*, double*, const octave_idx_type&, const
			     double&, octave_idx_type&, double*, const octave_idx_type&);
}

static NLFunc::nonlinear_fcn user_fun;
static NLFunc::jacobian_fcn user_jac;

// error handling

void
NLEqn::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal NLEqn error: %s", msg);
}

// Other operations

octave_idx_type
hybrd1_fcn (octave_idx_type *n, double *x, double *fvec, octave_idx_type *iflag)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  octave_idx_type nn = *n;
  ColumnVector tmp_f (nn);
  ColumnVector tmp_x (nn);

  for (octave_idx_type i = 0; i < nn; i++)
    tmp_x.elem (i) = x[i];

  tmp_f = (*user_fun) (tmp_x);

  if (tmp_f.length () == 0)
    *iflag = -1;
  else
    {
      for (octave_idx_type i = 0; i < nn; i++)
	fvec[i] = tmp_f.elem (i);
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

octave_idx_type
hybrj1_fcn (octave_idx_type *n, double *x, double *fvec, double *fjac,
	    octave_idx_type *ldfjac, octave_idx_type *iflag)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  octave_idx_type nn = *n;
  ColumnVector tmp_x (nn);

  for (octave_idx_type i = 0; i < nn; i++)
    tmp_x.elem (i) = x[i];

  octave_idx_type flag = *iflag;
  if (flag == 1)
    {
      ColumnVector tmp_f (nn);

      tmp_f = (*user_fun) (tmp_x);

      if (tmp_f.length () == 0)
	*iflag = -1;
      else
	{
	  for (octave_idx_type i = 0; i < nn; i++)
	    fvec[i] = tmp_f.elem (i);
	}
    }
  else
    {
      Matrix tmp_fj (nn, nn);

      tmp_fj = (*user_jac) (tmp_x);

      if (tmp_fj.rows () == 0 || tmp_fj.columns () == 0)
	*iflag = -1;
      else
	{
	  octave_idx_type ld = *ldfjac;
	  for (octave_idx_type j = 0; j < nn; j++)
	    for (octave_idx_type i = 0; i < nn; i++)
	      fjac[j*ld+i] = tmp_fj.elem (i, j);
	}
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

ColumnVector
NLEqn::solve (octave_idx_type& info)
{
  ColumnVector retval;

  octave_idx_type n = x.capacity ();

  if (n == 0)
    {
      error ("equation set not initialized");
      return retval;
    }

  double tol = tolerance ();

  retval = x;
  double *px = retval.fortran_vec ();

  user_fun = fun;
  user_jac = jac;

  if (jac)
    {
      Array<double> fvec (n);
      double *pfvec = fvec.fortran_vec ();

      octave_idx_type lwa = (n*(n+13))/2;
      Array<double> wa (lwa);
      double *pwa = wa.fortran_vec ();

      Array<double> fjac (n*n);
      double *pfjac = fjac.fortran_vec ();

      F77_XFCN (hybrj1, HYBRJ1, (hybrj1_fcn, n, px, pfvec, pfjac, n,
				 tol, info, pwa, lwa));

      solution_status = info;

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in hybrj1");
    }
  else
    {
      Array<double> fvec (n);
      double *pfvec = fvec.fortran_vec ();

      octave_idx_type lwa = (n*(3*n+13))/2;
      Array<double> wa (lwa);
      double *pwa = wa.fortran_vec ();

      F77_XFCN (hybrd1, HYBRD1, (hybrd1_fcn, n, px, pfvec, tol, info,
				 pwa, lwa));

      solution_status = info;

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in hybrd1");
    }

  return retval;
}

std::string
NLEqn::error_message (void) const
{
  std::string retval;

  std::string prefix;

  octave_idx_type info = solution_status;
  if (info < 0)
    info = -info;

  switch (info)
    {
    case 0:
      retval = "improper input parameters";
      break;

    case 1:
      retval = "solution converged within specified tolerance";
      break;

    case 2:
      retval = "number of function calls exceeded limit";
      break;

    case 3:
      retval = "no further improvement possible (tolerance may be too small)";
      break;

    case 4:
      retval = "iteration is not making good progress";
      break;

    default:
      retval = "unknown error state";
      break;
    }

  if (solution_status < 0)
    retval = std::string ("user requested termination: ") + retval;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
