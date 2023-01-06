////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cassert>

#include "Array.h"
#include "Quad.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"

static integrand_fcn user_fcn;
static float_integrand_fcn float_user_fcn;

typedef F77_INT (*quad_fcn_ptr) (const double&, int&, double&);
typedef F77_INT (*quad_float_fcn_ptr) (const float&, int&, float&);

extern "C"
{
  F77_RET_T
  F77_FUNC (dqagp, DQAGP) (quad_fcn_ptr, const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_DBLE *,
                           const F77_DBLE&, const F77_DBLE&, F77_DBLE&,
                           F77_DBLE&, F77_INT&, F77_INT&,
                           const F77_INT&, const F77_INT&,
                           F77_INT&, F77_INT *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (dqagi, DQAGI) (quad_fcn_ptr, const F77_DBLE&,
                           const F77_INT&, const F77_DBLE&,
                           const F77_DBLE&, F77_DBLE&, F77_DBLE&,
                           F77_INT&, F77_INT&,
                           const F77_INT&, const F77_INT&,
                           F77_INT&, F77_INT *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (qagp, QAGP) (quad_float_fcn_ptr, const F77_REAL&, const F77_REAL&,
                         const F77_INT&, const F77_REAL *, const F77_REAL&,
                         const F77_REAL&, F77_REAL&, F77_REAL&, F77_INT&,
                         F77_INT&, const F77_INT&,
                         const F77_INT&, F77_INT&,
                         F77_INT *, F77_REAL *);

  F77_RET_T
  F77_FUNC (qagi, QAGI) (quad_float_fcn_ptr, const F77_REAL&,
                         const F77_INT&, const F77_REAL&,
                         const F77_REAL&, F77_REAL&, F77_REAL&, F77_INT&,
                         F77_INT&, const F77_INT&,
                         const F77_INT&, F77_INT&,
                         F77_INT *, F77_REAL *);
}

static F77_INT
user_function (const double& x, int&, double& result)
{
  result = (*user_fcn) (x);

  return 0;
}

static F77_INT
float_user_function (const float& x, int&, float& result)
{
  result = (*float_user_fcn) (x);

  return 0;
}

double
DefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                       double& abserr)
{
  F77_INT npts = octave::to_f77_int (m_singularities.numel () + 2);
  double *points = m_singularities.fortran_vec ();
  double result = 0.0;

  F77_INT leniw = 183*npts - 122;
  Array<F77_INT> iwork (dim_vector (leniw, 1));
  F77_INT *piwork = iwork.fortran_vec ();

  F77_INT lenw = 2*leniw - npts;
  Array<double> work (dim_vector (lenw, 1));
  double *pwork = work.fortran_vec ();

  user_fcn = m_f;
  F77_INT last;

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  // NEVAL and IER are output only parameters and F77_INT can not be a
  // wider type than octave_idx_type so we can create local variables
  // here that are the correct type for the Fortran subroutine and then
  // copy them to the function parameters without needing to preserve
  // and pass the values to the Fortran subroutine.

  F77_INT xneval, xier;

  F77_XFCN (dqagp, DQAGP, (user_function, m_lower_limit, m_upper_limit,
                           npts, points, abs_tol, rel_tol, result,
                           abserr, xneval, xier, leniw, lenw, last,
                           piwork, pwork));

  neval = xneval;
  ier = xier;

  return result;
}

float
DefQuad::do_integrate (octave_idx_type&, octave_idx_type&, float&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
}

double
IndefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                         double& abserr)
{
  double result = 0.0;

  F77_INT leniw = 128;
  Array<F77_INT> iwork (dim_vector (leniw, 1));
  F77_INT *piwork = iwork.fortran_vec ();

  F77_INT lenw = 8*leniw;
  Array<double> work (dim_vector (lenw, 1));
  double *pwork = work.fortran_vec ();

  user_fcn = m_f;
  F77_INT last;

  F77_INT inf;
  switch (m_type)
    {
    case bound_to_inf:
      inf = 1;
      break;

    case neg_inf_to_bound:
      inf = -1;
      break;

    case doubly_infinite:
      inf = 2;
      break;

    default:
      assert (0);
      break;
    }

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  // NEVAL and IER are output only parameters and F77_INT can not be a
  // wider type than octave_idx_type so we can create local variables
  // here that are the correct type for the Fortran subroutine and then
  // copy them to the function parameters without needing to preserve
  // and pass the values to the Fortran subroutine.

  F77_INT xneval, xier;

  F77_XFCN (dqagi, DQAGI, (user_function, m_bound, inf, abs_tol, rel_tol,
                           result, abserr, xneval, xier, leniw, lenw,
                           last, piwork, pwork));

  neval = xneval;
  ier = xier;

  return result;
}

float
IndefQuad::do_integrate (octave_idx_type&, octave_idx_type&, float&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
}

double
FloatDefQuad::do_integrate (octave_idx_type&, octave_idx_type&, double&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
}

float
FloatDefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                            float& abserr)
{
  F77_INT npts = octave::to_f77_int (m_singularities.numel () + 2);
  float *points = m_singularities.fortran_vec ();
  float result = 0.0;

  F77_INT leniw = 183*npts - 122;
  Array<F77_INT> iwork (dim_vector (leniw, 1));
  F77_INT *piwork = iwork.fortran_vec ();

  F77_INT lenw = 2*leniw - npts;
  Array<float> work (dim_vector (lenw, 1));
  float *pwork = work.fortran_vec ();

  float_user_fcn = m_ff;
  F77_INT last;

  float abs_tol = single_precision_absolute_tolerance ();
  float rel_tol = single_precision_relative_tolerance ();

  // NEVAL and IER are output only parameters and F77_INT can not be a
  // wider type than octave_idx_type so we can create local variables
  // here that are the correct type for the Fortran subroutine and then
  // copy them to the function parameters without needing to preserve
  // and pass the values to the Fortran subroutine.

  F77_INT xneval, xier;

  F77_XFCN (qagp, QAGP, (float_user_function, m_lower_limit, m_upper_limit,
                         npts, points, abs_tol, rel_tol, result,
                         abserr, xneval, xier, leniw, lenw, last,
                         piwork, pwork));

  neval = xneval;
  ier = xier;

  return result;
}

double
FloatIndefQuad::do_integrate (octave_idx_type&, octave_idx_type&, double&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
}

float
FloatIndefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                              float& abserr)
{
  float result = 0.0;

  F77_INT leniw = 128;
  Array<F77_INT> iwork (dim_vector (leniw, 1));
  F77_INT *piwork = iwork.fortran_vec ();

  F77_INT lenw = 8*leniw;
  Array<float> work (dim_vector (lenw, 1));
  float *pwork = work.fortran_vec ();

  float_user_fcn = m_ff;
  F77_INT last;

  F77_INT inf;
  switch (m_type)
    {
    case bound_to_inf:
      inf = 1;
      break;

    case neg_inf_to_bound:
      inf = -1;
      break;

    case doubly_infinite:
      inf = 2;
      break;

    default:
      assert (0);
      break;
    }

  float abs_tol = single_precision_absolute_tolerance ();
  float rel_tol = single_precision_relative_tolerance ();

  // NEVAL and IER are output only parameters and F77_INT can not be a
  // wider type than octave_idx_type so we can create local variables
  // here that are the correct type for the Fortran subroutine and then
  // copy them to the function parameters without needing to preserve
  // and pass the values to the Fortran subroutine.

  F77_INT xneval, xier;

  F77_XFCN (qagi, QAGI, (float_user_function, m_bound, inf, abs_tol, rel_tol,
                         result, abserr, xneval, xier, leniw, lenw,
                         last, piwork, pwork));

  neval = xneval;
  ier = xier;

  return result;
}
