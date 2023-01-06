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

#include <cinttypes>
#include <sstream>

#include "DASSL.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"

typedef F77_INT (*dassl_fcn_ptr) (const double&, const double *,
                                  const double *, double *, F77_INT&,
                                  double *, F77_INT *);

typedef F77_INT (*dassl_jac_ptr) (const double&, const double *,
                                  const double *, double *, const double&,
                                  double *, F77_INT *);

extern "C"
{
  F77_RET_T
  F77_FUNC (ddassl, DDASSL) (dassl_fcn_ptr, const F77_INT&, F77_DBLE&,
                             F77_DBLE *, F77_DBLE *, F77_DBLE&, const F77_INT *,
                             const F77_DBLE *, const F77_DBLE *, F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_INT *,
                             const F77_INT&, const F77_DBLE *, const F77_INT *,
                             dassl_jac_ptr);
}

static DAEFunc::DAERHSFunc user_fcn;
static DAEFunc::DAEJacFunc user_jac;

static F77_INT nn;

static F77_INT
ddassl_f (const double& time, const double *state, const double *deriv,
          double *delta, F77_INT& ires, double *, F77_INT *)
{
  // FIXME: would be nice to avoid copying the data.

  ColumnVector tmp_deriv (nn);
  ColumnVector tmp_state (nn);
  ColumnVector tmp_delta (nn);

  for (F77_INT i = 0; i < nn; i++)
    {
      tmp_deriv.elem (i) = deriv[i];
      tmp_state.elem (i) = state[i];
    }

  octave_idx_type tmp_ires = ires;

  tmp_delta = user_fcn (tmp_state, tmp_deriv, time, tmp_ires);

  ires = octave::to_f77_int (tmp_ires);

  if (ires >= 0)
    {
      if (tmp_delta.isempty ())
        ires = -2;
      else
        {
          for (F77_INT i = 0; i < nn; i++)
            delta[i] = tmp_delta.elem (i);
        }
    }

  return 0;
}

static F77_INT
ddassl_j (const double& time, const double *state, const double *deriv,
          double *pd, const double& cj, double *, F77_INT *)
{
  // FIXME: would be nice to avoid copying the data.

  ColumnVector tmp_state (nn);
  ColumnVector tmp_deriv (nn);

  for (F77_INT i = 0; i < nn; i++)
    {
      tmp_deriv.elem (i) = deriv[i];
      tmp_state.elem (i) = state[i];
    }

  Matrix tmp_pd = user_jac (tmp_state, tmp_deriv, time, cj);

  for (F77_INT j = 0; j < nn; j++)
    for (F77_INT i = 0; i < nn; i++)
      pd[nn * j + i] = tmp_pd.elem (i, j);

  return 0;
}

ColumnVector
DASSL::do_integrate (double tout)
{
  ColumnVector retval;

  if (! m_initialized || m_restart || DAEFunc::m_reset
      || DASSL_options::m_reset)
    {
      m_integration_error = false;

      m_initialized = true;

      m_info.resize (dim_vector (15, 1));

      for (F77_INT i = 0; i < 15; i++)
        m_info(i) = 0;

      F77_INT n = octave::to_f77_int (size ());

      m_liw = 21 + n;
      m_lrw = 40 + 9*n + n*n;

      nn = n;

      m_iwork.resize (dim_vector (m_liw, 1));
      m_rwork.resize (dim_vector (m_lrw, 1));

      m_info(0) = 0;

      if (m_stop_time_set)
        {
          m_rwork(0) = m_stop_time;
          m_info(3) = 1;
        }
      else
        m_info(3) = 0;

      m_restart = false;

      // DAEFunc

      user_fcn = DAEFunc::function ();
      user_jac = DAEFunc::jacobian_function ();

      if (user_fcn)
        {
          octave_idx_type ires = 0;

          ColumnVector res = (*user_fcn) (m_x, m_xdot, m_t, ires);

          if (res.numel () != m_x.numel ())
            {
              (*current_liboctave_error_handler)
                ("dassl: inconsistent sizes for state and residual vectors");

              m_integration_error = true;
              return retval;
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("dassl: no user supplied RHS subroutine!");

          m_integration_error = true;
          return retval;
        }

      m_info(4) = (user_jac ? 1 : 0);

      DAEFunc::m_reset = false;

      // DASSL_options

      double hmax = maximum_step_size ();
      if (hmax >= 0.0)
        {
          m_rwork(1) = hmax;
          m_info(6) = 1;
        }
      else
        m_info(6) = 0;

      double h0 = initial_step_size ();
      if (h0 >= 0.0)
        {
          m_rwork(2) = h0;
          m_info(7) = 1;
        }
      else
        m_info(7) = 0;

      F77_INT sl = octave::to_f77_int (step_limit ());

      if (sl >= 0)
        {
          m_info(11) = 1;
          m_iwork(20) = sl;
        }
      else
        m_info(11) = 0;

      F77_INT maxord = octave::to_f77_int (maximum_order ());
      if (maxord >= 0)
        {
          if (maxord > 0 && maxord < 6)
            {
              m_info(8) = 1;
              m_iwork(2) = maxord;
            }
          else
            {
              (*current_liboctave_error_handler)
                ("dassl: invalid value for maximum order: %"
                 OCTAVE_F77_INT_TYPE_FORMAT, maxord);
              m_integration_error = true;
              return retval;
            }
        }

      F77_INT enc = octave::to_f77_int (enforce_nonnegativity_constraints ());
      m_info(9) = (enc ? 1 : 0);

      F77_INT ccic = octave::to_f77_int (compute_consistent_initial_condition ());
      m_info(10) = (ccic ? 1 : 0);

      m_abs_tol = absolute_tolerance ();
      m_rel_tol = relative_tolerance ();

      F77_INT abs_tol_len = octave::to_f77_int (m_abs_tol.numel ());
      F77_INT rel_tol_len = octave::to_f77_int (m_rel_tol.numel ());

      if (abs_tol_len == 1 && rel_tol_len == 1)
        {
          m_info(1) = 0;
        }
      else if (abs_tol_len == n && rel_tol_len == n)
        {
          m_info(1) = 1;
        }
      else
        {
          (*current_liboctave_error_handler)
            ("dassl: inconsistent sizes for tolerance arrays");

          m_integration_error = true;
          return retval;
        }

      DASSL_options::m_reset = false;
    }

  double *px = m_x.fortran_vec ();
  double *pxdot = m_xdot.fortran_vec ();

  F77_INT *pinfo = m_info.fortran_vec ();

  double *prel_tol = m_rel_tol.fortran_vec ();
  double *pabs_tol = m_abs_tol.fortran_vec ();

  double *prwork = m_rwork.fortran_vec ();
  F77_INT *piwork = m_iwork.fortran_vec ();

  double *dummy = nullptr;
  F77_INT *idummy = nullptr;

  F77_INT tmp_istate = octave::to_f77_int (m_istate);

  F77_XFCN (ddassl, DDASSL, (ddassl_f, nn, m_t, px, pxdot, tout, pinfo,
                             prel_tol, pabs_tol, tmp_istate, prwork, m_lrw,
                             piwork, m_liw, dummy, idummy, ddassl_j));

  m_istate = tmp_istate;

  switch (m_istate)
    {
    case 1: // A step was successfully taken in intermediate-output
            // mode.  The code has not yet reached TOUT.
    case 2: // The integration to TSTOP was successfully completed
            // (T=TSTOP) by stepping exactly to TSTOP.
    case 3: // The integration to TOUT was successfully completed
            // (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
            // interpolation.  YPRIME(*) is obtained by interpolation.
      retval = m_x;
      m_t = tout;
      break;

    case -1: // A large amount of work has been expended.  (~500 steps).
    case -2: // The error tolerances are too stringent.
    case -3: // The local error test cannot be satisfied because you
             // specified a zero component in ATOL and the
             // corresponding computed solution component is zero.
             // Thus, a pure relative error test is impossible for
             // this component.
    case -6: // DDASSL had repeated error test failures on the last
             // attempted step.
    case -7: // The corrector could not converge.
    case -8: // The matrix of partial derivatives is singular.
    case -9: // The corrector could not converge.  There were repeated
             // error test failures in this step.
    case -10: // The corrector could not converge because IRES was
              // equal to minus one.
    case -11: // IRES equal to -2 was encountered and control is being
              // returned to the calling program.
    case -12: // DDASSL failed to compute the initial YPRIME.
    case -33: // The code has encountered trouble from which it cannot
              // recover.  A message is printed explaining the trouble
              // and control is returned to the calling program.  For
              // example, this occurs when invalid input is detected.
      m_integration_error = true;
      break;

    default:
      m_integration_error = true;
      (*current_liboctave_error_handler)
        ("unrecognized value of istate (= %" OCTAVE_IDX_TYPE_FORMAT ") "
         "returned from ddassl", m_istate);
      break;
    }

  return retval;
}

Matrix
DASSL::do_integrate (const ColumnVector& tout)
{
  Matrix dummy;
  return integrate (tout, dummy);
}

Matrix
DASSL::integrate (const ColumnVector& tout, Matrix& xdot_out)
{
  Matrix retval;

  octave_idx_type n_out = tout.numel ();
  F77_INT n = octave::to_f77_int (size ());

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (F77_INT i = 0; i < n; i++)
        {
          retval.elem (0, i) = m_x.elem (i);
          xdot_out.elem (0, i) = m_xdot.elem (i);
        }

      for (octave_idx_type j = 1; j < n_out; j++)
        {
          ColumnVector x_next = do_integrate (tout.elem (j));

          if (m_integration_error)
            return retval;

          for (F77_INT i = 0; i < n; i++)
            {
              retval.elem (j, i) = x_next.elem (i);
              xdot_out.elem (j, i) = m_xdot.elem (i);
            }
        }
    }

  return retval;
}

Matrix
DASSL::do_integrate (const ColumnVector& tout, const ColumnVector& tcrit)
{
  Matrix dummy;
  return integrate (tout, dummy, tcrit);
}

Matrix
DASSL::integrate (const ColumnVector& tout, Matrix& xdot_out,
                  const ColumnVector& tcrit)
{
  Matrix retval;

  octave_idx_type n_out = tout.numel ();
  F77_INT n = octave::to_f77_int (size ());

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (F77_INT i = 0; i < n; i++)
        {
          retval.elem (0, i) = m_x.elem (i);
          xdot_out.elem (0, i) = m_xdot.elem (i);
        }

      octave_idx_type n_crit = tcrit.numel ();

      if (n_crit > 0)
        {
          octave_idx_type i_crit = 0;
          octave_idx_type i_out = 1;
          double next_crit = tcrit.elem (0);
          double next_out;
          while (i_out < n_out)
            {
              bool do_restart = false;

              next_out = tout.elem (i_out);
              if (i_crit < n_crit)
                next_crit = tcrit.elem (i_crit);

              bool save_output;
              double t_out;

              if (next_crit == next_out)
                {
                  set_stop_time (next_crit);
                  t_out = next_out;
                  save_output = true;
                  i_out++;
                  i_crit++;
                  do_restart = true;
                }
              else if (next_crit < next_out)
                {
                  if (i_crit < n_crit)
                    {
                      set_stop_time (next_crit);
                      t_out = next_crit;
                      save_output = false;
                      i_crit++;
                      do_restart = true;
                    }
                  else
                    {
                      clear_stop_time ();
                      t_out = next_out;
                      save_output = true;
                      i_out++;
                    }
                }
              else
                {
                  set_stop_time (next_crit);
                  t_out = next_out;
                  save_output = true;
                  i_out++;
                }

              ColumnVector x_next = do_integrate (t_out);

              if (m_integration_error)
                return retval;

              if (save_output)
                {
                  for (F77_INT i = 0; i < n; i++)
                    {
                      retval.elem (i_out-1, i) = x_next.elem (i);
                      xdot_out.elem (i_out-1, i) = m_xdot.elem (i);
                    }
                }

              if (do_restart)
                force_restart ();
            }
        }
      else
        {
          retval = integrate (tout, xdot_out);

          if (m_integration_error)
            return retval;
        }
    }

  return retval;
}

std::string
DASSL::error_message (void) const
{
  std::string retval;

  std::ostringstream buf;
  buf << m_t;
  std::string t_curr = buf.str ();

  switch (m_istate)
    {
    case 1:
      retval = "a step was successfully taken in intermediate-output mode.";
      break;

    case 2:
      retval = "integration completed by stepping exactly to TOUT";
      break;

    case 3:
      retval = "integration to tout completed by stepping past TOUT";
      break;

    case -1:
      retval = "a large amount of work has been expended (t =" + t_curr + ')';
      break;

    case -2:
      retval = "the error tolerances are too stringent";
      break;

    case -3:
      retval = "error weight became zero during problem. (t = " + t_curr +
               "; solution component i vanished, and atol or atol(i) == 0)";
      break;

    case -6:
      retval = "repeated error test failures on the last attempted step (t = "
               + t_curr + ')';
      break;

    case -7:
      retval = "the corrector could not converge (t = " + t_curr + ')';
      break;

    case -8:
      retval = "the matrix of partial derivatives is singular (t = " + t_curr +
               ')';
      break;

    case -9:
      retval = "the corrector could not converge (t = " + t_curr +
               "; repeated test failures)";
      break;

    case -10:
      retval = "corrector could not converge because IRES was -1 (t = "
               + t_curr + ')';
      break;

    case -11:
      retval = "return requested in user-supplied function (t = " + t_curr +
               ')';
      break;

    case -12:
      retval = "failed to compute consistent initial conditions";
      break;

    case -33:
      retval = "unrecoverable error (see printed message)";
      break;

    default:
      retval = "unknown error state";
      break;
    }

  return retval;
}
