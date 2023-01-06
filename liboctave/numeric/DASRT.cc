////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2023 The Octave Project Developers
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

#include "DASRT.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"

typedef F77_INT (*dasrt_fcn_ptr) (const double&, const double *, const double *,
                                  double *, F77_INT&, double *, F77_INT *);

typedef F77_INT (*dasrt_jac_ptr) (const double&, const double *, const double *,
                                  double *, const double&, double *, F77_INT *);

typedef F77_INT (*dasrt_constr_ptr) (const F77_INT&, const double&,
                                     const double *, const F77_INT&,
                                     double *, double *, F77_INT *);

extern "C"
{
  F77_RET_T
  F77_FUNC (ddasrt, DDASRT) (dasrt_fcn_ptr, const F77_INT&, F77_DBLE&,
                             F77_DBLE *, F77_DBLE *, const F77_DBLE&, F77_INT *,
                             const F77_DBLE *, const F77_DBLE *, F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_INT *,
                             const F77_INT&, F77_DBLE *, F77_INT *,
                             dasrt_jac_ptr, dasrt_constr_ptr, const F77_INT&,
                             F77_INT *);
}

static DAEFunc::DAERHSFunc user_fsub;
static DAEFunc::DAEJacFunc user_jsub;
static DAERTFunc::DAERTConstrFunc user_csub;

static F77_INT nn;

static F77_INT
ddasrt_f (const double& t, const double *state, const double *deriv,
          double *delta, F77_INT& ires, double *, F77_INT *)
{
  ColumnVector tmp_state (nn);
  ColumnVector tmp_deriv (nn);

  for (F77_INT i = 0; i < nn; i++)
    {
      tmp_state(i) = state[i];
      tmp_deriv(i) = deriv[i];
    }

  octave_idx_type tmp_ires = ires;

  ColumnVector tmp_fval = (*user_fsub) (tmp_state, tmp_deriv, t, tmp_ires);

  ires = octave::to_f77_int (tmp_ires);

  if (tmp_fval.isempty ())
    ires = -2;
  else
    {
      for (F77_INT i = 0; i < nn; i++)
        delta[i] = tmp_fval(i);
    }

  return 0;
}

F77_INT
ddasrt_j (const double& time, const double *state, const double *deriv,
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

  Matrix tmp_pd = (*user_jsub) (tmp_state, tmp_deriv, time, cj);

  for (F77_INT j = 0; j < nn; j++)
    for (F77_INT i = 0; i < nn; i++)
      pd[nn * j + i] = tmp_pd.elem (i, j);

  return 0;
}

static F77_INT
ddasrt_g (const F77_INT& neq, const double& t, const double *state,
          const F77_INT& m_ng, double *gout, double *, F77_INT *)
{
  F77_INT n = neq;

  ColumnVector tmp_state (n);
  for (F77_INT i = 0; i < n; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_fval = (*user_csub) (tmp_state, t);

  for (F77_INT i = 0; i < m_ng; i++)
    gout[i] = tmp_fval(i);

  return 0;
}

void
DASRT::integrate (double tout)
{
  // I suppose this is the safe thing to do.  If this is the first
  // call, or if anything about the problem has changed, we should
  // start completely fresh.

  if (! m_initialized || m_restart
      || DAEFunc::m_reset || DAERTFunc::m_reset || DASRT_options::m_reset)
    {
      m_integration_error = false;

      m_initialized = true;

      m_info.resize (dim_vector (15, 1));

      for (F77_INT i = 0; i < 15; i++)
        m_info(i) = 0;

      F77_INT n = octave::to_f77_int (size ());

      nn = n;

      // DAERTFunc

      user_csub = DAERTFunc::constraint_function ();

      if (user_csub)
        {
          ColumnVector tmp = (*user_csub) (m_x, m_t);
          m_ng = octave::to_f77_int (tmp.numel ());
        }
      else
        m_ng = 0;

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
                ("dassl: invalid value for maximum order");
              m_integration_error = true;
              return;
            }
        }

      m_liw = 21 + n;
      m_lrw = 50 + 9*n + n*n + 3*m_ng;

      m_iwork.resize (dim_vector (m_liw, 1));
      m_rwork.resize (dim_vector (m_lrw, 1));

      m_info(0) = 0;

      if (m_stop_time_set)
        {
          m_info(3) = 1;
          m_rwork(0) = m_stop_time;
        }
      else
        m_info(3) = 0;

      m_restart = false;

      // DAEFunc

      user_fsub = DAEFunc::function ();
      user_jsub = DAEFunc::jacobian_function ();

      if (user_fsub)
        {
          octave_idx_type ires = 0;

          ColumnVector fval = (*user_fsub) (m_x, m_xdot, m_t, ires);

          if (fval.numel () != m_x.numel ())
            {
              (*current_liboctave_error_handler)
                ("dasrt: inconsistent sizes for state and residual vectors");

              m_integration_error = true;
              return;
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("dasrt: no user supplied RHS subroutine!");

          m_integration_error = true;
          return;
        }

      m_info(4) = (user_jsub ? 1 : 0);

      DAEFunc::m_reset = false;

      m_jroot.resize (dim_vector (m_ng, 1), 1);

      DAERTFunc::m_reset = false;

      // DASRT_options

      double mss = maximum_step_size ();
      if (mss >= 0.0)
        {
          m_rwork(1) = mss;
          m_info(6) = 1;
        }
      else
        m_info(6) = 0;

      double iss = initial_step_size ();
      if (iss >= 0.0)
        {
          m_rwork(2) = iss;
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

      m_abs_tol = absolute_tolerance ();
      m_rel_tol = relative_tolerance ();

      F77_INT abs_tol_len = octave::to_f77_int (m_abs_tol.numel ());
      F77_INT rel_tol_len = octave::to_f77_int (m_rel_tol.numel ());

      if (abs_tol_len == 1 && rel_tol_len == 1)
        {
          m_info.elem (1) = 0;
        }
      else if (abs_tol_len == n && rel_tol_len == n)
        {
          m_info.elem (1) = 1;
        }
      else
        {
          (*current_liboctave_error_handler)
            ("dasrt: inconsistent sizes for tolerance arrays");

          m_integration_error = true;
          return;
        }

      DASRT_options::m_reset = false;
    }

  double *px = m_x.fortran_vec ();
  double *pxdot = m_xdot.fortran_vec ();

  F77_INT *pinfo = m_info.fortran_vec ();

  double *prel_tol = m_rel_tol.fortran_vec ();
  double *pabs_tol = m_abs_tol.fortran_vec ();

  double *prwork = m_rwork.fortran_vec ();
  F77_INT *piwork = m_iwork.fortran_vec ();

  F77_INT *pjroot = m_jroot.fortran_vec ();

  double *dummy = nullptr;
  F77_INT *idummy = nullptr;

  F77_INT tmp_istate = octave::to_f77_int (m_istate);

  F77_XFCN (ddasrt, DDASRT, (ddasrt_f, nn, m_t, px, pxdot, tout, pinfo,
                             prel_tol, pabs_tol, tmp_istate, prwork, m_lrw,
                             piwork, m_liw, dummy, idummy, ddasrt_j,
                             ddasrt_g, m_ng, pjroot));

  m_istate = tmp_istate;

  switch (m_istate)
    {
    case 1: // A step was successfully taken in intermediate-output
            // mode.  The code has not yet reached TOUT.
    case 2: // The integration to TOUT was successfully completed
            // (T=TOUT) by stepping exactly to TOUT.
    case 3: // The integration to TOUT was successfully completed
            // (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
            // interpolation.  YPRIME(*) is obtained by interpolation.
      m_t = tout;
      break;

    case 4: //  The integration was successfully completed
            // by finding one or more roots of G at T.
      break;

    case -1: // A large amount of work has been expended.
    case -2: // The error tolerances are too stringent.
    case -3: // The local error test cannot be satisfied because you
             // specified a zero component in ATOL and the
             // corresponding computed solution component is zero.
             // Thus, a pure relative error test is impossible for
             // this component.
    case -6: // DDASRT had repeated error test failures on the last
             // attempted step.
    case -7: // The corrector could not converge.
    case -8: // The matrix of partial derivatives is singular.
    case -9: // The corrector could not converge.  There were repeated
             // error test failures in this step.
    case -10: // The corrector could not converge because IRES was
              // equal to minus one.
    case -11: // IRES equal to -2 was encountered and control is being
              // returned to the calling program.
    case -12: // DASSL failed to compute the initial YPRIME.
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
         "returned from ddasrt", m_istate);
      break;
    }
}

DASRT_result
DASRT::integrate (const ColumnVector& tout)
{
  DASRT_result retval;

  Matrix x_out;
  Matrix xdot_out;
  ColumnVector t_out = tout;

  octave_idx_type n_out = tout.numel ();
  F77_INT n = octave::to_f77_int (size ());

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (F77_INT i = 0; i < n; i++)
        {
          x_out(0, i) = m_x(i);
          xdot_out(0, i) = m_xdot(i);
        }

      for (octave_idx_type j = 1; j < n_out; j++)
        {
          integrate (tout(j));

          if (m_integration_error)
            {
              retval = DASRT_result (x_out, xdot_out, t_out);
              return retval;
            }

          if (m_istate == 4)
            t_out(j) = m_t;
          else
            t_out(j) = tout(j);

          for (F77_INT i = 0; i < n; i++)
            {
              x_out(j, i) = m_x(i);
              xdot_out(j, i) = m_xdot(i);
            }

          if (m_istate == 4)
            {
              x_out.resize (j+1, n);
              xdot_out.resize (j+1, n);
              t_out.resize (j+1);
              break;
            }
        }
    }

  retval = DASRT_result (x_out, xdot_out, t_out);

  return retval;
}

DASRT_result
DASRT::integrate (const ColumnVector& tout, const ColumnVector& tcrit)
{
  DASRT_result retval;

  Matrix x_out;
  Matrix xdot_out;
  ColumnVector t_outs = tout;

  octave_idx_type n_out = tout.numel ();
  F77_INT n = octave::to_f77_int (size ());

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);
      xdot_out.resize (n_out, n);

      octave_idx_type n_crit = tcrit.numel ();

      if (n_crit > 0)
        {
          octave_idx_type i_crit = 0;
          octave_idx_type i_out = 1;
          double next_crit = tcrit(0);
          double next_out;
          while (i_out < n_out)
            {
              bool do_restart = false;

              next_out = tout(i_out);
              if (i_crit < n_crit)
                next_crit = tcrit(i_crit);

              bool save_output = false;
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

              integrate (t_out);

              if (m_integration_error)
                {
                  retval = DASRT_result (x_out, xdot_out, t_outs);
                  return retval;
                }

              if (m_istate == 4)
                t_out = m_t;

              if (save_output)
                {
                  for (F77_INT i = 0; i < n; i++)
                    {
                      x_out(i_out-1, i) = m_x(i);
                      xdot_out(i_out-1, i) = m_xdot(i);
                    }

                  t_outs(i_out-1) = t_out;

                  if (m_istate == 4)
                    {
                      x_out.resize (i_out, n);
                      xdot_out.resize (i_out, n);
                      t_outs.resize (i_out);
                      i_out = n_out;
                    }
                }

              if (do_restart)
                force_restart ();
            }

          retval = DASRT_result (x_out, xdot_out, t_outs);
        }
      else
        {
          retval = integrate (tout);

          if (m_integration_error)
            return retval;
        }
    }

  return retval;
}

std::string
DASRT::error_message (void) const
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

    case 4:
      retval = "integration completed by finding one or more roots of G at T";
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
