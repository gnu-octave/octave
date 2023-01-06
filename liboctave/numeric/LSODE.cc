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

#include "LSODE.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"

typedef F77_INT (*lsode_fcn_ptr) (const F77_INT&, const double&, double *,
                                  double *, F77_INT&);

typedef F77_INT (*lsode_jac_ptr) (const F77_INT&, const double&, double *,
                                  const F77_INT&, const F77_INT&, double *,
                                  const F77_INT&);

extern "C"
{
  F77_RET_T
  F77_FUNC (dlsode, DLSODE) (lsode_fcn_ptr, F77_INT&, F77_DBLE *, F77_DBLE&,
                             F77_DBLE&, F77_INT&, F77_DBLE&, const F77_DBLE *,
                             F77_INT&, F77_INT&, F77_INT&, F77_DBLE *,
                             F77_INT&, F77_INT *, F77_INT&, lsode_jac_ptr,
                             F77_INT&);
}

static ODEFunc::ODERHSFunc user_fcn;
static ODEFunc::ODEJacFunc user_jac;
static ColumnVector *tmp_x;
static bool user_jac_ignore_ml_mu;

static F77_INT
lsode_f (const F77_INT& neq, const double& time, double *, double *deriv,
         F77_INT& ierr)
{
  ColumnVector tmp_deriv;

  // NOTE: this won't work if LSODE passes copies of the state vector.
  //       In that case we have to create a temporary vector object
  //       and copy.

  tmp_deriv = (*user_fcn) (*tmp_x, time);

  if (tmp_deriv.isempty ())
    ierr = -1;
  else
    {
      for (F77_INT i = 0; i < neq; i++)
        deriv[i] = tmp_deriv.elem (i);
    }

  return 0;
}

static F77_INT
lsode_j (const F77_INT& neq, const double& time, double *,
         const F77_INT& ml, const F77_INT& mu,
         double *pd, const F77_INT& nrowpd)
{
  Matrix tmp_jac (neq, neq);

  // NOTE: this won't work if LSODE passes copies of the state vector.
  //       In that case we have to create a temporary vector object
  //       and copy.

  tmp_jac = (*user_jac) (*tmp_x, time);

  if (user_jac_ignore_ml_mu)
    for (F77_INT j = 0; j < neq; j++)
      for (F77_INT i = 0; i < neq; i++)
        pd[nrowpd * j + i] = tmp_jac (i, j);
  else
    // upper left ends of subdiagonals in tmp_jac
    for (F77_INT i = 0, j = mu; i <= ml; j == 0 ? i++ : j--)
      // walk down the subdiagonal in tmp_jac
      for (F77_INT k = i, l = j; k < neq && l < neq; k++, l++)
        pd[nrowpd * l + k + mu - l] = tmp_jac (k, l);

  return 0;
}

ColumnVector
LSODE::do_integrate (double tout)
{
  ColumnVector retval;

  static F77_INT nn = 0;

  if (! m_initialized || m_restart || ODEFunc::m_reset
      || LSODE_options::m_reset)
    {
      m_integration_error = false;

      m_initialized = true;

      m_istate = 1;

      F77_INT n = octave::to_f77_int (size ());

      nn = n;

      octave_idx_type max_maxord = 0;

      user_jac_ignore_ml_mu = true;

      m_iwork = Array<octave_f77_int_type> (dim_vector (2, 1));

      m_iwork(0) = lower_jacobian_subdiagonals ();  // 'ML' in dlsode.f

      m_iwork(1) = upper_jacobian_subdiagonals ();  // 'MU' in dlsode.f

      if (integration_method () == "stiff")
        {
          max_maxord = 5;

          if (m_jac)
            {
              if (jacobian_type () == "banded")
                {
                  m_method_flag = 24;
                  user_jac_ignore_ml_mu = false;
                }
              else
                m_method_flag = 21;
            }
          else
            {
              if (jacobian_type () == "full")
                m_method_flag = 22;
              else if (jacobian_type () == "banded")
                m_method_flag = 25;
              else if (jacobian_type () == "diagonal")
                m_method_flag = 23;
              else
                {
                  // should be prevented by lsode_options
                  (*current_liboctave_error_handler)
                    ("lsode: internal error, wrong jacobian type");
                  m_integration_error = true;
                  return retval;
                }
            }

          m_liw = 20 + n;
          m_lrw = 22 + n * (9 + n);
        }
      else
        {
          max_maxord = 12;

          m_method_flag = 10;

          m_liw = 20;
          m_lrw = 22 + 16 * n;
        }

      m_iwork.resize (dim_vector (m_liw, 1));

      for (F77_INT i = 4; i < 9; i++)
        m_iwork(i) = 0;

      m_rwork.resize (dim_vector (m_lrw, 1));

      for (F77_INT i = 4; i < 9; i++)
        m_rwork(i) = 0;

      octave_idx_type maxord = maximum_order ();

      if (maxord >= 0)
        {
          if (maxord > 0 && maxord <= max_maxord)
            {
              m_iwork(4) = octave::to_f77_int (maxord);
              m_iopt = 1;
            }
          else
            {
              // FIXME: Should this be a warning?
              (*current_liboctave_error_handler)
                ("lsode: invalid value for maximum order");
              m_integration_error = true;
              return retval;
            }
        }

      if (m_stop_time_set)
        {
          m_itask = 4;
          m_rwork(0) = m_stop_time;
          m_iopt = 1;
        }
      else
        {
          m_itask = 1;
        }

      m_restart = false;

      // ODEFunc

      // NOTE: this won't work if LSODE passes copies of the state vector.
      //       In that case we have to create a temporary vector object
      //       and copy.

      tmp_x = &m_x;

      user_fcn = function ();
      user_jac = jacobian_function ();

      ColumnVector m_xdot = (*user_fcn) (m_x, m_t);

      if (m_x.numel () != m_xdot.numel ())
        {
          // FIXME: Should this be a warning?
          (*current_liboctave_error_handler)
            ("lsode: inconsistent sizes for state and derivative vectors");

          m_integration_error = true;
          return retval;
        }

      ODEFunc::m_reset = false;

      // LSODE_options

      m_rel_tol = relative_tolerance ();
      m_abs_tol = absolute_tolerance ();

      F77_INT abs_tol_len = octave::to_f77_int (m_abs_tol.numel ());

      if (abs_tol_len == 1)
        m_itol = 1;
      else if (abs_tol_len == n)
        m_itol = 2;
      else
        {
          // FIXME: Should this be a warning?
          (*current_liboctave_error_handler)
            ("lsode: inconsistent sizes for state and absolute tolerance vectors");

          m_integration_error = true;
          return retval;
        }

      double iss = initial_step_size ();
      if (iss >= 0.0)
        {
          m_rwork(4) = iss;
          m_iopt = 1;
        }

      double maxss = maximum_step_size ();
      if (maxss >= 0.0)
        {
          m_rwork(5) = maxss;
          m_iopt = 1;
        }

      double minss = minimum_step_size ();
      if (minss >= 0.0)
        {
          m_rwork(6) = minss;
          m_iopt = 1;
        }

      F77_INT sl = octave::to_f77_int (step_limit ());
      if (sl > 0)
        {
          m_iwork(5) = sl;
          m_iopt = 1;
        }

      LSODE_options::m_reset = false;
    }

  double *px = m_x.fortran_vec ();

  double *pabs_tol = m_abs_tol.fortran_vec ();

  F77_INT *piwork = m_iwork.fortran_vec ();
  double *prwork = m_rwork.fortran_vec ();

  F77_INT tmp_istate = octave::to_f77_int (m_istate);

  F77_XFCN (dlsode, DLSODE, (lsode_f, nn, px, m_t, tout, m_itol, m_rel_tol,
                             pabs_tol, m_itask, tmp_istate, m_iopt, prwork,
                             m_lrw, piwork, m_liw, lsode_j, m_method_flag));

  m_istate = tmp_istate;

  switch (m_istate)
    {
    case 1:  // prior to initial integration step.
    case 2:  // lsode was successful.
      retval = m_x;
      m_t = tout;
      break;

    case -1:  // excess work done on this call (perhaps wrong mf).
    case -2:  // excess accuracy requested (tolerances too small).
    case -3:  // invalid input detected (see printed message).
    case -4:  // repeated error test failures (check all inputs).
    case -5:  // repeated convergence failures (perhaps bad Jacobian
              // supplied or wrong choice of mf or tolerances).
    case -6:  // error weight became zero during problem. (solution
              // component i vanished, and atol or atol(i) = 0.)
    case -13: // return requested in user-supplied function.
      m_integration_error = true;
      break;

    default:
      m_integration_error = true;
      (*current_liboctave_error_handler)
        ("unrecognized value of istate (= %" OCTAVE_IDX_TYPE_FORMAT ") "
         "returned from lsode", m_istate);
      break;
    }

  return retval;
}

std::string
LSODE::error_message (void) const
{
  std::string retval;

  std::ostringstream buf;
  buf << m_t;
  std::string t_curr = buf.str ();

  switch (m_istate)
    {
    case 1:
      retval = "prior to initial integration step";
      break;

    case 2:
      retval = "successful exit";
      break;

    case 3:
      retval = "prior to continuation call with modified parameters";
      break;

    case -1:
      retval = "excess work on this call (t = " + t_curr +
               "; perhaps wrong integration method)";
      break;

    case -2:
      retval = "excess accuracy requested (tolerances too small)";
      break;

    case -3:
      retval = "invalid input detected (see printed message)";
      break;

    case -4:
      retval = "repeated error test failures (t = " + t_curr +
               "; check all inputs)";
      break;

    case -5:
      retval = "repeated convergence failures (t = " + t_curr +
               "; perhaps bad Jacobian supplied or wrong choice of integration method or tolerances)";
      break;

    case -6:
      retval = "error weight became zero during problem. (t = " + t_curr +
               "; solution component i vanished, and atol or atol(i) == 0)";
      break;

    case -13:
      retval = "return requested in user-supplied function (t = "
               + t_curr + ')';
      break;

    default:
      retval = "unknown error state";
      break;
    }

  return retval;
}

Matrix
LSODE::do_integrate (const ColumnVector& tout)
{
  Matrix retval;

  octave_idx_type n_out = tout.numel ();
  F77_INT n = octave::to_f77_int (size ());

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);

      for (F77_INT i = 0; i < n; i++)
        retval.elem (0, i) = m_x.elem (i);

      for (octave_idx_type j = 1; j < n_out; j++)
        {
          ColumnVector x_next = do_integrate (tout.elem (j));

          if (m_integration_error)
            return retval;

          for (F77_INT i = 0; i < n; i++)
            retval.elem (j, i) = x_next.elem (i);
        }
    }

  return retval;
}

Matrix
LSODE::do_integrate (const ColumnVector& tout, const ColumnVector& tcrit)
{
  Matrix retval;

  octave_idx_type n_out = tout.numel ();
  F77_INT n = octave::to_f77_int (size ());

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);

      for (F77_INT i = 0; i < n; i++)
        retval.elem (0, i) = m_x.elem (i);

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

              ColumnVector x_next = do_integrate (t_out);

              if (m_integration_error)
                return retval;

              if (save_output)
                {
                  for (F77_INT i = 0; i < n; i++)
                    retval.elem (i_out-1, i) = x_next.elem (i);
                }

              if (do_restart)
                force_restart ();
            }
        }
      else
        {
          retval = do_integrate (tout);

          if (m_integration_error)
            return retval;
        }
    }

  return retval;
}
