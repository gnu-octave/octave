////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <list>
#include <string>

#include "DASPK.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "ovl.h"
#include "ov-fcn.h"
#include "ov-cell.h"
#include "pager.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "DASPK-opts.cc"

OCTAVE_BEGIN_NAMESPACE(octave)

// Global pointer for user defined function required by daspk.
static octave_value daspk_fcn;

// Global pointer for optional user defined jacobian function.
static octave_value daspk_jac;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
daspk_user_function (const ColumnVector& x, const ColumnVector& xdot,
                     double t, octave_idx_type& ires)
{
  ColumnVector retval;

  error_unless (x.numel () == xdot.numel ());

  octave_value_list args;

  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (daspk_fcn.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (daspk_fcn, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "daspk");
        }

      int tlen = tmp.length ();
      if (tlen == 0 || ! tmp(0).is_defined ())
        err_user_supplied_eval ("daspk");

      if (! warned_fcn_imaginary && tmp(0).iscomplex ())
        {
          warning ("daspk: ignoring imaginary part returned from user-supplied function");
          warned_fcn_imaginary = true;
        }

      retval = tmp(0).vector_value ();

      if (tlen > 1)
        ires = tmp(1).idx_type_value ();

      if (retval.isempty ())
        err_user_supplied_eval ("daspk");
    }

  return retval;
}

static Matrix
daspk_user_jacobian (const ColumnVector& x, const ColumnVector& xdot,
                     double t, double cj)
{
  Matrix retval;

  error_unless (x.numel () == xdot.numel ());

  octave_value_list args;

  args(3) = cj;
  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (daspk_jac.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (daspk_jac, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "daspk");
        }

      int tlen = tmp.length ();
      if (tlen == 0 || ! tmp(0).is_defined ())
        err_user_supplied_eval ("daspk");

      if (! warned_jac_imaginary && tmp(0).iscomplex ())
        {
          warning ("daspk: ignoring imaginary part returned from user-supplied jacobian function");
          warned_jac_imaginary = true;
        }

      retval = tmp(0).matrix_value ();

      if (retval.isempty ())
        err_user_supplied_eval ("daspk");
    }

  return retval;
}

DEFMETHOD (daspk, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{x}, @var{xdot}, @var{istate}, @var{msg}] =} daspk (@var{fcn}, @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})
Solve a set of differential-algebraic equations.

@code{daspk} solves the set of equations
@tex
$$ 0 = f (x, \dot{x}, t) $$
with
$$ x(t_0) = x_0, \dot{x}(t_0) = \dot{x}_0 $$
@end tex
@ifnottex

@example
0 = f (x, xdot, t)
@end example

@noindent
with

@example
x(t_0) = x_0, xdot(t_0) = xdot_0
@end example

@end ifnottex
The solution is returned in the matrices @var{x} and @var{xdot},
with each row in the result matrices corresponding to one of the
elements in the vector @var{t}.  The first element of @var{t}
should be @math{t_0} and correspond to the initial state of the
system @var{x_0} and its derivative @var{xdot_0}, so that the first
row of the output @var{x} is @var{x_0} and the first row
of the output @var{xdot} is @var{xdot_0}.

The first argument, @var{fcn}, is a string, inline, or function handle
that names the function @math{f} to call to compute the vector of
residuals for the set of equations.  It must have the form

@example
@var{res} = f (@var{x}, @var{xdot}, @var{t})
@end example

@noindent
in which @var{x}, @var{xdot}, and @var{res} are vectors, and @var{t} is a
scalar.

If @var{fcn} is a two-element string array or a two-element cell array
of strings, inline functions, or function handles, the first element names
the function @math{f} described above, and the second element names a
function to compute the modified Jacobian
@tex
$$
J = {\partial f \over \partial x}
  + c {\partial f \over \partial \dot{x}}
$$
@end tex
@ifnottex

@example
@group
      df       df
jac = -- + c ------
      dx     d xdot
@end group
@end example

@end ifnottex

The modified Jacobian function must have the form

@example
@group

@var{jac} = j (@var{x}, @var{xdot}, @var{t}, @var{c})

@end group
@end example

The second and third arguments to @code{daspk} specify the initial
condition of the states and their derivatives, and the fourth argument
specifies a vector of output times at which the solution is desired,
including the time corresponding to the initial condition.

The set of initial states and derivatives are not strictly required to
be consistent.  If they are not consistent, you must use the
@code{daspk_options} function to provide additional information so
that @code{daspk} can compute a consistent starting point.

The fifth argument is optional, and may be used to specify a set of
times that the DAE solver should not integrate past.  It is useful for
avoiding difficulties with singularities and points where there is a
discontinuity in the derivative.

After a successful computation, the value of @var{istate} will be
greater than zero (consistent with the Fortran version of @sc{daspk}).

If the computation is not successful, the value of @var{istate} will be
less than zero and @var{msg} will contain additional information.

You can use the function @code{daspk_options} to set optional
parameters for @code{daspk}.
@seealso{dassl}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 4 || nargin > 5)
    print_usage ();

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;

  octave_value_list retval (4);

  unwind_protect_var<int> restore_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    error ("daspk: invalid recursive call");

  std::string fcn_name, fname, jac_name, jname;

  daspk_fcn = octave_value ();
  daspk_jac = octave_value ();

  octave_value f_arg = args(0);

  std::list<std::string> fcn_param_names ({"x", "xdot", "t"});
  std::list<std::string> jac_param_names ({"x", "xdot", "t", "cj"});

  if (f_arg.iscell ())
    {
      Cell c = f_arg.cell_value ();
      if (c.numel () == 1)
        f_arg = c(0);
      else if (c.numel () == 2)
        {
          daspk_fcn = get_function_handle (interp, c(0), fcn_param_names);

          if (daspk_fcn.is_defined ())
            {
              daspk_jac = get_function_handle (interp, c(1), jac_param_names);

              if (daspk_jac.is_undefined ())
                daspk_fcn = octave_value ();
            }
        }
      else
        error ("daspk: incorrect number of elements in cell array");
    }

  if (daspk_fcn.is_undefined () && ! f_arg.iscell ())
    {
      if (f_arg.is_function_handle () || f_arg.is_inline_function ())
        daspk_fcn = f_arg;
      else
        {
          switch (f_arg.rows ())
            {
            case 1:
              daspk_fcn = get_function_handle (interp, f_arg, fcn_param_names);
              break;

            case 2:
              {
                string_vector tmp = f_arg.string_vector_value ();

                daspk_fcn = get_function_handle (interp, tmp(0),
                                                 fcn_param_names);

                if (daspk_fcn.is_defined ())
                  {
                    daspk_jac = get_function_handle (interp, tmp(1),
                                                     jac_param_names);

                    if (daspk_jac.is_undefined ())
                      daspk_fcn = octave_value ();
                  }
              }
              break;

            default:
              error ("daspk: first arg should be a string or 2-element string array");
            }
        }
    }

  if (daspk_fcn.is_undefined ())
    error ("daspk: FCN argument is not a valid function name or handle");

  ColumnVector state = args(1).xvector_value ("daspk: initial state X_0 must be a vector");

  ColumnVector deriv = args(2).xvector_value ("daspk: initial derivatives XDOT_0 must be a vector");

  ColumnVector out_times = args(3).xvector_value ("daspk: output time variable T must be a vector");

  ColumnVector crit_times;
  int crit_times_set = 0;
  if (nargin > 4)
    {
      crit_times = args(4).xvector_value ("daspk: list of critical times T_CRIT must be a vector");

      crit_times_set = 1;
    }

  if (state.numel () != deriv.numel ())
    error ("daspk: X_0 and XDOT_0 must have the same size");

  double tzero = out_times (0);

  DAEFunc fcn (daspk_user_function);
  if (daspk_jac.is_defined ())
    fcn.set_jacobian_function (daspk_user_jacobian);

  DASPK dae (state, deriv, tzero, fcn);
  dae.set_options (daspk_opts);

  Matrix output;
  Matrix deriv_output;

  if (crit_times_set)
    output = dae.integrate (out_times, deriv_output, crit_times);
  else
    output = dae.integrate (out_times, deriv_output);

  std::string msg = dae.error_message ();

  if (dae.integration_ok ())
    {
      retval(0) = output;
      retval(1) = deriv_output;
    }
  else
    {
      if (nargout < 3)
        error ("daspk: %s", msg.c_str ());

      retval(0) = Matrix ();
      retval(1) = Matrix ();
    }

  retval(2) = static_cast<double> (dae.integration_state ());
  retval(3) = msg;

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
