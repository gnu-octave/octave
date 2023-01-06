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

#include <list>
#include <string>

#include "DASRT.h"
#include "lo-mappers.h"

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

#include "DASRT-opts.cc"

OCTAVE_BEGIN_NAMESPACE(octave)

// Global pointers for user defined function required by dasrt.
static octave_value dasrt_fcn;
static octave_value dasrt_jac;
static octave_value dasrt_cf;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;
static bool warned_cf_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
dasrt_user_f (const ColumnVector& x, const ColumnVector& xdot,
              double t, octave_idx_type&)
{
  ColumnVector retval;

  error_unless (x.numel () == xdot.numel ());

  octave_value_list args;

  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (dasrt_fcn.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (dasrt_fcn, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "dasrt");
        }

      if (tmp.empty () || ! tmp(0).is_defined ())
        err_user_supplied_eval ("dasrt");

      if (! warned_fcn_imaginary && tmp(0).iscomplex ())
        {
          warning ("dasrt: ignoring imaginary part returned from user-supplied function");
          warned_fcn_imaginary = true;
        }

      retval = tmp(0).vector_value ();

      if (retval.isempty ())
        err_user_supplied_eval ("dasrt");
    }

  return retval;
}

static ColumnVector
dasrt_user_cf (const ColumnVector& x, double t)
{
  ColumnVector retval;

  octave_value_list args;

  args(1) = t;
  args(0) = x;

  if (dasrt_cf.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (dasrt_cf, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "dasrt");
        }

      if (tmp.empty () || ! tmp(0).is_defined ())
        err_user_supplied_eval ("dasrt");

      if (! warned_cf_imaginary && tmp(0).iscomplex ())
        {
          warning ("dasrt: ignoring imaginary part returned from user-supplied constraint function");
          warned_cf_imaginary = true;
        }

      retval = tmp(0).vector_value ();

      if (retval.isempty ())
        err_user_supplied_eval ("dasrt");
    }

  return retval;
}

static Matrix
dasrt_user_j (const ColumnVector& x, const ColumnVector& xdot,
              double t, double cj)
{
  Matrix retval;

  error_unless (x.numel () == xdot.numel ());

  octave_value_list args;

  args(3) = cj;
  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (dasrt_jac.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (dasrt_jac, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "dasrt");
        }

      int tlen = tmp.length ();
      if (tlen == 0 || ! tmp(0).is_defined ())
        err_user_supplied_eval ("dasrt");

      if (! warned_jac_imaginary && tmp(0).iscomplex ())
        {
          warning ("dasrt: ignoring imaginary part returned from user-supplied jacobian function");
          warned_jac_imaginary = true;
        }

      retval = tmp(0).matrix_value ();

      if (retval.isempty ())
        err_user_supplied_eval ("dasrt");
    }

  return retval;
}

DEFMETHOD (dasrt, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{x}, @var{xdot}, @var{t_out}, @var{istat}, @var{msg}] =} dasrt (@var{fcn}, @var{g}, @var{x_0}, @var{xdot_0}, @var{t})
@deftypefnx {} {@dots{} =} dasrt (@var{fcn}, @var{g}, @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})
@deftypefnx {} {@dots{} =} dasrt (@var{fcn}, @var{x_0}, @var{xdot_0}, @var{t})
@deftypefnx {} {@dots{} =} dasrt (@var{fcn}, @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})
Solve a set of differential-algebraic equations.

@code{dasrt} solves the set of equations
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
with functional stopping criteria (root solving).

The solution is returned in the matrices @var{x} and @var{xdot},
with each row in the result matrices corresponding to one of the
elements in the vector @var{t_out}.  The first element of @var{t}
should be @math{t_0} and correspond to the initial state of the
system @var{x_0} and its derivative @var{xdot_0}, so that the first
row of the output @var{x} is @var{x_0} and the first row
of the output @var{xdot} is @var{xdot_0}.

The vector @var{t} provides an upper limit on the length of the
integration.  If the stopping condition is met, the vector
@var{t_out} will be shorter than @var{t}, and the final element of
@var{t_out} will be the point at which the stopping condition was met,
and may not correspond to any element of the vector @var{t}.

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

The optional second argument names a function that defines the
constraint functions whose roots are desired during the integration.
This function must have the form

@example
@var{g_out} = g (@var{x}, @var{t})
@end example

@noindent
and return a vector of the constraint function values.
If the value of any of the constraint functions changes sign, @sc{dasrt}
will attempt to stop the integration at the point of the sign change.

If the name of the constraint function is omitted, @code{dasrt} solves
the same problem as @code{daspk} or @code{dassl}.

Note that because of numerical errors in the constraint functions
due to round-off and integration error, @sc{dasrt} may return false
roots, or return the same root at two or more nearly equal values of
@var{T}.  If such false roots are suspected, the user should consider
smaller error tolerances or higher precision in the evaluation of the
constraint functions.

If a root of some constraint function defines the end of the problem,
the input to @sc{dasrt} should nevertheless allow integration to a
point slightly past that root, so that @sc{dasrt} can locate the root
by interpolation.

The third and fourth arguments to @code{dasrt} specify the initial
condition of the states and their derivatives, and the fourth argument
specifies a vector of output times at which the solution is desired,
including the time corresponding to the initial condition.

The set of initial states and derivatives are not strictly required to
be consistent.  In practice, however, @sc{dassl} is not very good at
determining a consistent set for you, so it is best if you ensure that
the initial values result in the function evaluating to zero.

The sixth argument is optional, and may be used to specify a set of
times that the DAE solver should not integrate past.  It is useful for
avoiding difficulties with singularities and points where there is a
discontinuity in the derivative.

After a successful computation, the value of @var{istate} will be
greater than zero (consistent with the Fortran version of @sc{dassl}).

If the computation is not successful, the value of @var{istate} will be
less than zero and @var{msg} will contain additional information.

You can use the function @code{dasrt_options} to set optional
parameters for @code{dasrt}.
@seealso{dasrt_options, daspk, dasrt, lsode}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 4 || nargin > 6)
    print_usage ();

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;
  warned_cf_imaginary = false;

  octave_value_list retval (5);

  unwind_protect_var<int> restore_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    error ("dasrt: invalid recursive call");

  int argp = 0;
  std::string fcn_name, fname, jac_name, jname;

  dasrt_fcn = octave_value ();
  dasrt_jac = octave_value ();
  dasrt_cf = octave_value ();

  // Check all the arguments.  Are they the right animals?

  // Here's where I take care of f and j in one shot:

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
          dasrt_fcn = get_function_handle (interp, c(0), fcn_param_names);

          if (dasrt_fcn.is_defined ())
            {
              dasrt_jac = get_function_handle (interp, c(1), jac_param_names);

              if (dasrt_jac.is_undefined ())
                dasrt_fcn = octave_value ();
            }
        }
      else
        error ("dasrt: incorrect number of elements in cell array");
    }

  if (dasrt_fcn.is_undefined () && ! f_arg.iscell ())
    {
      if (f_arg.is_function_handle () || f_arg.is_inline_function ())
        dasrt_fcn = f_arg;
      else
        {
          switch (f_arg.rows ())
            {
            case 1:
              dasrt_fcn = get_function_handle (interp, f_arg, fcn_param_names);
              break;

            case 2:
              {
                string_vector tmp = f_arg.string_vector_value ();

                dasrt_fcn = get_function_handle (interp, tmp(0),
                                                 fcn_param_names);

                if (dasrt_fcn.is_defined ())
                  {
                    dasrt_jac = get_function_handle (interp, tmp(1),
                                                     jac_param_names);

                    if (dasrt_jac.is_undefined ())
                      dasrt_fcn = octave_value ();
                  }
              }
              break;

            default:
              error ("dasrt: first arg should be a string or 2-element string array");
            }
        }
    }

  if (dasrt_fcn.is_undefined ())
    error ("dasrt: FCN argument is not a valid function name or handle");

  DAERTFunc fcn (dasrt_user_f);

  argp++;

  if (args(1).isempty () && args(1).is_double_type ())
    {
      // Allow [] to skip constraint function.  This feature is
      // undocumented now, but was supported by earlier versions.

      argp++;
    }
  else
    {
      if (args(1).is_function_handle () || args(1).is_inline_function ()
          || args(1).is_string ())
        {
          std::list<std::string> cf_param_names ({"x", "t"});

          dasrt_cf = get_function_handle (interp, args(1), cf_param_names);
        }

      if (dasrt_cf.is_defined ())
        {
          argp++;

          fcn.set_constraint_function (dasrt_user_cf);
        }
    }

  if (argp + 3 > nargin)
    print_usage ();

  ColumnVector state = args(argp++).xvector_value ("dasrt: initial state X_0 must be a vector");

  ColumnVector stateprime = args(
                              argp++).xvector_value ("dasrt: initial derivatives XDOT_0 must be a vector");

  ColumnVector out_times = args(
                             argp++).xvector_value ("dasrt: output time variable T must be a vector");

  double tzero = out_times (0);

  ColumnVector crit_times;

  bool crit_times_set = false;

  if (argp < nargin)
    {
      crit_times = args(argp).xvector_value ("dasrt: list of critical times T_CRIT must be a vector");
      argp++;

      crit_times_set = true;
    }

  if (dasrt_jac.is_defined ())
    fcn.set_jacobian_function (dasrt_user_j);

  DASRT_result output;

  DASRT dae = DASRT (state, stateprime, tzero, fcn);

  dae.set_options (dasrt_opts);

  if (crit_times_set)
    output = dae.integrate (out_times, crit_times);
  else
    output = dae.integrate (out_times);

  std::string msg = dae.error_message ();

  if (dae.integration_ok ())
    {
      retval(0) = output.state ();
      retval(1) = output.deriv ();
      retval(2) = output.times ();
    }
  else
    {
      if (nargout < 4)
        error ("dasrt: %s", msg.c_str ());

      retval(0) = Matrix ();
      retval(1) = Matrix ();
      retval(2) = Matrix ();
    }

  retval(3) = static_cast<double> (dae.integration_state ());
  retval(4) = msg;

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
