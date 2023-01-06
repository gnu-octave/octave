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

#include "LSODE.h"
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
#include "pr-output.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "LSODE-opts.cc"

OCTAVE_BEGIN_NAMESPACE(octave)

// Global pointer for user defined function required by lsode.
static octave_value lsode_fcn;

// Global pointer for optional user defined jacobian function used by lsode.
static octave_value lsode_jac;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
lsode_user_function (const ColumnVector& x, double t)
{
  ColumnVector retval;

  octave_value_list args;
  args(1) = t;
  args(0) = x;

  if (lsode_fcn.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = octave::feval (lsode_fcn, args, 1);
        }
      catch (octave::execution_exception& ee)
        {
          err_user_supplied_eval (ee, "lsode");
        }

      if (tmp.empty () || ! tmp(0).is_defined ())
        err_user_supplied_eval ("lsode");

      if (! warned_fcn_imaginary && tmp(0).iscomplex ())
        {
          warning ("lsode: ignoring imaginary part returned from user-supplied function");
          warned_fcn_imaginary = true;
        }

      retval = tmp(0).xvector_value ("lsode: expecting user supplied function to return numeric vector");

      if (retval.isempty ())
        err_user_supplied_eval ("lsode");
    }

  return retval;
}

static Matrix
lsode_user_jacobian (const ColumnVector& x, double t)
{
  Matrix retval;

  octave_value_list args;
  args(1) = t;
  args(0) = x;

  if (lsode_jac.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = octave::feval (lsode_jac, args, 1);
        }
      catch (octave::execution_exception& ee)
        {
          err_user_supplied_eval (ee, "lsode");
        }

      if (tmp.empty () || ! tmp(0).is_defined ())
        err_user_supplied_eval ("lsode");

      if (! warned_jac_imaginary && tmp(0).iscomplex ())
        {
          warning ("lsode: ignoring imaginary part returned from user-supplied jacobian function");
          warned_jac_imaginary = true;
        }

      retval = tmp(
                 0).xmatrix_value ("lsode: expecting user supplied jacobian function to return numeric array");

      if (retval.isempty ())
        err_user_supplied_eval ("lsode");
    }

  return retval;
}

DEFMETHOD (lsode, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{x}, @var{istate}, @var{msg}] =} lsode (@var{fcn}, @var{x_0}, @var{t})
@deftypefnx {} {[@var{x}, @var{istate}, @var{msg}] =} lsode (@var{fcn}, @var{x_0}, @var{t}, @var{t_crit})
Ordinary Differential Equation (ODE) solver.

The set of differential equations to solve is
@tex
$$ {dx \over dt} = f (x, t) $$
with
$$ x(t_0) = x_0 $$
@end tex
@ifnottex

@example
@group
dx
-- = f (x, t)
dt
@end group
@end example

@noindent
with

@example
x(t_0) = x_0
@end example

@end ifnottex
The solution is returned in the matrix @var{x}, with each row
corresponding to an element of the vector @var{t}.  The first element
of @var{t} should be @math{t_0} and should correspond to the initial
state of the system @var{x_0}, so that the first row of the output
is @var{x_0}.

The first argument, @var{fcn}, is a string, inline, or function handle
that names the function @math{f} to call to compute the vector of right
hand sides for the set of equations.  The function must have the form

@example
@var{xdot} = f (@var{x}, @var{t})
@end example

@noindent
in which @var{xdot} and @var{x} are vectors and @var{t} is a scalar.

If @var{fcn} is a two-element string array or a two-element cell array
of strings, inline functions, or function handles, the first element names
the function @math{f} described above, and the second element names a
function to compute the Jacobian of @math{f}.  The Jacobian function
must have the form

@example
@var{jac} = j (@var{x}, @var{t})
@end example

@noindent
in which @var{jac} is the matrix of partial derivatives
@tex
$$ J = {\partial f_i \over \partial x_j} = \left[\matrix{
{\partial f_1 \over \partial x_1}
  & {\partial f_1 \over \partial x_2}
  & \cdots
  & {\partial f_1 \over \partial x_N} \cr
{\partial f_2 \over \partial x_1}
  & {\partial f_2 \over \partial x_2}
  & \cdots
  & {\partial f_2 \over \partial x_N} \cr
 \vdots & \vdots & \ddots & \vdots \cr
{\partial f_M \over \partial x_1}
  & {\partial f_M \over \partial x_2}
  & \cdots
  & {\partial f_M \over \partial x_N} \cr}\right]$$
@end tex
@ifnottex

@example
@group
             | df_1  df_1       df_1 |
             | ----  ----  ...  ---- |
             | dx_1  dx_2       dx_N |
             |                       |
             | df_2  df_2       df_2 |
             | ----  ----  ...  ---- |
      df_i   | dx_1  dx_2       dx_N |
jac = ---- = |                       |
      dx_j   |  .    .     .    .    |
             |  .    .      .   .    |
             |  .    .       .  .    |
             |                       |
             | df_M  df_M       df_M |
             | ----  ----  ...  ---- |
             | dx_1  dx_2       dx_N |
@end group
@end example

@end ifnottex

The second argument specifies the initial state of the system @math{x_0}.  The
third argument is a vector, @var{t}, specifying the time values for which a
solution is sought.

The fourth argument is optional, and may be used to specify a set of
times that the ODE solver should not integrate past.  It is useful for
avoiding difficulties with singularities and points where there is a
discontinuity in the derivative.

After a successful computation, the value of @var{istate} will be 2
(consistent with the Fortran version of @sc{lsode}).

If the computation is not successful, @var{istate} will be something
other than 2 and @var{msg} will contain additional information.

You can use the function @code{lsode_options} to set optional
parameters for @code{lsode}.

See @nospell{Alan C. Hindmarsh},
@cite{ODEPACK, A Systematized Collection of ODE Solvers},
in Scientific Computing, @nospell{R. S. Stepleman}, editor, (1983)
or @url{https://computing.llnl.gov/projects/odepack}
for more information about the inner workings of @code{lsode}.

Example: Solve the @nospell{Van der Pol} equation

@example
@group
fvdp = @@(@var{y},@var{t}) [@var{y}(2); (1 - @var{y}(1)^2) * @var{y}(2) - @var{y}(1)];
@var{t} = linspace (0, 20, 100);
@var{y} = lsode (fvdp, [2; 0], @var{t});
@end group
@end example
@seealso{daspk, dassl, dasrt}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 3 || nargin > 4)
    print_usage ();

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;

  unwind_protect_var<int> restore_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    error ("lsode: invalid recursive call");

  symbol_table& symtab = interp.get_symbol_table ();

  std::string fcn_name, fname, jac_name, jname;

  lsode_fcn = octave_value ();
  lsode_jac = octave_value ();

  octave_value f_arg = args(0);

  std::list<std::string> parameter_names ({"x", "t"});

  if (f_arg.iscell ())
    {
      Cell c = f_arg.cell_value ();
      if (c.numel () == 1)
        f_arg = c(0);
      else if (c.numel () == 2)
        {
          lsode_fcn = get_function_handle (interp, c(0), parameter_names);

          if (lsode_fcn.is_defined ())
            {
              lsode_jac = get_function_handle (interp, c(1), parameter_names);

              if (lsode_jac.is_undefined ())
                lsode_fcn = octave_value ();
            }
        }
      else
        error ("lsode: incorrect number of elements in cell array");
    }

  if (lsode_fcn.is_undefined () && ! f_arg.iscell ())
    {
      if (f_arg.is_function_handle () || f_arg.is_inline_function ())
        lsode_fcn = f_arg;
      else
        {
          switch (f_arg.rows ())
            {
            case 1:
              lsode_fcn = get_function_handle (interp, f_arg, parameter_names);
              break;

            case 2:
              {
                string_vector tmp = f_arg.string_vector_value ();

                lsode_fcn = get_function_handle (interp, tmp(0),
                                                 parameter_names);

                if (lsode_fcn.is_defined ())
                  {
                    lsode_jac = get_function_handle (interp, tmp(1),
                                                     parameter_names);

                    if (lsode_jac.is_undefined ())
                      lsode_fcn = octave_value ();
                  }
              }
              break;

            default:
              error ("lsode: first arg should be a string or 2-element string array");
            }
        }
    }

  if (lsode_fcn.is_undefined ())
    error ("lsode: FCN argument is not a valid function name or handle");

  ColumnVector state = args(1).xvector_value ("lsode: initial state X_0 must be a vector");
  ColumnVector out_times = args(2).xvector_value ("lsode: output time variable T must be a vector");

  ColumnVector crit_times;

  int crit_times_set = 0;
  if (nargin > 3)
    {
      crit_times = args(3).xvector_value ("lsode: list of critical times T_CRIT must be a vector");

      crit_times_set = 1;
    }

  double tzero = out_times (0);

  ODEFunc fcn (lsode_user_function);

  if (lsode_jac.is_defined ())
    fcn.set_jacobian_function (lsode_user_jacobian);

  LSODE ode (state, tzero, fcn);

  ode.set_options (lsode_opts);

  Matrix output;
  if (crit_times_set)
    output = ode.integrate (out_times, crit_times);
  else
    output = ode.integrate (out_times);

  if (fcn_name.length ())
    symtab.clear_function (fcn_name);
  if (jac_name.length ())
    symtab.clear_function (jac_name);

  std::string msg = ode.error_message ();

  octave_value_list retval (3);

  if (ode.integration_ok ())
    retval(0) = output;
  else if (nargout < 2)
    error ("lsode: %s", msg.c_str ());
  else
    retval(0) = Matrix ();

  retval(1) = static_cast<double> (ode.integration_state ());
  retval(2) = msg;

  return retval;
}

/*

## dassl-1.m
##
## Test lsode() function
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         20 May 1998
##
## Problem
##
##    y1' = -y2,   y1(0) = 1
##    y2' =  y1,   y2(0) = 0
##
## Solution
##
##    y1(t) = cos(t)
##    y2(t) = sin(t)
##
%!function xdot = __f (x, t)
%!  xdot = [-x(2); x(1)];
%!endfunction
%!test
%!
%! x0 = [1; 0];
%! xdot0 = [0; 1];
%! t = (0:1:10)';
%!
%! tol = 500 * lsode_options ("relative tolerance");
%!
%! x = lsode ("__f", x0, t);
%!
%! y = [cos(t), sin(t)];
%!
%! assert (x, y, tol);

%!function xdotdot = __f (x, t)
%!  xdotdot = [x(2); -x(1)];
%!endfunction
%!test
%!
%! x0 = [1; 0];
%! t = [0; 2*pi];
%! tol = 100 * dassl_options ("relative tolerance");
%!
%! x = lsode ("__f", x0, t);
%!
%! y = [1, 0; 1, 0];
%!
%! assert (x, y, tol);

%!function xdot = __f (x, t)
%!  xdot = x;
%!endfunction
%!test
%!
%! x0 = 1;
%! t = [0; 1];
%! tol = 100 * dassl_options ("relative tolerance");
%!
%! x = lsode ("__f", x0, t);
%!
%! y = [1; e];
%!
%! assert (x, y, tol);

%!test
%! lsode_options ("absolute tolerance", eps);
%! assert (lsode_options ("absolute tolerance") == eps);

%!error lsode_options ("foo", 1, 2)
*/

OCTAVE_END_NAMESPACE(octave)
