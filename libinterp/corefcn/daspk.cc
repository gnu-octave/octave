/*

Copyright (C) 1996-2015 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include <iomanip>
#include <iostream>

#include "DASPK.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "ov-fcn.h"
#include "ov-cell.h"
#include "pager.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "DASPK-opts.cc"

// Global pointer for user defined function required by daspk.
static octave_function *daspk_fcn;

// Global pointer for optional user defined jacobian function.
static octave_function *daspk_jac;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

ColumnVector
daspk_user_function (const ColumnVector& x, const ColumnVector& xdot,
                     double t, octave_idx_type& ires)
{
  ColumnVector retval;

  assert (x.numel () == xdot.numel ());

  octave_value_list args;

  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (daspk_fcn)
    {
      octave_value_list tmp;

      try
        {
          tmp = daspk_fcn->do_multi_index_op (1, args);
        }
      catch (octave_execution_exception& e)
        {
          err_user_supplied_eval (e, "daspk");
        }

      int tlen = tmp.length ();
      if (tlen == 0 || ! tmp(0).is_defined ())
        err_user_supplied_eval ("daspk");

      if (! warned_fcn_imaginary && tmp(0).is_complex_type ())
        {
          warning ("daspk: ignoring imaginary part returned from user-supplied function");
          warned_fcn_imaginary = true;
        }

      retval = tmp(0).vector_value ();

      if (tlen > 1)
        ires = tmp(1).idx_type_value ();

      if (retval.is_empty ())
        err_user_supplied_eval ("daspk");
    }

  return retval;
}

Matrix
daspk_user_jacobian (const ColumnVector& x, const ColumnVector& xdot,
                     double t, double cj)
{
  Matrix retval;

  assert (x.numel () == xdot.numel ());

  octave_value_list args;

  args(3) = cj;
  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (daspk_jac)
    {
      octave_value_list tmp;

      try
        {
          tmp = daspk_jac->do_multi_index_op (1, args);
        }
      catch (octave_execution_exception& e)
        {
          err_user_supplied_eval (e, "daspk");
        }

      int tlen = tmp.length ();
      if (tlen == 0 || ! tmp(0).is_defined ())
        err_user_supplied_eval ("daspk");

      if (! warned_jac_imaginary && tmp(0).is_complex_type ())
        {
          warning ("daspk: ignoring imaginary part returned from user-supplied jacobian function");
          warned_jac_imaginary = true;
        }

      retval = tmp(0).matrix_value ();

      if (retval.is_empty ())
        err_user_supplied_eval ("daspk");
    }

  return retval;
}

DEFUN (daspk, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{x}, @var{xdot}, @var{istate}, @var{msg}] =} daspk (@var{fcn}, @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})
Solve the set of differential-algebraic equations
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

  octave::unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    error ("daspk: invalid recursive call");

  std::string fcn_name, fname, jac_name, jname;
  daspk_fcn = 0;
  daspk_jac = 0;

  octave_value f_arg = args(0);

  if (f_arg.is_cell ())
    {
      Cell c = f_arg.cell_value ();
      if (c.numel () == 1)
        f_arg = c(0);
      else if (c.numel () == 2)
        {
          if (c(0).is_function_handle () || c(0).is_inline_function ())
            daspk_fcn = c(0).function_value ();
          else
            {
              fcn_name = unique_symbol_name ("__daspk_fcn__");
              fname = "function y = ";
              fname.append (fcn_name);
              fname.append (" (x, xdot, t) y = ");
              daspk_fcn = extract_function
                (c(0), "daspk", fcn_name, fname, "; endfunction");
            }

          if (daspk_fcn)
            {
              if (c(1).is_function_handle () || c(1).is_inline_function ())
                daspk_jac = c(1).function_value ();
              else
                {
                  jac_name = unique_symbol_name ("__daspk_jac__");
                  jname = "function jac = ";
                  jname.append (jac_name);
                  jname.append (" (x, xdot, t, cj) jac = ");
                  daspk_jac = extract_function (c(1), "daspk", jac_name,
                                                jname, "; endfunction");

                  if (! daspk_jac)
                    {
                      if (fcn_name.length ())
                        clear_function (fcn_name);
                      daspk_fcn = 0;
                    }
                }
            }
        }
      else
        error ("daspk: incorrect number of elements in cell array");
    }

  if (! daspk_fcn && ! f_arg.is_cell ())
    {
      if (f_arg.is_function_handle () || f_arg.is_inline_function ())
        daspk_fcn = f_arg.function_value ();
      else
        {
          switch (f_arg.rows ())
            {
            case 1:
              do
                {
                  fcn_name = unique_symbol_name ("__daspk_fcn__");
                  fname = "function y = ";
                  fname.append (fcn_name);
                  fname.append (" (x, xdot, t) y = ");
                  daspk_fcn = extract_function (f_arg, "daspk", fcn_name,
                                                fname, "; endfunction");
                }
              while (0);
              break;

            case 2:
              {
                string_vector tmp = f_arg.string_vector_value ();

                fcn_name = unique_symbol_name ("__daspk_fcn__");
                fname = "function y = ";
                fname.append (fcn_name);
                fname.append (" (x, xdot, t) y = ");
                daspk_fcn = extract_function (tmp(0), "daspk", fcn_name,
                                              fname, "; endfunction");

                if (daspk_fcn)
                  {
                    jac_name = unique_symbol_name ("__daspk_jac__");
                    jname = "function jac = ";
                    jname.append (jac_name);
                    jname.append (" (x, xdot, t, cj) jac = ");
                    daspk_jac = extract_function (tmp(1), "daspk",
                                                  jac_name, jname,
                                                  "; endfunction");

                    if (! daspk_jac)
                      {
                        if (fcn_name.length ())
                          clear_function (fcn_name);
                        daspk_fcn = 0;
                      }
                  }
              }
            }
        }
    }

  if (! daspk_fcn)
    return retval;

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

  DAEFunc func (daspk_user_function);
  if (daspk_jac)
    func.set_jacobian_function (daspk_user_jacobian);

  DASPK dae (state, deriv, tzero, func);
  dae.set_options (daspk_opts);

  Matrix output;
  Matrix deriv_output;

  if (crit_times_set)
    output = dae.integrate (out_times, deriv_output, crit_times);
  else
    output = dae.integrate (out_times, deriv_output);

  if (fcn_name.length ())
    clear_function (fcn_name);
  if (jac_name.length ())
    clear_function (jac_name);

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
