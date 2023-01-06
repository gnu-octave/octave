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

#include <string>

#include "Quad.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "pager.h"
#include "parse.h"
#include "ov.h"
#include "ovl.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "Quad-opts.cc"

OCTAVE_BEGIN_NAMESPACE(octave)

// Global pointer for user defined function required by quadrature functions.
static octave_value quad_fcn;

// Have we warned about imaginary values returned from user function?
static bool warned_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

static double
quad_user_function (double x)
{
  double retval = 0.0;

  octave_value_list args;
  args(0) = x;

  if (quad_fcn.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (quad_fcn, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "quad");
        }

      if (! tmp.length () || ! tmp(0).is_defined ())
        err_user_supplied_eval ("quad");

      if (! warned_imaginary && tmp(0).iscomplex ())
        {
          warning ("quad: ignoring imaginary part returned from user-supplied function");
          warned_imaginary = true;
        }

      retval = tmp(0).xdouble_value ("quad: expecting user supplied function to return numeric value");
    }

  return retval;
}

static float
quad_float_user_function (float x)
{
  float retval = 0.0;

  octave_value_list args;
  args(0) = x;

  if (quad_fcn.is_defined ())
    {
      octave_value_list tmp;

      try
        {
          tmp = feval (quad_fcn, args, 1);
        }
      catch (execution_exception& ee)
        {
          err_user_supplied_eval (ee, "quad");
        }

      if (! tmp.length () || ! tmp(0).is_defined ())
        err_user_supplied_eval ("quad");

      if (! warned_imaginary && tmp(0).iscomplex ())
        {
          warning ("quad: ignoring imaginary part returned from user-supplied function");
          warned_imaginary = true;
        }

      retval = tmp(0).xfloat_value ("quad: expecting user supplied function to return numeric value");
    }

  return retval;
}

DEFMETHODX ("quad", Fquad, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn  {} {@var{q} =} quad (@var{f}, @var{a}, @var{b})
@deftypefnx {} {@var{q} =} quad (@var{f}, @var{a}, @var{b}, @var{tol})
@deftypefnx {} {@var{q} =} quad (@var{f}, @var{a}, @var{b}, @var{tol}, @var{sing})
@deftypefnx {} {[@var{q}, @var{ier}, @var{nfev}, @var{err}] =} quad (@dots{})
Numerically evaluate the integral of @var{f} from @var{a} to @var{b} using
Fortran routines from @w{@sc{quadpack}}.

@var{f} is a function handle, inline function, or a string containing the
name of the function to evaluate.  The function must have the form @code{y =
f (x)} where @var{y} and @var{x} are scalars.

@var{a} and @var{b} are the lower and upper limits of integration.  Either
or both may be infinite.

The optional argument @var{tol} is a vector that specifies the desired
accuracy of the result.  The first element of the vector is the desired
absolute tolerance, and the second element is the desired relative
tolerance.  To choose a relative test only, set the absolute
tolerance to zero.  To choose an absolute test only, set the relative
tolerance to zero.  Both tolerances default to @code{sqrt (eps)} or
approximately 1.5e-8.

The optional argument @var{sing} is a vector of values at which the
integrand is known to be singular.

The result of the integration is returned in @var{q}.

@var{ier} contains an integer error code (0 indicates a successful
integration).

@var{nfev} indicates the number of function evaluations that were
made.

@var{err} contains an estimate of the error in the solution.

The function @code{quad_options} can set other optional parameters for
@code{quad}.

Note: because @code{quad} is written in Fortran it cannot be called
recursively.  This prevents its use in integrating over more than one
variable by routines @code{dblquad} and @code{triplequad}.
@seealso{quad_options, quadv, quadl, quadgk, quadcc, trapz, dblquad, triplequad}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 3 || nargin > 5)
    print_usage ();

  warned_imaginary = false;

  unwind_protect_var<int> restore_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    error ("quad: invalid recursive call");

  quad_fcn = get_function_handle (interp, args(0), "x");

  octave_value_list retval;

  if (args(1).is_single_type () || args(2).is_single_type ())
    {
      float a = args(1).xfloat_value ("quad: lower limit of integration A must be a scalar");
      float b = args(2).xfloat_value ("quad: upper limit of integration B must be a scalar");

      int indefinite = 0;
      FloatIndefQuad::IntegralType indef_type
        = FloatIndefQuad::doubly_infinite;
      float bound = 0.0;
      if (math::isinf (a) && math::isinf (b))
        {
          indefinite = 1;
          indef_type = FloatIndefQuad::doubly_infinite;
        }
      else if (math::isinf (a))
        {
          indefinite = 1;
          bound = b;
          indef_type = FloatIndefQuad::neg_inf_to_bound;
        }
      else if (math::isinf (b))
        {
          indefinite = 1;
          bound = a;
          indef_type = FloatIndefQuad::bound_to_inf;
        }

      octave_idx_type ier = 0;
      octave_idx_type nfev = 0;
      float abserr = 0.0;
      float val = 0.0;
      bool have_sing = false;
      FloatColumnVector sing;
      FloatColumnVector tol;

      switch (nargin)
        {
        case 5:
          if (indefinite)
            error ("quad: singularities not allowed on infinite intervals");

          have_sing = true;

          sing = args(4).xfloat_vector_value ("quad: fifth argument SING must be a vector of singularities");
          OCTAVE_FALLTHROUGH;

        case 4:
          tol = args(3).xfloat_vector_value ("quad: TOL must be a 1 or 2-element vector");

          switch (tol.numel ())
            {
            case 2:
              quad_opts.set_single_precision_relative_tolerance (tol (1));
              OCTAVE_FALLTHROUGH;

            case 1:
              quad_opts.set_single_precision_absolute_tolerance (tol (0));
              break;

            default:
              error ("quad: TOL must be a 1 or 2-element vector");
            }
          OCTAVE_FALLTHROUGH;

        case 3:
          if (indefinite)
            {
              FloatIndefQuad iq (quad_float_user_function, bound,
                                 indef_type);
              iq.set_options (quad_opts);
              val = iq.float_integrate (ier, nfev, abserr);
            }
          else
            {
              if (have_sing)
                {
                  FloatDefQuad dq (quad_float_user_function, a, b, sing);
                  dq.set_options (quad_opts);
                  val = dq.float_integrate (ier, nfev, abserr);
                }
              else
                {
                  FloatDefQuad dq (quad_float_user_function, a, b);
                  dq.set_options (quad_opts);
                  val = dq.float_integrate (ier, nfev, abserr);
                }
            }
          break;

        default:
          panic_impossible ();
          break;
        }

      retval = ovl (val, ier, nfev, abserr);

    }
  else
    {
      double a = args(1).xdouble_value ("quad: lower limit of integration A must be a scalar");
      double b = args(2).xdouble_value ("quad: upper limit of integration B must be a scalar");

      int indefinite = 0;
      IndefQuad::IntegralType indef_type = IndefQuad::doubly_infinite;
      double bound = 0.0;
      if (math::isinf (a) && math::isinf (b))
        {
          indefinite = 1;
          indef_type = IndefQuad::doubly_infinite;
        }
      else if (math::isinf (a))
        {
          indefinite = 1;
          bound = b;
          indef_type = IndefQuad::neg_inf_to_bound;
        }
      else if (math::isinf (b))
        {
          indefinite = 1;
          bound = a;
          indef_type = IndefQuad::bound_to_inf;
        }

      octave_idx_type ier = 0;
      octave_idx_type nfev = 0;
      double abserr = 0.0;
      double val = 0.0;
      bool have_sing = false;
      ColumnVector sing;
      ColumnVector tol;

      switch (nargin)
        {
        case 5:
          if (indefinite)
            error ("quad: singularities not allowed on infinite intervals");

          have_sing = true;

          sing = args(4).xvector_value ("quad: fifth argument SING must be a vector of singularities");
          OCTAVE_FALLTHROUGH;

        case 4:
          tol = args(3).xvector_value ("quad: TOL must be a 1 or 2-element vector");

          switch (tol.numel ())
            {
            case 2:
              quad_opts.set_relative_tolerance (tol (1));
              OCTAVE_FALLTHROUGH;

            case 1:
              quad_opts.set_absolute_tolerance (tol (0));
              break;

            default:
              error ("quad: TOL must be a 1 or 2-element vector");
            }
          OCTAVE_FALLTHROUGH;

        case 3:
          if (indefinite)
            {
              IndefQuad iq (quad_user_function, bound, indef_type);
              iq.set_options (quad_opts);
              val = iq.integrate (ier, nfev, abserr);
            }
          else
            {
              if (have_sing)
                {
                  DefQuad dq (quad_user_function, a, b, sing);
                  dq.set_options (quad_opts);
                  val = dq.integrate (ier, nfev, abserr);
                }
              else
                {
                  DefQuad dq (quad_user_function, a, b);
                  dq.set_options (quad_opts);
                  val = dq.integrate (ier, nfev, abserr);
                }
            }
          break;

        default:
          panic_impossible ();
          break;
        }

      retval = ovl (val, ier, nfev, abserr);
    }

  return retval;
}

/*
%!function y = __f (x)
%!  y = x + 1;
%!endfunction

%!test
%! [v, ier, nfev, err] = quad ("__f", 0, 5);
%! assert (ier, 0);
%! assert (v, 17.5, sqrt (eps));
%! assert (nfev > 0);
%! assert (err < sqrt (eps));

%!test
%! [v, ier, nfev, err] = quad ("__f", single (0), single (5));
%! assert (ier, 0);
%! assert (v, 17.5, sqrt (eps ("single")));
%! assert (nfev > 0);
%! assert (err < sqrt (eps ("single")));

%!function y = __f (x)
%!  y = x .* sin (1 ./ x) .* sqrt (abs (1 - x));
%!endfunction

%!test
%!  [v, ier, nfev, err] = quad ("__f", 0.001, 3);
%! assert (ier == 0 || ier == 1);
%! assert (v, 1.98194120273598, sqrt (eps));
%! assert (nfev > 0);

%!test
%!  [v, ier, nfev, err] = quad (@__f, 0.001, 3);
%! assert (ier == 0 || ier == 1);
%! assert (v, 1.98194120273598, sqrt (eps));
%! assert (nfev > 0);

%!test
%!  fstr = "x .* sin (1 ./ x) .* sqrt (abs (1 - x))";
%!  [v, ier, nfev, err] = quad (fstr, 0.001, 3);
%! assert (ier == 0 || ier == 1);
%! assert (v, 1.98194120273598, sqrt (eps));
%! assert (nfev > 0);

%!test
%!  anon_fcn = @(x) x .* sin (1 ./ x) .* sqrt (abs (1 - x));
%!  [v, ier, nfev, err] = quad (anon_fcn, 0.001, 3);
%! assert (ier == 0 || ier == 1);
%! assert (v, 1.98194120273598, sqrt (eps));
%! assert (nfev > 0);

%!test
%!  inline_fcn = inline ("x .* sin (1 ./ x) .* sqrt (abs (1 - x))", "x");
%!  [v, ier, nfev, err] = quad (inline_fcn, 0.001, 3);
%! assert (ier == 0 || ier == 1);
%! assert (v, 1.98194120273598, sqrt (eps));
%! assert (nfev > 0);

%!test
%!  [v, ier, nfev, err] = quad ("__f", single (0.001), single (3));
%! assert (ier == 0 || ier == 1);
%! assert (v, 1.98194120273598, sqrt (eps ("single")));
%! assert (nfev > 0);

%!error quad ()
%!error quad ("__f", 1, 2, 3, 4, 5)

%!test
%! quad_options ("absolute tolerance", eps);
%! assert (quad_options ("absolute tolerance") == eps);

%!error quad_options (1, 2, 3)
*/

OCTAVE_END_NAMESPACE(octave)
