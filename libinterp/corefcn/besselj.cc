////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1997-2023 The Octave Project Developers
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

#include "lo-specfun.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

enum bessel_type
{
  BESSEL_J,
  BESSEL_Y,
  BESSEL_I,
  BESSEL_K,
  BESSEL_H1,
  BESSEL_H2
};

#define DO_BESSEL(type, alpha, x, scaled, ierr, result)                 \
  do                                                                    \
    {                                                                   \
      switch (type)                                                     \
        {                                                               \
        case BESSEL_J:                                                  \
          result = math::besselj (alpha, x, scaled, ierr);      \
          break;                                                        \
                                                                        \
        case BESSEL_Y:                                                  \
          result = math::bessely (alpha, x, scaled, ierr);      \
          break;                                                        \
                                                                        \
        case BESSEL_I:                                                  \
          result = math::besseli (alpha, x, scaled, ierr);      \
          break;                                                        \
                                                                        \
        case BESSEL_K:                                                  \
          result = math::besselk (alpha, x, scaled, ierr);      \
          break;                                                        \
                                                                        \
        case BESSEL_H1:                                                 \
          result = math::besselh1 (alpha, x, scaled, ierr);     \
          break;                                                        \
                                                                        \
        case BESSEL_H2:                                                 \
          result = math::besselh2 (alpha, x, scaled, ierr);     \
          break;                                                        \
                                                                        \
        default:                                                        \
          break;                                                        \
        }                                                               \
    }                                                                   \
  while (0)

octave_value_list
do_bessel (enum bessel_type type, const char *fcn,
           const octave_value_list& args, int nargout)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value_list retval (nargout > 1 ? 2 : 1);

  bool scaled = false;
  if (nargin == 3)
    {
      octave_value opt_arg = args(2);
      bool rpt_error = false;

      if (! opt_arg.is_scalar_type ())
        rpt_error = true;
      else if (opt_arg.isnumeric ())
        {
          double opt_val = opt_arg.double_value ();
          if (opt_val != 0.0 && opt_val != 1.0)
            rpt_error = true;
          scaled = (opt_val == 1.0);
        }
      else if (opt_arg.islogical ())
        scaled = opt_arg.bool_value ();

      if (rpt_error)
        error ("%s: OPT must be 0 (or false) or 1 (or true)", fcn);
    }

  octave_value alpha_arg = args(0);
  octave_value x_arg = args(1);

  if (alpha_arg.is_single_type () || x_arg.is_single_type ())
    {
      if (alpha_arg.is_scalar_type ())
        {
          float alpha = args(0).xfloat_value ("%s: ALPHA must be a scalar or matrix", fcn);

          if (x_arg.is_scalar_type ())
            {
              FloatComplex x = x_arg.xfloat_complex_value ("%s: X must be a scalar or matrix", fcn);

              octave_idx_type ierr;
              octave_value result;

              DO_BESSEL (type, alpha, x, scaled, ierr, result);

              retval(0) = result;
              if (nargout > 1)
                retval(1) = static_cast<float> (ierr);
            }
          else
            {
              FloatComplexNDArray x = x_arg.xfloat_complex_array_value ("%s: X must be a scalar or matrix", fcn);

              Array<octave_idx_type> ierr;
              octave_value result;

              DO_BESSEL (type, alpha, x, scaled, ierr, result);

              retval(0) = result;
              if (nargout > 1)
                retval(1) = NDArray (ierr);
            }
        }
      else
        {
          dim_vector dv0 = args(0).dims ();
          dim_vector dv1 = args(1).dims ();

          bool args0_is_row_vector = (dv0(1) == dv0.numel ());
          bool args1_is_col_vector = (dv1(0) == dv1.numel ());

          if (args0_is_row_vector && args1_is_col_vector)
            {
              FloatRowVector ralpha = args(0).xfloat_row_vector_value ("%s: ALPHA must be a scalar or matrix",
                                      fcn);

              FloatComplexColumnVector cx
                = x_arg.xfloat_complex_column_vector_value ("%s: X must be a scalar or matrix", fcn);

              Array<octave_idx_type> ierr;
              octave_value result;

              DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

              retval(0) = result;
              if (nargout > 1)
                retval(1) = NDArray (ierr);
            }
          else
            {
              FloatNDArray alpha = args(0).xfloat_array_value ("%s: ALPHA must be a scalar or matrix", fcn);

              if (x_arg.is_scalar_type ())
                {
                  FloatComplex x = x_arg.xfloat_complex_value ("%s: X must be a scalar or matrix", fcn);

                  Array<octave_idx_type> ierr;
                  octave_value result;

                  DO_BESSEL (type, alpha, x, scaled, ierr, result);

                  retval(0) = result;
                  if (nargout > 1)
                    retval(1) = NDArray (ierr);
                }
              else
                {
                  FloatComplexNDArray x = x_arg.xfloat_complex_array_value ("%s: X must be a scalar or matrix", fcn);

                  Array<octave_idx_type> ierr;
                  octave_value result;

                  DO_BESSEL (type, alpha, x, scaled, ierr, result);

                  retval(0) = result;
                  if (nargout > 1)
                    retval(1) = NDArray (ierr);
                }
            }
        }
    }
  else
    {
      if (alpha_arg.is_scalar_type ())
        {
          double alpha = args(0).xdouble_value ("%s: ALPHA must be a scalar or matrix", fcn);

          if (x_arg.is_scalar_type ())
            {
              Complex x = x_arg.xcomplex_value ("%s: X must be a scalar or matrix", fcn);

              octave_idx_type ierr;
              octave_value result;

              DO_BESSEL (type, alpha, x, scaled, ierr, result);

              retval(0) = result;
              if (nargout > 1)
                retval(1) = static_cast<double> (ierr);
            }
          else
            {
              ComplexNDArray x = x_arg.xcomplex_array_value ("%s: X must be a scalar or matrix", fcn);

              Array<octave_idx_type> ierr;
              octave_value result;

              DO_BESSEL (type, alpha, x, scaled, ierr, result);

              retval(0) = result;
              if (nargout > 1)
                retval(1) = NDArray (ierr);
            }
        }
      else
        {
          dim_vector dv0 = args(0).dims ();
          dim_vector dv1 = args(1).dims ();

          bool args0_is_row_vector = (dv0(1) == dv0.numel ());
          bool args1_is_col_vector = (dv1(0) == dv1.numel ());

          if (args0_is_row_vector && args1_is_col_vector)
            {
              RowVector ralpha = args(0).xrow_vector_value ("%s: ALPHA must be a scalar or matrix", fcn);

              ComplexColumnVector cx
                = x_arg.xcomplex_column_vector_value ("%s: X must be a scalar or matrix", fcn);

              Array<octave_idx_type> ierr;
              octave_value result;

              DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

              retval(0) = result;
              if (nargout > 1)
                retval(1) = NDArray (ierr);
            }
          else
            {
              NDArray alpha = args(0).xarray_value ("%s: ALPHA must be a scalar or matrix", fcn);

              if (x_arg.is_scalar_type ())
                {
                  Complex x = x_arg.xcomplex_value ("%s: X must be a scalar or matrix", fcn);

                  Array<octave_idx_type> ierr;
                  octave_value result;

                  DO_BESSEL (type, alpha, x, scaled, ierr, result);

                  retval(0) = result;
                  if (nargout > 1)
                    retval(1) = NDArray (ierr);
                }
              else
                {
                  ComplexNDArray x = x_arg.xcomplex_array_value ("%s: X must be a scalar or matrix", fcn);

                  Array<octave_idx_type> ierr;
                  octave_value result;

                  DO_BESSEL (type, alpha, x, scaled, ierr, result);

                  retval(0) = result;
                  if (nargout > 1)
                    retval(1) = NDArray (ierr);
                }
            }
        }
    }

  return retval;
}

DEFUN (besselj, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{J} =} besselj (@var{alpha}, @var{x})
@deftypefnx {} {@var{J} =} besselj (@var{alpha}, @var{x}, @var{opt})
@deftypefnx {} {[@var{J}, @var{ierr}] =} besselj (@dots{})
Compute Bessel functions of the first kind.

The order of the Bessel function @var{alpha} must be real.  The points for
evaluation @var{x} may be complex.

If the optional argument @var{opt} is 1 or true, the result @var{J} is
multiplied by @w{@code{exp (-abs (imag (@var{x})))}}.

If @var{alpha} is a scalar, the result is the same size as @var{x}.  If @var{x}
is a scalar, the result is the same size as @var{alpha}.  If @var{alpha} is a
row vector and @var{x} is a column vector, the result is a matrix with
@code{length (@var{x})} rows and @code{length (@var{alpha})} columns.
Otherwise, @var{alpha} and @var{x} must conform and the result will be the same
size.

If requested, @var{ierr} contains the following status information and is the
same size as the result.

@enumerate 0
@item
Normal return.

@item
Input error, return @code{NaN}.

@item
Overflow, return @code{Inf}.

@item
Loss of significance by argument reduction results in less than half of machine
accuracy.

@item
Loss of significance by argument reduction, output may be inaccurate.

@item
Error---no computation, algorithm termination condition not met, return
@code{NaN}.
@end enumerate

@seealso{bessely, besseli, besselk, besselh}
@end deftypefn */)
{
  return do_bessel (BESSEL_J, "besselj", args, nargout);
}

/*
%!# Function besselj is tested along with other bessels at the end of this file

## besselj specific BIST
%!assert <*48316> (! any (isnan (besselj (1, 10.^[5:12]))))

*/

DEFUN (bessely, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{Y} =} bessely (@var{alpha}, @var{x})
@deftypefnx {} {@var{Y} =} bessely (@var{alpha}, @var{x}, @var{opt})
@deftypefnx {} {[@var{Y}, @var{ierr}] =} bessely (@dots{})
Compute Bessel functions of the second kind.

The order of the Bessel function @var{alpha} must be real.  The points for
evaluation @var{x} may be complex.

If the optional argument @var{opt} is 1 or true, the result @var{Y} is
multiplied by @w{@code{exp (-abs (imag (@var{x})))}}.

If @var{alpha} is a scalar, the result is the same size as @var{x}.  If @var{x}
is a scalar, the result is the same size as @var{alpha}.  If @var{alpha} is a
row vector and @var{x} is a column vector, the result is a matrix with
@code{length (@var{x})} rows and @code{length (@var{alpha})} columns.
Otherwise, @var{alpha} and @var{x} must conform and the result will be the same
size.

If requested, @var{ierr} contains the following status information and is the
same size as the result.

@enumerate 0
@item
Normal return.

@item
Input error, return @code{NaN}.

@item
Overflow, return @code{Inf}.

@item
Loss of significance by argument reduction results in less than half of machine
accuracy.

@item
Complete loss of significance by argument reduction, return @code{NaN}.

@item
Error---no computation, algorithm termination condition not met, return
@code{NaN}.
@end enumerate

@seealso{besselj, besseli, besselk, besselh}
@end deftypefn */)
{
  return do_bessel (BESSEL_Y, "bessely", args, nargout);
}

/*
%!# Function bessely is tested along with other bessels at the end of this file
*/

DEFUN (besseli, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{I} =} besseli (@var{alpha}, @var{x})
@deftypefnx {} {@var{I} =} besseli (@var{alpha}, @var{x}, @var{opt})
@deftypefnx {} {[@var{I}, @var{ierr}] =} besseli (@dots{})
Compute modified Bessel functions of the first kind.

The order of the Bessel function @var{alpha} must be real.  The points for
evaluation @var{x} may be complex.

If the optional argument @var{opt} is 1 or true, the result @var{I} is
multiplied by @w{@code{exp (-abs (real (@var{x})))}}.

If @var{alpha} is a scalar, the result is the same size as @var{x}.  If @var{x}
is a scalar, the result is the same size as @var{alpha}.  If @var{alpha} is a
row vector and @var{x} is a column vector, the result is a matrix with
@code{length (@var{x})} rows and @code{length (@var{alpha})} columns.
Otherwise, @var{alpha} and @var{x} must conform and the result will be the same
size.

If requested, @var{ierr} contains the following status information and is the
same size as the result.

@enumerate 0
@item
Normal return.

@item
Input error, return @code{NaN}.

@item
Overflow, return @code{Inf}.

@item
Loss of significance by argument reduction results in less than half of machine
accuracy.

@item
Complete loss of significance by argument reduction, return @code{NaN}.

@item
Error---no computation, algorithm termination condition not met, return
@code{NaN}.
@end enumerate

@seealso{besselk, besselj, bessely, besselh}
@end deftypefn */)

{
  return do_bessel (BESSEL_I, "besseli", args, nargout);
}

/*
%!# Function besseli is tested along with other bessels at the end of this file

## besseli specific BIST
%!assert (! isinf (besseli (0, 700)))

*/

DEFUN (besselk, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{K} =} besselk (@var{alpha}, @var{x})
@deftypefnx {} {@var{K} =} besselk (@var{alpha}, @var{x}, @var{opt})
@deftypefnx {} {[@var{K}, @var{ierr}] =} besselk (@dots{})

Compute modified Bessel functions of the second kind.

The order of the Bessel function @var{alpha} must be real.  The points for
evaluation @var{x} may be complex.

If the optional argument @var{opt} is 1 or true, the result @var{K} is
multiplied by @w{@code{exp (@var{x})}}.

If @var{alpha} is a scalar, the result is the same size as @var{x}.  If @var{x}
is a scalar, the result is the same size as @var{alpha}.  If @var{alpha} is a
row vector and @var{x} is a column vector, the result is a matrix with
@code{length (@var{x})} rows and @code{length (@var{alpha})} columns.
Otherwise, @var{alpha} and @var{x} must conform and the result will be the same
size.

If requested, @var{ierr} contains the following status information and is the
same size as the result.

@enumerate 0
@item
Normal return.

@item
Input error, return @code{NaN}.

@item
Overflow, return @code{Inf}.

@item
Loss of significance by argument reduction results in less than half of machine
accuracy.

@item
Complete loss of significance by argument reduction, return @code{NaN}.

@item
Error---no computation, algorithm termination condition not met, return
@code{NaN}.
@end enumerate

@seealso{besseli, besselj, bessely, besselh}
@end deftypefn */)
{
  return do_bessel (BESSEL_K, "besselk", args, nargout);
}

/*
%!# Function besselk is tested along with other bessels at the end of this file
*/

DEFUN (besselh, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{H} =} besselh (@var{alpha}, @var{x})
@deftypefnx {} {@var{H} =} besselh (@var{alpha}, @var{k}, @var{x})
@deftypefnx {} {@var{H} =} besselh (@var{alpha}, @var{k}, @var{x}, @var{opt})
@deftypefnx {} {[@var{H}, @var{ierr}] =} besselh (@dots{})
Compute Bessel functions of the third kind (Hankel functions).

The order of the Bessel function @var{alpha} must be real.  The kind of Hankel
function is specified by @var{k} and may be either first (@var{k} = 1) or
second (@var{k} = 2).  The default is Hankel functions of the first kind.  The
points for evaluation @var{x} may be complex.

If the optional argument @var{opt} is 1 or true, the result is multiplied
by @code{exp (-I*@var{x})} for @var{k} = 1 or @code{exp (I*@var{x})} for
@var{k} = 2.

If @var{alpha} is a scalar, the result is the same size as @var{x}.  If @var{x}
is a scalar, the result is the same size as @var{alpha}.  If @var{alpha} is a
row vector and @var{x} is a column vector, the result is a matrix with
@code{length (@var{x})} rows and @code{length (@var{alpha})} columns.
Otherwise, @var{alpha} and @var{x} must conform and the result will be the same
size.

If requested, @var{ierr} contains the following status information and is the
same size as the result.

@enumerate 0
@item
Normal return.

@item
Input error, return @code{NaN}.

@item
Overflow, return @code{Inf}.

@item
Loss of significance by argument reduction results in less than half of machine
accuracy.

@item
Complete loss of significance by argument reduction, return @code{NaN}.

@item
Error---no computation, algorithm termination condition not met, return
@code{NaN}.
@end enumerate

@seealso{besselj, bessely, besseli, besselk}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 4)
    print_usage ();

  octave_value_list retval;

  if (nargin == 2)
    {
      retval = do_bessel (BESSEL_H1, "besselh", args, nargout);
    }
  else
    {
      octave_idx_type kind = args(1).xint_value ("besselh: invalid value of K");

      octave_value_list tmp_args;

      if (nargin == 4)
        tmp_args(2) = args(3);

      tmp_args(1) = args(2);
      tmp_args(0) = args(0);

      if (kind == 1)
        retval = do_bessel (BESSEL_H1, "besselh", tmp_args, nargout);
      else if (kind == 2)
        retval = do_bessel (BESSEL_H2, "besselh", tmp_args, nargout);
      else
        error ("besselh: K must be 1 or 2");
    }

  return retval;
}

/*
%!# Function besselh is tested along with other bessels at the end of this file
*/

DEFUN (airy, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{a} =} airy (@var{z})
@deftypefnx {} {@var{a} =} airy (@var{k}, @var{z})
@deftypefnx {} {@var{a} =} airy (@var{k}, @var{z}, @var{scale})
@deftypefnx {} {[@var{a}, @var{ierr}] =} airy (@dots{})

Compute Airy functions of the first and second kind, and their derivatives.

@example
@group
 K   Function   Scale factor (if @var{scale} is true)
---  --------   ---------------------------------------
 0   Ai (Z)     exp ((2/3) * Z * sqrt (Z))
 1   dAi(Z)/dZ  exp ((2/3) * Z * sqrt (Z))
 2   Bi (Z)     exp (-abs (real ((2/3) * Z * sqrt (Z))))
 3   dBi(Z)/dZ  exp (-abs (real ((2/3) * Z * sqrt (Z))))
@end group
@end example

The function call @code{airy (@var{z})} is equivalent to
@code{airy (0, @var{z})}.

The optional third input @var{scale} determines whether to
apply scaling as described above.  It is false by default.

The result @var{a} is the same size as @var{z}.

The optional output @var{ierr} contains the following status information and
is the same size as the result.

@enumerate 0
@item
Normal return.

@item
Input error, return @code{NaN}.

@item
Overflow, return @code{Inf}.

@item
Loss of significance by argument reduction results in less than half
 of machine accuracy.

@item
Loss of significance by argument reduction, output may be inaccurate.

@item
Error---no computation, algorithm termination condition not met,
return @code{NaN}.
@end enumerate
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value_list retval (nargout > 1 ? 2 : 1);

  int kind = 0;
  if (nargin > 1)
    {
      kind = args(0).xint_value ("airy: K must be an integer value");

      if (kind < 0 || kind > 3)
        error ("airy: K must be 0, 1, 2, or 3");
    }

  bool scale = (nargin == 3) && args(2).xbool_value ("airy: scale option must be a logical value");

  int idx = (nargin == 1 ? 0 : 1);

  Array<octave_idx_type> ierr;
  octave_value result;

  if (args(idx).is_single_type ())
    {
      FloatComplexNDArray z = args(idx).xfloat_complex_array_value ("airy: Z must be a complex matrix");

      if (kind > 1)
        result = math::biry (z, kind == 3, scale, ierr);
      else
        result = math::airy (z, kind == 1, scale, ierr);
    }
  else
    {
      ComplexNDArray z = args(idx).xcomplex_array_value ("airy: Z must be a complex matrix");

      if (kind > 1)
        result = math::biry (z, kind == 3, scale, ierr);
      else
        result = math::airy (z, kind == 1, scale, ierr);
    }

  retval(0) = result;
  if (nargout > 1)
    retval(1) = NDArray (ierr);

  return retval;
}

/*
%!test <*62321>
%! assert (airy (0, +1, false),   0.1352924163128814,                       1e-15);
%! assert (airy (0, -1, false),   0.5355608832923521,                       1e-15);
%! assert (airy (0, +i, false),   0.3314933054321411 - 0.3174498589684437i, 1e-15);
%! assert (airy (0, -i, false),   0.3314933054321411 + 0.3174498589684437i, 1e-15);
%! assert (airy (0, +1,  true),   0.2635136447491401,                       1e-15);
%! assert (airy (0, -1,  true),   0.4208904755499093 - 0.3311746779333462i, 1e-15);
%! assert (airy (0, +i,  true),   0.2743053542644657 - 0.0825606941400591i, 1e-15);
%! assert (airy (0, -i,  true),   0.2743053542644657 + 0.0825606941400591i, 1e-15);
%! assert (airy (1, +1, false),  -0.1591474412967932,                       1e-15);
%! assert (airy (1, -1, false),  -0.0101605671166451,                       1e-15);
%! assert (airy (1, +i, false),  -0.4324926598418070 + 0.0980478562292432i, 1e-15);
%! assert (airy (1, -i, false),  -0.4324926598418070 - 0.0980478562292432i, 1e-15);
%! assert (airy (1, +1,  true),  -0.3099768889605148,                       1e-15);
%! assert (airy (1, -1,  true),  -0.0079850602592406 + 0.0062829878869967i, 1e-15);
%! assert (airy (1, +i,  true),  -0.2682792317254506 - 0.0680655016581369i, 1e-15);
%! assert (airy (1, -i,  true),  -0.2682792317254506 + 0.0680655016581369i, 1e-15);
%! assert (airy (2, +1, false),   1.2074235949528715,                       1e-15);
%! assert (airy (2, -1, false),   0.1039973894969446,                       1e-15);
%! assert (airy (2, +i, false),   0.6488582083303949 + 0.3449586347680484i, 1e-15);
%! assert (airy (2, -i, false),   0.6488582083303949 - 0.3449586347680484i, 1e-15);
%! assert (airy (2, +1,  true),   0.6199119435726786,                       1e-15);
%! assert (airy (2, -1,  true),   0.1039973894969446,                       1e-15);
%! assert (airy (2, +i,  true),   0.4049686654663902 + 0.2152973271658010i, 1e-15);
%! assert (airy (2, -i,  true),   0.4049686654663902 - 0.2152973271658010i, 1e-15);
%! assert (airy (3, +1, false),   0.9324359333927754,                       1e-15);
%! assert (airy (3, -1, false),   0.5923756264227923,                       1e-15);
%! assert (airy (3, +i, false),   0.1350266467108190 - 0.1288373867812549i, 1e-15);
%! assert (airy (3, -i, false),   0.1350266467108190 + 0.1288373867812549i, 1e-15);
%! assert (airy (3, +1,  true),   0.4787285706049846,                       1e-15);
%! assert (airy (3, -1,  true),   0.5923756264227923,                       1e-15);
%! assert (airy (3, +i,  true),   0.0842735134099415 - 0.0804106412111761i, 1e-15);
%! assert (airy (3, -i,  true),   0.0842735134099415 + 0.0804106412111761i, 1e-15);
%! z = -1 - 1e-6i;
%! s1 = exp (2/3 * z * sqrt(z));
%! s2 = exp (-abs (real (2/3 * z * sqrt(z))));
%! assert (airy (0, z, true), s1 * airy (0, z, false), 1e-15);
%! assert (airy (1, z, true), s1 * airy (1, z, false), 1e-15);
%! assert (airy (2, z, true), s2 * airy (2, z, false), 1e-15);
%! assert (airy (3, z, true), s2 * airy (3, z, false), 1e-15);

Input validation tests
%!error airy ()
%!error airy (0, 1, 2, 3)
%!error <K must be an integer value> airy ("foo", 2, false)
%!error <K must be 0, 1, 2, or 3> airy (3743, 2, false)
%!error <scale option must be a logical value> airy (0, 2, "foo")
%!error <Z must be a complex matrix> airy (0, "foo", false)
*/


/*
## Test values computed with GP/PARI version 2.3.3
%!shared alpha, x, jx, yx, ix, kx, nix
%!
%! ## Bessel functions, even order, positive and negative x
%! alpha = 2;  x = 1.25;
%! jx = 0.1710911312405234823613091417;
%! yx = -1.193199310178553861283790424;
%! ix = 0.2220184483766341752692212604;
%! kx = 0.9410016167388185767085460540;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%!assert (besselj (-alpha,x), jx, 100*eps)
%!assert (bessely (-alpha,x), yx, 100*eps)
%!assert (besseli (-alpha,x), ix, 100*eps)
%!assert (besselk (-alpha,x), kx, 100*eps)
%!assert (besselh (-alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (-alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (-alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (-alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (-alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (-alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (-alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (-alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! x *= -1;
%! yx = -1.193199310178553861283790424 + 0.3421822624810469647226182835*I;
%! kx = 0.9410016167388185767085460540 - 0.6974915263814386815610060884*I;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! ## Bessel functions, odd order, positive and negative x
%! alpha = 3;  x = 2.5;
%! jx = 0.2166003910391135247666890035;
%! yx = -0.7560554967536709968379029772;
%! ix = 0.4743704087780355895548240179;
%! kx = 0.2682271463934492027663765197;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%!assert (besselj (-alpha,x), -jx, 100*eps)
%!assert (bessely (-alpha,x), -yx, 100*eps)
%!assert (besseli (-alpha,x), ix, 100*eps)
%!assert (besselk (-alpha,x), kx, 100*eps)
%!assert (besselh (-alpha,1,x), -(jx + I*yx), 100*eps)
%!assert (besselh (-alpha,2,x), -(jx - I*yx), 100*eps)
%!
%!assert (besselj (-alpha,x,1), -jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (-alpha,x,1), -yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (-alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (-alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (-alpha,1,x,1), -(jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (-alpha,2,x,1), -(jx - I*yx)* exp (I*x), 100*eps)
%!
%! x *= -1;
%! jx = -jx;
%! yx = 0.7560554967536709968379029772 - 0.4332007820782270495333780070*I;
%! ix = -ix;
%! kx = -0.2682271463934492027663765197 - 1.490278591297463775542004240*I;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! ## Bessel functions, fractional order, positive and negative x
%!
%! alpha = 3.5;  x = 2.75;
%! jx = 0.1691636439842384154644784389;
%! yx = -0.8301381935499356070267953387;
%! ix = 0.3930540878794826310979363668;
%! kx = 0.2844099013460621170288192503;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! nix = 0.2119931212254662995364461998;
%!
%!assert (besselj (-alpha,x), yx, 100*eps)
%!assert (bessely (-alpha,x), -jx, 100*eps)
%!assert (besseli (-alpha,x), nix, 100*eps)
%!assert (besselk (-alpha,x), kx, 100*eps)
%!assert (besselh (-alpha,1,x), -I*(jx + I*yx), 100*eps)
%!assert (besselh (-alpha,2,x), I*(jx - I*yx), 100*eps)
%!
%!assert (besselj (-alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (-alpha,x,1), -jx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (-alpha,x,1), nix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (-alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (-alpha,1,x,1), -I*(jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (-alpha,2,x,1), I*(jx - I*yx)* exp (I*x), 100*eps)
%!
%! x *= -1;
%! jx *= -I;
%! yx = -0.8301381935499356070267953387*I;
%! ix *= -I;
%! kx = -0.9504059335995575096509874508*I;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! ## Bessel functions, even order, complex x
%!
%! alpha = 2;  x = 1.25 + 3.625 * I;
%! jx = -1.299533366810794494030065917 + 4.370833116012278943267479589*I;
%! yx = -4.370357232383223896393056727 - 1.283083391453582032688834041*I;
%! ix = -0.6717801680341515541002273932 - 0.2314623443930774099910228553*I;
%! kx = -0.01108009888623253515463783379 + 0.2245218229358191588208084197*I;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%!assert (besselj (-alpha,x), jx, 100*eps)
%!assert (bessely (-alpha,x), yx, 100*eps)
%!assert (besseli (-alpha,x), ix, 100*eps)
%!assert (besselk (-alpha,x), kx, 100*eps)
%!assert (besselh (-alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (-alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (-alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (-alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (-alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (-alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (-alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (-alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! ## Bessel functions, odd order, complex x
%!
%! alpha = 3; x = 2.5 + 1.875 * I;
%! jx = 0.1330721523048277493333458596 + 0.5386295217249660078754395597*I;
%! yx = -0.6485072392105829901122401551 + 0.2608129289785456797046996987*I;
%! ix = -0.6182064685486998097516365709 + 0.4677561094683470065767989920*I;
%! kx = -0.1568585587733540007867882337 - 0.05185853709490846050505141321*I;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%!assert (besselj (-alpha,x), -jx, 100*eps)
%!assert (bessely (-alpha,x), -yx, 100*eps)
%!assert (besseli (-alpha,x), ix, 100*eps)
%!assert (besselk (-alpha,x), kx, 100*eps)
%!assert (besselh (-alpha,1,x), -(jx + I*yx), 100*eps)
%!assert (besselh (-alpha,2,x), -(jx - I*yx), 100*eps)
%!
%!assert (besselj (-alpha,x,1), -jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (-alpha,x,1), -yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (-alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (-alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (-alpha,1,x,1), -(jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (-alpha,2,x,1), -(jx - I*yx)* exp (I*x), 100*eps)
%!
%! ## Bessel functions, fractional order, complex x
%!
%! alpha = 3.5;  x = 1.75 + 4.125 * I;
%! jx = -3.018566131370455929707009100 - 0.7585648436793900607704057611*I;
%! yx = 0.7772278839106298215614791107 - 3.018518722313849782683792010*I;
%! ix = 0.2100873577220057189038160913 - 0.6551765604618246531254970926*I;
%! kx = 0.1757147290513239935341488069 + 0.08772348296883849205562558311*I;
%!
%!assert (besselj (alpha,x), jx, 100*eps)
%!assert (bessely (alpha,x), yx, 100*eps)
%!assert (besseli (alpha,x), ix, 100*eps)
%!assert (besselk (alpha,x), kx, 100*eps)
%!assert (besselh (alpha,1,x), jx + I*yx, 100*eps)
%!assert (besselh (alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert (besselj (alpha,x,1), jx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (alpha,x,1), ix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (alpha,1,x,1), (jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (alpha,2,x,1), (jx - I*yx)* exp (I*x), 100*eps)
%!
%! nix = 0.09822388691172060573913739253 - 0.7110230642207380127317227407*I;
%!
%!assert (besselj (-alpha,x), yx, 100*eps)
%!assert (bessely (-alpha,x), -jx, 100*eps)
%!assert (besseli (-alpha,x), nix, 100*eps)
%!assert (besselk (-alpha,x), kx, 100*eps)
%!assert (besselh (-alpha,1,x), -I*(jx + I*yx), 100*eps)
%!assert (besselh (-alpha,2,x), I*(jx - I*yx), 100*eps)
%!
%!assert (besselj (-alpha,x,1), yx* exp (-abs (imag (x))), 100*eps)
%!assert (bessely (-alpha,x,1), -jx* exp (-abs (imag (x))), 100*eps)
%!assert (besseli (-alpha,x,1), nix* exp (-abs (real (x))), 100*eps)
%!assert (besselk (-alpha,x,1), kx* exp (x), 100*eps)
%!assert (besselh (-alpha,1,x,1), -I*(jx + I*yx)* exp (-I*x), 100*eps)
%!assert (besselh (-alpha,2,x,1), I*(jx - I*yx)* exp (I*x), 100*eps)

Tests contributed by Robert T. Short.
Tests are based on the properties and tables in A&S:
 Abramowitz and Stegun, "Handbook of Mathematical Functions",
 1972.

For regular Bessel functions, there are 3 tests.  These compare octave
results against Tables 9.1, 9.2, and 9.4 in A&S. Tables 9.1 and 9.2
are good to only a few decimal places, so any failures should be
considered a broken implementation.  Table 9.4 is an extended table
for larger orders and arguments.  There are some differences between
Octave and Table 9.4, mostly in the last decimal place but in a very
few instances the errors are in the last two places.  The comparison
tolerance has been changed to reflect this.

Similarly for modified Bessel functions, there are 3 tests.  These
compare octave results against Tables 9.8, 9.9, and 9.11 in A&S.
Tables 9.8 and 9.9 are good to only a few decimal places, so any
failures should be considered a broken implementation.  Table 9.11 is
an extended table for larger orders and arguments.  There are some
differences between octave and Table 9.11, mostly in the last decimal
place but in a very few instances the errors are in the last two
places.  The comparison tolerance has been changed to reflect this.

For spherical Bessel functions, there are also three tests, comparing
octave results to Tables 10.1, 10.2, and 10.4 in A&S.  Very similar
comments may be made here as in the previous lines.  At this time,
modified spherical Bessel function tests are not included.

% Table 9.1 - J and Y for integer orders 0, 1, 2.
% Compare against excerpts of Table 9.1, Abramowitz and Stegun.
%!test
%! n = 0:2;
%! z = (0:2.5:17.5)';
%!
%! Jt = [[ 1.000000000000000,  0.0000000000,  0.0000000000];
%!       [-0.048383776468198,  0.4970941025,  0.4460590584];
%!       [-0.177596771314338, -0.3275791376,  0.0465651163];
%!       [ 0.266339657880378,  0.1352484276, -0.2302734105];
%!       [-0.245935764451348,  0.0434727462,  0.2546303137];
%!       [ 0.146884054700421, -0.1654838046, -0.1733614634];
%!       [-0.014224472826781,  0.2051040386,  0.0415716780];
%!       [-0.103110398228686, -0.1634199694,  0.0844338303]];
%!
%! Yt = [[-Inf,          -Inf,          -Inf        ];
%!       [ 0.4980703596,  0.1459181380, -0.38133585 ];
%!       [-0.3085176252,  0.1478631434,  0.36766288 ];
%!       [ 0.1173132861, -0.2591285105, -0.18641422 ];
%!       [ 0.0556711673,  0.2490154242, -0.00586808 ];
%!       [-0.1712143068, -0.1538382565,  0.14660019 ];
%!       [ 0.2054642960,  0.0210736280, -0.20265448 ];
%!       [-0.1604111925,  0.0985727987,  0.17167666 ]];
%!
%! J = besselj (n,z);
%! Y = bessely (n,z);
%! assert (Jt(:,1), J(:,1), 0.5e-10);
%! assert (Yt(:,1), Y(:,1), 0.5e-10);
%! assert (Jt(:,2:3), J(:,2:3), 0.5e-10);

Table 9.2 - J and Y for integer orders 3-9.

%!test
%! n = (3:9);
%! z = (0:2:20).';
%!
%! Jt = [[ 0.0000e+00, 0.0000e+00, 0.0000e+00, 0.0000e+00, 0.0000e+00, 0.0000e+00, 0.0000e+00];
%!       [ 1.2894e-01, 3.3996e-02, 7.0396e-03, 1.2024e-03, 1.7494e-04, 2.2180e-05, 2.4923e-06];
%!       [ 4.3017e-01, 2.8113e-01, 1.3209e-01, 4.9088e-02, 1.5176e-02, 4.0287e-03, 9.3860e-04];
%!       [ 1.1477e-01, 3.5764e-01, 3.6209e-01, 2.4584e-01, 1.2959e-01, 5.6532e-02, 2.1165e-02];
%!       [-2.9113e-01,-1.0536e-01, 1.8577e-01, 3.3758e-01, 3.2059e-01, 2.2345e-01, 1.2632e-01];
%!       [ 5.8379e-02,-2.1960e-01,-2.3406e-01,-1.4459e-02, 2.1671e-01, 3.1785e-01, 2.9186e-01];
%!       [ 1.9514e-01, 1.8250e-01,-7.3471e-02,-2.4372e-01,-1.7025e-01, 4.5095e-02, 2.3038e-01];
%!       [-1.7681e-01, 7.6244e-02, 2.2038e-01, 8.1168e-02,-1.5080e-01,-2.3197e-01,-1.1431e-01];
%!       [-4.3847e-02,-2.0264e-01,-5.7473e-02, 1.6672e-01, 1.8251e-01,-7.0211e-03,-1.8953e-01];
%!       [ 1.8632e-01, 6.9640e-02,-1.5537e-01,-1.5596e-01, 5.1399e-02, 1.9593e-01, 1.2276e-01];
%!       [-9.8901e-02, 1.3067e-01, 1.5117e-01,-5.5086e-02,-1.8422e-01,-7.3869e-02, 1.2513e-01]];
%!
%! Yt = [[       -Inf,       -Inf,       -Inf,       -Inf,       -Inf,       -Inf,       -Inf];
%!       [-1.1278e+00,-2.7659e+00,-9.9360e+00,-4.6914e+01,-2.7155e+02,-1.8539e+03,-1.4560e+04];
%!       [-1.8202e-01,-4.8894e-01,-7.9585e-01,-1.5007e+00,-3.7062e+00,-1.1471e+01,-4.2178e+01];
%!       [ 3.2825e-01, 9.8391e-02,-1.9706e-01,-4.2683e-01,-6.5659e-01,-1.1052e+00,-2.2907e+00];
%!       [ 2.6542e-02, 2.8294e-01, 2.5640e-01, 3.7558e-02,-2.0006e-01,-3.8767e-01,-5.7528e-01];
%!       [-2.5136e-01,-1.4495e-01, 1.3540e-01, 2.8035e-01, 2.0102e-01, 1.0755e-03,-1.9930e-01];
%!       [ 1.2901e-01,-1.5122e-01,-2.2982e-01,-4.0297e-02, 1.8952e-01, 2.6140e-01, 1.5902e-01];
%!       [ 1.2350e-01, 2.0393e-01,-6.9717e-03,-2.0891e-01,-1.7209e-01, 3.6816e-02, 2.1417e-01];
%!       [-1.9637e-01,-7.3222e-05, 1.9633e-01, 1.2278e-01,-1.0425e-01,-2.1399e-01,-1.0975e-01];
%!       [ 3.3724e-02,-1.7722e-01,-1.1249e-01, 1.1472e-01, 1.8897e-01, 3.2253e-02,-1.6030e-01];
%!       [ 1.4967e-01, 1.2409e-01,-1.0004e-01,-1.7411e-01,-4.4312e-03, 1.7101e-01, 1.4124e-01]];
%!
%! n = (3:9);
%! z = (0:2:20).';
%! J = besselj (n,z);
%! Y = bessely (n,z);
%!
%! assert (J(1,:), zeros (1, columns (J)));
%! assert (J(2:end,:), Jt(2:end,:), -5e-5);
%! assert (Yt(1,:), Y(1,:));
%! assert (Y(2:end,:), Yt(2:end,:), -5e-5);

Table 9.4 - J and Y for various integer orders and arguments.

%!test
%! Jt = [[ 7.651976866e-01,   2.238907791e-01,  -1.775967713e-01,  -2.459357645e-01,  5.581232767e-02,  1.998585030e-02];
%!       [ 2.497577302e-04,   7.039629756e-03,   2.611405461e-01,  -2.340615282e-01, -8.140024770e-02, -7.419573696e-02];
%!       [ 2.630615124e-10,   2.515386283e-07,   1.467802647e-03,   2.074861066e-01, -1.138478491e-01, -5.473217694e-02];
%!       [ 2.297531532e-17,   7.183016356e-13,   4.796743278e-07,   4.507973144e-03, -1.082255990e-01,  1.519812122e-02];
%!       [ 3.873503009e-25,   3.918972805e-19,   2.770330052e-11,   1.151336925e-05, -1.167043528e-01,  6.221745850e-02];
%!       [ 3.482869794e-42,   3.650256266e-33,   2.671177278e-21,   1.551096078e-12,  4.843425725e-02,  8.146012958e-02];
%!       [ 1.107915851e-60,   1.196077458e-48,   8.702241617e-33,   6.030895312e-21, -1.381762812e-01,  7.270175482e-02];
%!       [ 2.906004948e-80,   3.224095839e-65,   2.294247616e-45,   1.784513608e-30,  1.214090219e-01, -3.869833973e-02];
%!       [ 8.431828790e-189,  1.060953112e-158,  6.267789396e-119,  6.597316064e-89,  1.115927368e-21,  9.636667330e-02]];
%!
%! Yt = [[ 8.825696420e-02,   5.103756726e-01,  -3.085176252e-01,   5.567116730e-02, -9.806499547e-02, -7.724431337e-02]
%!       [-2.604058666e+02,  -9.935989128e+00,  -4.536948225e-01,   1.354030477e-01, -7.854841391e-02, -2.948019628e-02]
%!       [-1.216180143e+08,  -1.291845422e+05,  -2.512911010e+01,  -3.598141522e-01,  5.723897182e-03,  5.833157424e-02]
%!       [-9.256973276e+14,  -2.981023646e+10,  -4.694049564e+04,  -6.364745877e+00,  4.041280205e-02,  7.879068695e-02]
%!       [-4.113970315e+22,  -4.081651389e+16,  -5.933965297e+08,  -1.597483848e+03,  1.644263395e-02,  5.124797308e-02]
%!       [-3.048128783e+39,  -2.913223848e+30,  -4.028568418e+18,  -7.256142316e+09, -1.164572349e-01,  6.138839212e-03]
%!       [-7.184874797e+57,  -6.661541235e+45,  -9.216816571e+29,  -1.362803297e+18, -4.530801120e-02,  4.074685217e-02]
%!       [-2.191142813e+77,  -1.976150576e+62,  -2.788837017e+42,  -3.641066502e+27, -2.103165546e-01,  7.650526394e-02]
%!       [-3.775287810e+185, -3.000826049e+155, -5.084863915e+115, -4.849148271e+85, -3.293800188e+18, -1.669214114e-01]];
%!
%! n = [(0:5:20).';30;40;50;100];
%! z = [1,2,5,10,50,100];
%! J = besselj (n.', z.').';
%! Y = bessely (n.', z.').';
%! assert (J, Jt, -1e-9);
%! assert (Y, Yt, -1e-9);

Table 9.8 - I and K for integer orders 0, 1, 2.

%!test
%! n  = 0:2;
%! z1 = [0.1;2.5;5.0];
%! z2 = [7.5;10.0;15.0;20.0];
%! rtbl = [[ 0.9071009258   0.0452984468   0.1251041992   2.6823261023  10.890182683    1.995039646  ];
%!         [ 0.2700464416   0.2065846495   0.2042345837   0.7595486903   0.9001744239   0.759126289  ];
%!         [ 0.1835408126   0.1639722669   0.7002245988   0.5478075643   0.6002738588   0.132723593  ];
%!         [ 0.1483158301   0.1380412115   0.111504840    0.4505236991   0.4796689336   0.57843541   ];
%!         [ 0.1278333372   0.1212626814   0.103580801    0.3916319344   0.4107665704   0.47378525   ];
%!         [ 0.1038995314   0.1003741751   0.090516308    0.3210023535   0.3315348950   0.36520701   ];
%!         [ 0.0897803119   0.0875062222   0.081029690    0.2785448768   0.2854254970   0.30708743   ]];
%!
%! tbl = [besseli(n,z1,1), besselk(n,z1,1)];
%! tbl(:,3) = tbl(:,3) .* (exp (z1) .* z1.^(-2));
%! tbl(:,6) = tbl(:,6) .* (exp (-z1) .* z1.^(2));
%! tbl = [tbl;[besseli(n,z2,1),besselk(n,z2,1)]];
%!
%! assert (tbl, rtbl, -2e-8);

Table 9.9 - I and K for orders 3-9.

%!test
%! It = [[  0.0000e+00  0.0000e+00  0.0000e+00  0.0000e+00  0.0000e+00  0.0000e+00  0.0000e+00];
%!       [  2.8791e-02  6.8654e-03  1.3298e-03  2.1656e-04  3.0402e-05  3.7487e-06  4.1199e-07];
%!       [  6.1124e-02  2.5940e-02  9.2443e-03  2.8291e-03  7.5698e-04  1.7968e-04  3.8284e-05];
%!       [  7.4736e-02  4.1238e-02  1.9752e-02  8.3181e-03  3.1156e-03  1.0484e-03  3.1978e-04];
%!       [  7.9194e-02  5.0500e-02  2.8694e-02  1.4633e-02  6.7449e-03  2.8292e-03  1.0866e-03];
%!       [  7.9830e-02  5.5683e-02  3.5284e-02  2.0398e-02  1.0806e-02  5.2694e-03  2.3753e-03];
%!       [  7.8848e-02  5.8425e-02  3.9898e-02  2.5176e-02  1.4722e-02  8.0010e-03  4.0537e-03];
%!       [  7.7183e-02  5.9723e-02  4.3056e-02  2.8969e-02  1.8225e-02  1.0744e-02  5.9469e-03];
%!       [  7.5256e-02  6.0155e-02  4.5179e-02  3.1918e-02  2.1240e-02  1.3333e-02  7.9071e-03];
%!       [  7.3263e-02  6.0059e-02  4.6571e-02  3.4186e-02  2.3780e-02  1.5691e-02  9.8324e-03];
%!       [  7.1300e-02  5.9640e-02  4.7444e-02  3.5917e-02  2.5894e-02  1.7792e-02  1.1661e-02]];
%!
%! Kt = [[ Inf         Inf         Inf         Inf         Inf         Inf         Inf];
%!      [  4.7836e+00  1.6226e+01  6.9687e+01  3.6466e+02  2.2576e+03  1.6168e+04  1.3160e+05];
%!      [  1.6317e+00  3.3976e+00  8.4268e+00  2.4465e+01  8.1821e+01  3.1084e+02  1.3252e+03];
%!      [  9.9723e-01  1.6798e+00  3.2370e+00  7.0748e+00  1.7387e+01  4.7644e+01  1.4444e+02];
%!      [  7.3935e-01  1.1069e+00  1.8463e+00  3.4148e+00  6.9684e+00  1.5610e+01  3.8188e+01];
%!      [  6.0028e-01  8.3395e-01  1.2674e+00  2.1014e+00  3.7891e+00  7.4062e+00  1.5639e+01];
%!      [  5.1294e-01  6.7680e-01  9.6415e-01  1.4803e+00  2.4444e+00  4.3321e+00  8.2205e+00];
%!      [  4.5266e-01  5.7519e-01  7.8133e-01  1.1333e+00  1.7527e+00  2.8860e+00  5.0510e+00];
%!      [  4.0829e-01  5.0414e-01  6.6036e-01  9.1686e-01  1.3480e+00  2.0964e+00  3.4444e+00];
%!      [  3.7411e-01  4.5162e-01  5.7483e-01  7.7097e-01  1.0888e+00  1.6178e+00  2.5269e+00];
%!      [  3.4684e-01  4.1114e-01  5.1130e-01  6.6679e-01  9.1137e-01  1.3048e+00  1.9552e+00]];
%!
%! n = (3:9);
%! z = (0:2:20).';
%! I = besseli (n,z,1);
%! K = besselk (n,z,1);
%!
%! assert (abs (I(1,:)), zeros (1, columns (I)));
%! assert (I(2:end,:), It(2:end,:), -5e-5);
%! assert (Kt(1,:), K(1,:));
%! assert (K(2:end,:), Kt(2:end,:), -5e-5);

Table 9.11 - I and K for various integer orders and arguments.

%!test
%! It = [[   1.266065878e+00    2.279585302e+00    2.723987182e+01    2.815716628e+03     2.93255378e+20     1.07375171e+42 ];
%!       [   2.714631560e-04    9.825679323e-03    2.157974547e+00    7.771882864e+02     2.27854831e+20     9.47009387e+41 ];
%!       [   2.752948040e-10    3.016963879e-07    4.580044419e-03    2.189170616e+01     1.07159716e+20     6.49897552e+41 ];
%!       [   2.370463051e-17    8.139432531e-13    1.047977675e-06    1.043714907e-01     3.07376455e+19     3.47368638e+41 ];
%!       [   3.966835986e-25    4.310560576e-19    5.024239358e-11    1.250799736e-04     5.44200840e+18     1.44834613e+41 ];
%!       [   3.539500588e-42    3.893519664e-33    3.997844971e-21    7.787569783e-12     4.27499365e+16     1.20615487e+40 ];
%!       [   1.121509741e-60    1.255869192e-48    1.180426980e-32    2.042123274e-20     6.00717897e+13     3.84170550e+38 ];
%!       [   2.934635309e-80    3.353042830e-65    2.931469647e-45    4.756894561e-30     1.76508024e+10     4.82195809e+36 ];
%!       [   8.473674008e-189   1.082171475e-158   7.093551489e-119   1.082344202e-88     2.72788795e-16     4.64153494e+21 ]];
%!
%! Kt = [[   4.210244382e-01    1.138938727e-01    3.691098334e-03    1.778006232e-05     3.41016774e-23     4.65662823e-45 ];
%!       [   3.609605896e+02    9.431049101e+00    3.270627371e-02    5.754184999e-05     4.36718224e-23     5.27325611e-45 ];
%!       [   1.807132899e+08    1.624824040e+05    9.758562829e+00    1.614255300e-03     9.15098819e-23     7.65542797e-45 ];
%!       [   1.403066801e+15    4.059213332e+10    3.016976630e+04    2.656563849e-01     3.11621117e-22     1.42348325e-44 ];
%!       [   6.294369360e+22    5.770856853e+16    4.827000521e+08    1.787442782e+02     1.70614838e-21     3.38520541e-44 ];
%!       [   4.706145527e+39    4.271125755e+30    4.112132063e+18    2.030247813e+09     2.00581681e-19     3.97060205e-43 ];
%!       [   1.114220651e+58    9.940839886e+45    1.050756722e+30    5.938224681e+17     1.29986971e-16     1.20842080e-41 ];
%!       [   3.406896854e+77    2.979981740e+62    3.394322243e+42    2.061373775e+27     4.00601349e-13     9.27452265e-40 ];
%!       [   5.900333184e+185   4.619415978e+155   7.039860193e+115   4.596674084e+85     1.63940352e+13     7.61712963e-25 ]];
%!
%! n = [(0:5:20).';30;40;50;100];
%! z = [1,2,5,10,50,100];
%! I = besseli (n.', z.').';
%! K = besselk (n.', z.').';
%! assert (I, It, -5e-9);
%! assert (K, Kt, -5e-9);

The next section checks that negative integer orders and positive
integer orders are appropriately related.

%!test
%! n = (0:2:20);
%! assert (besselj (n,1), besselj (-n,1), 1e-8);
%! assert (-besselj (n+1,1), besselj (-n-1,1), 1e-8);

besseli (n,z) = besseli (-n,z);

%!test
%! n = (0:2:20);
%! assert (besseli (n,1), besseli (-n,1), 1e-8);

Table 10.1 - j and y for integer orders 0, 1, 2.
Compare against excerpts of Table 10.1, Abramowitz and Stegun.

%!test
%! n = (0:2);
%! z = [0.1;(2.5:2.5:10.0).'];
%!
%! jt = [[ 9.9833417e-01  3.33000119e-02  6.6619061e-04 ];
%!       [ 2.3938886e-01  4.16212989e-01  2.6006673e-01 ];
%!       [-1.9178485e-01 -9.50894081e-02  1.3473121e-01 ];
%!       [    1.2507e-01     -2.9542e-02    -1.3688e-01 ];
%!       [   -5.4402e-02      7.8467e-02     7.7942e-02 ]];
%!
%! yt = [[-9.9500417e+00  -1.0049875e+02 -3.0050125e+03 ];
%!       [ 3.2045745e-01  -1.1120588e-01 -4.5390450e-01 ];
%!       [-5.6732437e-02   1.8043837e-01  1.6499546e-01 ];
%!       [   -4.6218e-02     -1.3123e-01    -6.2736e-03 ];
%!       [    8.3907e-02      6.2793e-02    -6.5069e-02 ]];
%!
%! j = sqrt ((pi/2)./z) .* besselj (n+1/2,z);
%! y = sqrt ((pi/2)./z) .* bessely (n+1/2,z);
%! assert (jt, j, -5e-5);
%! assert (yt, y, -5e-5);

Table 10.2 - j and y for orders 3-8.
Compare against excerpts of Table 10.2, Abramowitzh and Stegun.

 Important note: In A&S, y_4(0.1) = -1.0507e+7, but Octave returns
 y_4(0.1) = -1.0508e+07 (-10507503.75).  If I compute the same term using
 a series, the difference is in the eighth significant digit so I left
 the Octave results in place.

%!test
%! n = (3:8);
%! z = (0:2.5:10).';  z(1) = 0.1;
%!
%! jt = [[ 9.5185e-06  1.0577e-07  9.6163e-10  7.3975e-12  4.9319e-14  2.9012e-16];
%!       [ 1.0392e-01  3.0911e-02  7.3576e-03  1.4630e-03  2.5009e-04  3.7516e-05];
%!       [ 2.2982e-01  1.8702e-01  1.0681e-01  4.7967e-02  1.7903e-02  5.7414e-03];
%!       [-6.1713e-02  7.9285e-02  1.5685e-01  1.5077e-01  1.0448e-01  5.8188e-02];
%!       [-3.9496e-02 -1.0559e-01 -5.5535e-02  4.4501e-02  1.1339e-01  1.2558e-01]];
%!
%! yt = [[-1.5015e+05 -1.0508e+07 -9.4553e+08 -1.0400e+11 -1.3519e+13 -2.0277e+15];
%!       [-7.9660e-01 -1.7766e+00 -5.5991e+00 -2.2859e+01 -1.1327e+02 -6.5676e+02];
%!       [-1.5443e-02 -1.8662e-01 -3.2047e-01 -5.1841e-01 -1.0274e+00 -2.5638e+00];
%!       [ 1.2705e-01  1.2485e-01  2.2774e-02 -9.1449e-02 -1.8129e-01 -2.7112e-01];
%!       [-9.5327e-02 -1.6599e-03  9.3834e-02  1.0488e-01  4.2506e-02 -4.1117e-02]];
%!
%! j = sqrt ((pi/2)./z) .* besselj (n+1/2,z);
%! y = sqrt ((pi/2)./z) .* bessely (n+1/2,z);
%!
%! assert (jt, j, -5e-5);
%! assert (yt, y, -5e-5);

Table 10.4 - j and y for various integer orders and arguments.

%!test
%! jt = [[ 8.414709848e-01    4.546487134e-01   -1.917848549e-01   -5.440211109e-02   -5.247497074e-03   -5.063656411e-03];
%!       [ 9.256115861e-05    2.635169770e-03    1.068111615e-01   -5.553451162e-02   -2.004830056e-02   -9.290148935e-03];
%!       [ 7.116552640e-11    6.825300865e-08    4.073442442e-04    6.460515449e-02   -1.503922146e-02   -1.956578597e-04];
%!       [ 5.132686115e-18    1.606982166e-13    1.084280182e-07    1.063542715e-03   -1.129084539e-02    7.877261748e-03];
%!       [ 7.537795722e-26    7.632641101e-20    5.427726761e-12    2.308371961e-06   -1.578502990e-02    1.010767128e-02];
%!       [ 5.566831267e-43    5.836617888e-34    4.282730217e-22    2.512057385e-13   -1.494673454e-03    8.700628514e-03];
%!       [ 1.538210374e-61    1.660978779e-49    1.210347583e-33    8.435671634e-22   -2.606336952e-02    1.043410851e-02];
%!       [ 3.615274717e-81    4.011575290e-66    2.857479350e-46    2.230696023e-31    1.882910737e-02    5.797140882e-04];
%!       [7.444727742e-190   9.367832591e-160   5.535650303e-120    5.832040182e-90    1.019012263e-22    1.088047701e-02]];
%!
%! yt = [[ -5.403023059e-01    2.080734183e-01   -5.673243709e-02    8.390715291e-02   -1.929932057e-02   -8.623188723e-03]
%!       [ -9.994403434e+02   -1.859144531e+01   -3.204650467e-01    9.383354168e-02   -6.971131965e-04    3.720678486e-03]
%!       [ -6.722150083e+08   -3.554147201e+05   -2.665611441e+01   -1.724536721e-01    1.352468751e-02    1.002577737e-02]
%!       [ -6.298007233e+15   -1.012182944e+11   -6.288146513e+04   -3.992071745e+00    1.712319725e-02    6.258641510e-03]
%!       [ -3.239592219e+23   -1.605436493e+17   -9.267951403e+08   -1.211210605e+03    1.375953130e-02    5.631729379e-05]
%!       [ -2.946428547e+40   -1.407393871e+31   -7.760717570e+18   -6.908318646e+09   -2.241226812e-02   -5.412929349e-03]
%!       [ -8.028450851e+58   -3.720929322e+46   -2.055758716e+30   -1.510304919e+18    4.978797221e-05   -7.048420407e-04]
%!       [ -2.739192285e+78   -1.235021944e+63   -6.964109188e+42   -4.528227272e+27   -4.190000150e-02    1.074782297e-02]
%!       [-6.683079463e+186  -2.655955830e+156  -1.799713983e+116   -8.573226309e+85   -1.125692891e+18   -2.298385049e-02]];
%!
%! n = [(0:5:20).';30;40;50;100];
%! z = [1,2,5,10,50,100];
%! j = sqrt ((pi/2)./z) .* besselj ((n+1/2).', z.').';
%! y = sqrt ((pi/2)./z) .* bessely ((n+1/2).', z.').';
%! assert (j, jt, -1e-9);
%! assert (y, yt, -1e-9);
*/

OCTAVE_END_NAMESPACE(octave)
