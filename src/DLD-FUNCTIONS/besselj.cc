/*

Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "lo-specfun.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

enum bessel_type
{
  BESSEL_J,
  BESSEL_Y,
  BESSEL_I,
  BESSEL_K,
  BESSEL_H1,
  BESSEL_H2
};

#define DO_BESSEL(type, alpha, x, scaled, ierr, result) \
  do \
    { \
      switch (type) \
        { \
          case BESSEL_J: \
            result = besselj (alpha, x, scaled, ierr); \
            break; \
 \
          case BESSEL_Y: \
            result = bessely (alpha, x, scaled, ierr); \
            break; \
 \
          case BESSEL_I: \
            result = besseli (alpha, x, scaled, ierr); \
            break; \
 \
          case BESSEL_K: \
            result = besselk (alpha, x, scaled, ierr); \
            break; \
 \
          case BESSEL_H1: \
            result = besselh1 (alpha, x, scaled, ierr); \
            break; \
 \
          case BESSEL_H2: \
            result = besselh2 (alpha, x, scaled, ierr); \
            break; \
 \
          default: \
            break; \
        } \
    } \
  while (0)

static void
gripe_bessel_arg (const char *fn, const char *arg)
{
  error ("%s: expecting scalar or matrix as %s argument", fn, arg);
}

octave_value_list
do_bessel (enum bessel_type type, const char *fn,
           const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      bool scaled = (nargin == 3);

      octave_value alpha_arg = args(0);
      octave_value x_arg = args(1);

      if (alpha_arg.is_single_type () || x_arg.is_single_type ())
        {
          if (alpha_arg.is_scalar_type ())
            {
              float alpha = args(0).float_value ();

              if (! error_state)
                {
                  if (x_arg.is_scalar_type ())
                    {
                      FloatComplex x = x_arg.float_complex_value ();

                      if (! error_state)
                        {
                          octave_idx_type ierr;
                          octave_value result;

                          DO_BESSEL (type, alpha, x, scaled, ierr, result);

                          if (nargout > 1)
                            retval(1) = static_cast<float> (ierr);

                          retval(0) = result;
                        }
                      else
                        gripe_bessel_arg (fn, "second");
                    }
                  else
                    {
                      FloatComplexNDArray x = x_arg.float_complex_array_value ();

                      if (! error_state)
                        {
                          Array<octave_idx_type> ierr;
                          octave_value result;

                          DO_BESSEL (type, alpha, x, scaled, ierr, result);

                          if (nargout > 1)
                            retval(1) = NDArray (ierr);

                          retval(0) = result;
                        }
                      else
                        gripe_bessel_arg (fn, "second");
                    }
                }
              else
                gripe_bessel_arg (fn, "first");
            }
          else
            {
              dim_vector dv0 = args(0).dims ();
              dim_vector dv1 = args(1).dims ();

              bool args0_is_row_vector = (dv0 (1) == dv0.numel ());
              bool args1_is_col_vector = (dv1 (0) == dv1.numel ());

              if (args0_is_row_vector && args1_is_col_vector)
                {
                  FloatRowVector ralpha = args(0).float_row_vector_value ();

                  if (! error_state)
                    {
                      FloatComplexColumnVector cx = 
                        x_arg.float_complex_column_vector_value ();

                      if (! error_state)
                        {
                          Array2<octave_idx_type> ierr;
                          octave_value result;

                          DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

                          if (nargout > 1)
                            retval(1) = NDArray (ierr);

                          retval(0) = result;
                        }
                      else
                        gripe_bessel_arg (fn, "second");
                    }
                  else
                    gripe_bessel_arg (fn, "first");
                }
              else
                {
                  FloatNDArray alpha = args(0).float_array_value ();

                  if (! error_state)
                    {
                      if (x_arg.is_scalar_type ())
                        {
                          FloatComplex x = x_arg.float_complex_value ();

                          if (! error_state)
                            {
                              Array<octave_idx_type> ierr;
                              octave_value result;

                              DO_BESSEL (type, alpha, x, scaled, ierr, result);

                              if (nargout > 1)
                                retval(1) = NDArray (ierr);

                              retval(0) = result;
                            }
                          else
                            gripe_bessel_arg (fn, "second");
                        }
                      else
                        {
                          FloatComplexNDArray x = x_arg.float_complex_array_value ();

                          if (! error_state)
                            {
                              Array<octave_idx_type> ierr;
                              octave_value result;
                          
                              DO_BESSEL (type, alpha, x, scaled, ierr, result);
                          
                              if (nargout > 1)
                                retval(1) = NDArray (ierr);

                              retval(0) = result;
                            }
                          else
                            gripe_bessel_arg (fn, "second");
                        }
                    }
                  else
                    gripe_bessel_arg (fn, "first");
                }
            }
        }
      else
        {
          if (alpha_arg.is_scalar_type ())
            {
              double alpha = args(0).double_value ();

              if (! error_state)
                {
                  if (x_arg.is_scalar_type ())
                    {
                      Complex x = x_arg.complex_value ();

                      if (! error_state)
                        {
                          octave_idx_type ierr;
                          octave_value result;

                          DO_BESSEL (type, alpha, x, scaled, ierr, result);

                          if (nargout > 1)
                            retval(1) = static_cast<double> (ierr);

                          retval(0) = result;
                        }
                      else
                        gripe_bessel_arg (fn, "second");
                    }
                  else
                    {
                      ComplexNDArray x = x_arg.complex_array_value ();

                      if (! error_state)
                        {
                          Array<octave_idx_type> ierr;
                          octave_value result;

                          DO_BESSEL (type, alpha, x, scaled, ierr, result);

                          if (nargout > 1)
                            retval(1) = NDArray (ierr);

                          retval(0) = result;
                        }
                      else
                        gripe_bessel_arg (fn, "second");
                    }
                }
              else
                gripe_bessel_arg (fn, "first");
            }
          else
            {
              dim_vector dv0 = args(0).dims ();
              dim_vector dv1 = args(1).dims ();

              bool args0_is_row_vector = (dv0 (1) == dv0.numel ());
              bool args1_is_col_vector = (dv1 (0) == dv1.numel ());

              if (args0_is_row_vector && args1_is_col_vector)
                {
                  RowVector ralpha = args(0).row_vector_value ();

                  if (! error_state)
                    {
                      ComplexColumnVector cx = 
                        x_arg.complex_column_vector_value ();

                      if (! error_state)
                        {
                          Array2<octave_idx_type> ierr;
                          octave_value result;

                          DO_BESSEL (type, ralpha, cx, scaled, ierr, result);

                          if (nargout > 1)
                            retval(1) = NDArray (ierr);

                          retval(0) = result;
                        }
                      else
                        gripe_bessel_arg (fn, "second");
                    }
                  else
                    gripe_bessel_arg (fn, "first");
                }
              else
                {
                  NDArray alpha = args(0).array_value ();

                  if (! error_state)
                    {
                      if (x_arg.is_scalar_type ())
                        {
                          Complex x = x_arg.complex_value ();

                          if (! error_state)
                            {
                              Array<octave_idx_type> ierr;
                              octave_value result;

                              DO_BESSEL (type, alpha, x, scaled, ierr, result);

                              if (nargout > 1)
                                retval(1) = NDArray (ierr);

                              retval(0) = result;
                            }
                          else
                            gripe_bessel_arg (fn, "second");
                        }
                      else
                        {
                          ComplexNDArray x = x_arg.complex_array_value ();

                          if (! error_state)
                            {
                              Array<octave_idx_type> ierr;
                              octave_value result;
                          
                              DO_BESSEL (type, alpha, x, scaled, ierr, result);
                          
                              if (nargout > 1)
                                retval(1) = NDArray (ierr);

                              retval(0) = result;
                            }
                          else
                            gripe_bessel_arg (fn, "second");
                        }
                    }
                  else
                    gripe_bessel_arg (fn, "first");
                }
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (besselj, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{j}, @var{ierr}] =} besselj (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{ierr}] =} bessely (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{i}, @var{ierr}] =} besseli (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{k}, @var{ierr}] =} besselk (@var{alpha}, @var{x}, @var{opt})\n\
@deftypefnx {Loadable Function} {[@var{h}, @var{ierr}] =} besselh (@var{alpha}, @var{k}, @var{x}, @var{opt})\n\
Compute Bessel or Hankel functions of various kinds:\n\
\n\
@table @code\n\
@item besselj\n\
Bessel functions of the first kind.  If the argument @var{opt} is supplied, \n\
the result is multiplied by @code{exp(-abs(imag(x)))}.\n\
@item bessely\n\
Bessel functions of the second kind.  If the argument @var{opt} is supplied,\n\
the result is multiplied by @code{exp(-abs(imag(x)))}.\n\
@item besseli\n\
Modified Bessel functions of the first kind.  If the argument @var{opt} is supplied,\n\
the result is multiplied by @code{exp(-abs(real(x)))}.\n\
@item besselk\n\
Modified Bessel functions of the second kind.  If the argument @var{opt} is supplied,\n\
the result is multiplied by @code{exp(x)}.\n\
@item besselh\n\
Compute Hankel functions of the first (@var{k} = 1) or second (@var{k}\n\
= 2) kind.  If the argument @var{opt} is supplied, the result is multiplied by\n\
@code{exp (-I*@var{x})} for @var{k} = 1 or @code{exp (I*@var{x})} for\n\
@var{k} = 2.\n\
@end table\n\
\n\
If @var{alpha} is a scalar, the result is the same size as @var{x}.\n\
If @var{x} is a scalar, the result is the same size as @var{alpha}.\n\
If @var{alpha} is a row vector and @var{x} is a column vector, the\n\
result is a matrix with @code{length (@var{x})} rows and\n\
@code{length (@var{alpha})} columns.  Otherwise, @var{alpha} and\n\
@var{x} must conform and the result will be the same size.\n\
\n\
The value of @var{alpha} must be real.  The value of @var{x} may be\n\
complex.\n\
\n\
If requested, @var{ierr} contains the following status information\n\
and is the same size as the result.\n\
\n\
@enumerate 0\n\
@item\n\
Normal return.\n\
@item\n\
Input error, return @code{NaN}.\n\
@item\n\
Overflow, return @code{Inf}.\n\
@item\n\
Loss of significance by argument reduction results in less than\n\
half of machine accuracy.\n\
@item\n\
Complete loss of significance by argument reduction, return @code{NaN}.\n\
@item\n\
Error---no computation, algorithm termination condition not met,\n\
return @code{NaN}.\n\
@end enumerate\n\
@end deftypefn")
{
  return do_bessel (BESSEL_J, "besselj", args, nargout);
}

DEFUN_DLD (bessely, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{y}, @var{ierr}] =} bessely (@var{alpha}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  return do_bessel (BESSEL_Y, "bessely", args, nargout);
}

DEFUN_DLD (besseli, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{i}, @var{ierr}] =} besseli (@var{alpha}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  return do_bessel (BESSEL_I, "besseli", args, nargout);
}

DEFUN_DLD (besselk, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{k}, @var{ierr}] =} besselk (@var{alpha}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  return do_bessel (BESSEL_K, "besselk", args, nargout);
}

DEFUN_DLD (besselh, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{h}, @var{ierr}] =} besselh (@var{alpha}, @var{k}, @var{x}, @var{opt})\n\
See besselj.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      retval = do_bessel (BESSEL_H1, "besselh", args, nargout);
    }
  else if (nargin == 3 || nargin == 4)
    {
      octave_idx_type kind = args(1).int_value ();

      if (! error_state)
        {
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
            error ("besselh: expecting K = 1 or 2");
        }
      else
        error ("besselh: invalid value of K");
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (airy, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{a}, @var{ierr}] =} airy (@var{k}, @var{z}, @var{opt})\n\
Compute Airy functions of the first and second kind, and their\n\
derivatives.\n\
\n\
@example\n\
@group\n\
 K   Function   Scale factor (if 'opt' is supplied)\n\
---  --------   ---------------------------------------\n\
 0   Ai (Z)     exp ((2/3) * Z * sqrt (Z))\n\
 1   dAi(Z)/dZ  exp ((2/3) * Z * sqrt (Z))\n\
 2   Bi (Z)     exp (-abs (real ((2/3) * Z *sqrt (Z))))\n\
 3   dBi(Z)/dZ  exp (-abs (real ((2/3) * Z *sqrt (Z))))\n\
@end group\n\
@end example\n\
\n\
The function call @code{airy (@var{z})} is equivalent to\n\
@code{airy (0, @var{z})}.\n\
\n\
The result is the same size as @var{z}.\n\
\n\
If requested, @var{ierr} contains the following status information and\n\
is the same size as the result.\n\
\n\
@enumerate 0\n\
@item\n\
Normal return.\n\
@item\n\
Input error, return @code{NaN}.\n\
@item\n\
Overflow, return @code{Inf}.\n\
@item\n\
Loss of significance by argument reduction results in less than half\n\
 of machine accuracy.\n\
@item\n\
Complete loss of significance by argument reduction, return @code{NaN}.\n\
@item\n\
Error---no computation, algorithm termination condition not met,\n\
return @code{NaN}.\n\
@end enumerate\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      bool scale = (nargin == 3);

      int kind = 0;

      if (nargin > 1)
        {
          kind = args(0).int_value ();

          if (! error_state)
            {
              if (kind < 0 || kind > 3)
                error ("airy: expecting K = 0, 1, 2, or 3");
            }         
          else
            error ("airy: expecting integer value for K");
        }

      if (! error_state)
        {
          int idx = nargin == 1 ? 0 : 1;

          if (args (idx).is_single_type ())
            {
              FloatComplexNDArray z = args(idx).float_complex_array_value ();

              if (! error_state)
                {
                  Array<octave_idx_type> ierr;
                  octave_value result;

                  if (kind > 1)
                    result = biry (z, kind == 3, scale, ierr);
                  else
                    result = airy (z, kind == 1, scale, ierr);

                  if (nargout > 1)
                    retval(1) = NDArray (ierr);

                  retval(0) = result;
                }
              else
                error ("airy: expecting complex matrix for Z");
            }
          else
            {
              ComplexNDArray z = args(idx).complex_array_value ();

              if (! error_state)
                {
                  Array<octave_idx_type> ierr;
                  octave_value result;

                  if (kind > 1)
                    result = biry (z, kind == 3, scale, ierr);
                  else
                    result = airy (z, kind == 1, scale, ierr);

                  if (nargout > 1)
                    retval(1) = NDArray (ierr);

                  retval(0) = result;
                }
              else
                error ("airy: expecting complex matrix for Z");
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%! # Test values computed with GP/PARI version 2.3.3
%!
%!shared alpha, x, jx, yx, ix, kx, nix
%!
%! # Bessel functions, even order, positive and negative x
%! alpha = 2; x = 1.25;
%! jx = 0.1710911312405234823613091417;
%! yx = -1.193199310178553861283790424;
%! ix = 0.2220184483766341752692212604;
%! kx = 0.9410016167388185767085460540;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%!assert(besselj(-alpha,x), jx, 100*eps) 
%!assert(bessely(-alpha,x), yx, 100*eps)
%!assert(besseli(-alpha,x), ix, 100*eps)
%!assert(besselk(-alpha,x), kx, 100*eps) 
%!assert(besselh(-alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(-alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(-alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(-alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(-alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(-alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(-alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(-alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%! x *= -1;
%! yx = -1.193199310178553861283790424 + 0.3421822624810469647226182835*I;
%! kx = 0.9410016167388185767085460540 - 0.6974915263814386815610060884*I;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%! # Bessel functions, odd order, positive and negative x
%! alpha = 3; x = 2.5;
%! jx = 0.2166003910391135247666890035;
%! yx = -0.7560554967536709968379029772;
%! ix = 0.4743704087780355895548240179;
%! kx = 0.2682271463934492027663765197;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%!assert(besselj(-alpha,x), -jx, 100*eps) 
%!assert(bessely(-alpha,x), -yx, 100*eps)
%!assert(besseli(-alpha,x), ix, 100*eps)
%!assert(besselk(-alpha,x), kx, 100*eps) 
%!assert(besselh(-alpha,1,x), -(jx + I*yx), 100*eps)
%!assert(besselh(-alpha,2,x), -(jx - I*yx), 100*eps)
%!
%!assert(besselj(-alpha,x,1), -jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(-alpha,x,1), -yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(-alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(-alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(-alpha,1,x,1), -(jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(-alpha,2,x,1), -(jx - I*yx)*exp(I*x), 100*eps)
%!
%! x *= -1;
%! jx = -jx;
%! yx = 0.7560554967536709968379029772 - 0.4332007820782270495333780070*I;
%! ix = -ix;
%! kx = -0.2682271463934492027663765197 - 1.490278591297463775542004240*I;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%! # Bessel functions, fractional order, positive and negative x
%!
%! alpha = 3.5; x = 2.75;
%! jx = 0.1691636439842384154644784389;
%! yx = -0.8301381935499356070267953387;
%! ix = 0.3930540878794826310979363668;
%! kx = 0.2844099013460621170288192503;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%! nix = 0.2119931212254662995364461998;
%!
%!assert(besselj(-alpha,x), yx, 100*eps) 
%!assert(bessely(-alpha,x), -jx, 100*eps)
%!assert(besseli(-alpha,x), nix, 100*eps)
%!assert(besselk(-alpha,x), kx, 100*eps) 
%!assert(besselh(-alpha,1,x), -I*(jx + I*yx), 100*eps)
%!assert(besselh(-alpha,2,x), I*(jx - I*yx), 100*eps)
%!
%!assert(besselj(-alpha,x,1), yx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(-alpha,x,1), -jx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(-alpha,x,1), nix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(-alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(-alpha,1,x,1), -I*(jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(-alpha,2,x,1), I*(jx - I*yx)*exp(I*x), 100*eps)
%!
%! x *= -1;
%! jx *= -I;
%! yx = -0.8301381935499356070267953387*I;
%! ix *= -I;
%! kx = -0.9504059335995575096509874508*I;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%! # Bessel functions, even order, complex x
%!
%! alpha = 2; x = 1.25 + 3.625 * I;
%! jx = -1.299533366810794494030065917 + 4.370833116012278943267479589*I;
%! yx = -4.370357232383223896393056727 - 1.283083391453582032688834041*I;
%! ix = -0.6717801680341515541002273932 - 0.2314623443930774099910228553*I;
%! kx = -0.01108009888623253515463783379 + 0.2245218229358191588208084197*I;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%!assert(besselj(-alpha,x), jx, 100*eps) 
%!assert(bessely(-alpha,x), yx, 100*eps)
%!assert(besseli(-alpha,x), ix, 100*eps)
%!assert(besselk(-alpha,x), kx, 100*eps) 
%!assert(besselh(-alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(-alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(-alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(-alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(-alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(-alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(-alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(-alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%! # Bessel functions, odd order, complex x
%!
%! alpha = 3; x = 2.5 + 1.875 * I;
%! jx = 0.1330721523048277493333458596 + 0.5386295217249660078754395597*I;
%! yx = -0.6485072392105829901122401551 + 0.2608129289785456797046996987*I;
%! ix = -0.6182064685486998097516365709 + 0.4677561094683470065767989920*I;
%! kx = -0.1568585587733540007867882337 - 0.05185853709490846050505141321*I;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%!assert(besselj(-alpha,x), -jx, 100*eps) 
%!assert(bessely(-alpha,x), -yx, 100*eps)
%!assert(besseli(-alpha,x), ix, 100*eps)
%!assert(besselk(-alpha,x), kx, 100*eps) 
%!assert(besselh(-alpha,1,x), -(jx + I*yx), 100*eps)
%!assert(besselh(-alpha,2,x), -(jx - I*yx), 100*eps)
%!
%!assert(besselj(-alpha,x,1), -jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(-alpha,x,1), -yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(-alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(-alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(-alpha,1,x,1), -(jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(-alpha,2,x,1), -(jx - I*yx)*exp(I*x), 100*eps)
%!
%! # Bessel functions, fractional order, complex x
%!
%! alpha = 3.5; x = 1.75 + 4.125 * I;
%! jx = -3.018566131370455929707009100 - 0.7585648436793900607704057611*I;
%! yx = 0.7772278839106298215614791107 - 3.018518722313849782683792010*I;
%! ix = 0.2100873577220057189038160913 - 0.6551765604618246531254970926*I;
%! kx = 0.1757147290513239935341488069 + 0.08772348296883849205562558311*I;
%!
%!assert(besselj(alpha,x), jx, 100*eps) 
%!assert(bessely(alpha,x), yx, 100*eps)
%!assert(besseli(alpha,x), ix, 100*eps)
%!assert(besselk(alpha,x), kx, 100*eps) 
%!assert(besselh(alpha,1,x), jx + I*yx, 100*eps)
%!assert(besselh(alpha,2,x), jx - I*yx, 100*eps)
%!
%!assert(besselj(alpha,x,1), jx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(alpha,x,1), yx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(alpha,x,1), ix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(alpha,1,x,1), (jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(alpha,2,x,1), (jx - I*yx)*exp(I*x), 100*eps)
%!
%!  nix = 0.09822388691172060573913739253 - 0.7110230642207380127317227407*I;
%!
%!assert(besselj(-alpha,x), yx, 100*eps) 
%!assert(bessely(-alpha,x), -jx, 100*eps)
%!assert(besseli(-alpha,x), nix, 100*eps)
%!assert(besselk(-alpha,x), kx, 100*eps) 
%!assert(besselh(-alpha,1,x), -I*(jx + I*yx), 100*eps)
%!assert(besselh(-alpha,2,x), I*(jx - I*yx), 100*eps)
%!
%!assert(besselj(-alpha,x,1), yx*exp(-abs(imag(x))), 100*eps) 
%!assert(bessely(-alpha,x,1), -jx*exp(-abs(imag(x))), 100*eps)
%!assert(besseli(-alpha,x,1), nix*exp(-abs(real(x))), 100*eps)
%!assert(besselk(-alpha,x,1), kx*exp(x), 100*eps) 
%!assert(besselh(-alpha,1,x,1), -I*(jx + I*yx)*exp(-I*x), 100*eps)
%!assert(besselh(-alpha,2,x,1), I*(jx - I*yx)*exp(I*x), 100*eps)
*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
