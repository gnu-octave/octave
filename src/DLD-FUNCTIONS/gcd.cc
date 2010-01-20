/*

Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 David Bateman

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

#include "dNDArray.h"
#include "CNDArray.h"
#include "fNDArray.h"
#include "fCNDArray.h"
#include "lo-mappers.h"
#include "oct-locbuf.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

// FIXME -- should probably handle Inf, NaN.

static inline bool
is_integer_value (double x)
{
  return x == std::floor (x);
}

static inline bool
is_integer_value (float x)
{
  return x == std::floor (x);
}

DEFUN_DLD (gcd, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{g} =} gcd (@var{a})\n\
@deftypefnx {Loadable Function} {@var{g} =} gcd (@var{a1}, @var{a2}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{g}, @var{v1}, @dots{}] =} gcd (@var{a1}, @var{a2}, @dots{})\n\
\n\
Compute the greatest common divisor of the elements of @var{a}.  If more\n\
than one argument is given all arguments must be the same size or scalar.\n\
  In this case the greatest common divisor is calculated for each element\n\
individually.  All elements must be integers.  For example,\n\
\n\
@example\n\
@group\n\
gcd ([15, 20])\n\
    @result{}  5\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
and\n\
\n\
@example\n\
@group\n\
gcd ([15, 9], [20, 18])\n\
    @result{}  5  9\n\
@end group\n\
@end example\n\
\n\
Optional return arguments @var{v1}, etc., contain integer vectors such\n\
that,\n\
\n\
@tex\n\
$g = v_1 a_1 + v_2 a_2 + \\cdots$\n\
@end tex\n\
@ifnottex\n\
@example\n\
@var{g} = @var{v1} .* @var{a1} + @var{v2} .* @var{a2} + @dots{}\n\
@end example\n\
@end ifnottex\n\
\n\
For backward compatibility with previous versions of this function, when\n\
all arguments are scalar, a single return argument @var{v1} containing\n\
all of the values of @var{v1}, @dots{} is acceptable.\n\
@seealso{lcm, factor}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_usage ();
      return retval;
    }

  bool all_args_scalar = true;
  bool any_single = false;

  dim_vector dv(1);

  for (int i = 0; i < nargin; i++)
    {
      if (! args(i).is_scalar_type ())
        {
          if (! args(i).is_matrix_type ())
            {
              error ("gcd: invalid argument type");
              return retval;
            }

          if (all_args_scalar)
            {
              all_args_scalar = false;
              dv = args(i).dims ();
            }
          else
            {
              if (dv != args(i).dims ())
                {
                  error ("gcd: all arguments must be the same size or scalar");
                  return retval;
                }
            }
        }
      if (!any_single && args(i).is_single_type ())
        any_single = true;
    }

  if (any_single)
    {
      if (nargin == 1)
        {
          FloatNDArray gg = args(0).float_array_value ();

          int nel = dv.numel ();

          FloatNDArray v (dv);

          FloatRowVector x (3);
          FloatRowVector y (3);

          float g = std::abs (gg(0));

          if (! is_integer_value (g))
            {
              error ("gcd: all arguments must be integer");
              return retval;
            }

          v(0) = signum (gg(0));
      
          for (int k = 1; k < nel; k++)
            {
              x(0) = g;
              x(1) = 1;
              x(2) = 0;

              y(0) = std::abs (gg(k));
              y(1) = 0;
              y(2) = 1;

              if (! is_integer_value (y(0)))
                {
                  error ("gcd: all arguments must be integer");
                  return retval;
                }

              while (y(0) > 0)
                {
                  FloatRowVector r = x - y * std::floor (x(0) / y(0));
                  x = y;
                  y = r;
                }

              g = x(0);

              for (int i = 0; i < k; i++) 
                v(i) *= x(1);

              v(k) = x(2) * signum (gg(k));
            }

          retval (1) = v;
          retval (0) = g;
        }
      else if (all_args_scalar && nargout < 3)
        {
          float g = args(0).float_value ();

          if (error_state || ! is_integer_value (g))
            {
              error ("gcd: all arguments must be integer");
              return retval;
            }

          FloatRowVector v (nargin, 0);
          FloatRowVector x (3);
          FloatRowVector y (3);

          v(0) = signum (g);

          g = std::abs(g);
      
          for (int k = 1; k < nargin; k++)
            {
              x(0) = g;
              x(1) = 1;
              x(2) = 0;

              y(0) = args(k).float_value ();
              y(1) = 0;
              y(2) = 1;

              float sgn = signum (y(0));

              y(0) = std::abs (y(0));

              if (error_state || ! is_integer_value (g))
                {
                  error ("gcd: all arguments must be integer");
                  return retval;
                }

              while (y(0) > 0)
                {
                  FloatRowVector r = x - y * std::floor (x(0) / y(0));
                  x = y;
                  y = r;
                }

              g = x(0);

              for (int i = 0; i < k; i++) 
                v(i) *= x(1);

              v(k) = x(2) * sgn;
            }

          retval (1) = v;
          retval (0) = g;
        }
      else
        {
          // FIXME -- we need to handle a possible mixture of scalar and
          // array values here.

          FloatNDArray g = args(0).float_array_value ();

          OCTAVE_LOCAL_BUFFER (FloatNDArray, v, nargin);

          int nel = dv.numel ();

          v[0].resize(dv);

          for (int i = 0; i < nel; i++)
            {
              v[0](i) = signum (g(i));
              g(i) = std::abs (g(i));

              if (! is_integer_value (g(i)))
                {
                  error ("gcd: all arguments must be integer");
                  return retval;
                }
            }

          FloatRowVector x (3);
          FloatRowVector y (3);

          for (int k = 1; k < nargin; k++)
            {
              FloatNDArray gnew = args(k).float_array_value ();

              v[k].resize(dv);

              for (int n = 0; n < nel; n++)
                {
                  x(0) = g(n);
                  x(1) = 1;
                  x(2) = 0;

                  y(0) = std::abs (gnew(n));
                  y(1) = 0;
                  y(2) = 1; 

                  if (! is_integer_value (y(0)))
                    {
                      error ("gcd: all arguments must be integer");
                      return retval;
                    }

                  while (y(0) > 0)
                    {
                      FloatRowVector r = x - y * std::floor (x(0) / y(0));
                      x = y;
                      y = r;
                    }

                  g(n) = x(0);

                  for (int i = 0; i < k; i++) 
                    v[i](n) *= x(1);

                  v[k](n) = x(2) * signum (gnew(n));
                }
            }

          for (int k = 0; k < nargin; k++)
            retval(1+k) = v[k];

          retval (0) = g;
        }
    }
  else if (nargin == 1)
    {
      NDArray gg = args(0).array_value ();

      int nel = dv.numel ();

      NDArray v (dv);

      RowVector x (3);
      RowVector y (3);

      double g = std::abs (gg(0));

      if (! is_integer_value (g))
        {
          error ("gcd: all arguments must be integer");
          return retval;
        }

      v(0) = signum (gg(0));
      
      for (int k = 1; k < nel; k++)
        {
          x(0) = g;
          x(1) = 1;
          x(2) = 0;

          y(0) = std::abs (gg(k));
          y(1) = 0;
          y(2) = 1;

          if (! is_integer_value (y(0)))
            {
              error ("gcd: all arguments must be integer");
              return retval;
            }

          while (y(0) > 0)
            {
              RowVector r = x - y * std::floor (x(0) / y(0));
              x = y;
              y = r;
            }

          g = x(0);

          for (int i = 0; i < k; i++) 
            v(i) *= x(1);

          v(k) = x(2) * signum (gg(k));
        }

      retval (1) = v;
      retval (0) = g;
    }
  else if (all_args_scalar && nargout < 3)
    {
      double g = args(0).double_value ();

      if (error_state || ! is_integer_value (g))
        {
          error ("gcd: all arguments must be integer");
          return retval;
        }

      RowVector v (nargin, 0);
      RowVector x (3);
      RowVector y (3);

      v(0) = signum (g);

      g = std::abs(g);
      
      for (int k = 1; k < nargin; k++)
        {
          x(0) = g;
          x(1) = 1;
          x(2) = 0;

          y(0) = args(k).double_value ();
          y(1) = 0;
          y(2) = 1;

          double sgn = signum (y(0));

          y(0) = std::abs (y(0));

          if (error_state || ! is_integer_value (g))
            {
              error ("gcd: all arguments must be integer");
              return retval;
            }

          while (y(0) > 0)
            {
              RowVector r = x - y * std::floor (x(0) / y(0));
              x = y;
              y = r;
            }

          g = x(0);

          for (int i = 0; i < k; i++) 
            v(i) *= x(1);

          v(k) = x(2) * sgn;
        }

      retval (1) = v;
      retval (0) = g;
    }
  else
    {
      // FIXME -- we need to handle a possible mixture of scalar and
      // array values here.

      NDArray g = args(0).array_value ();

      OCTAVE_LOCAL_BUFFER (NDArray, v, nargin);

      int nel = dv.numel ();

      v[0].resize(dv);

      for (int i = 0; i < nel; i++)
        {
          v[0](i) = signum (g(i));
          g(i) = std::abs (g(i));

          if (! is_integer_value (g(i)))
            {
              error ("gcd: all arguments must be integer");
              return retval;
            }
        }

      RowVector x (3);
      RowVector y (3);

      for (int k = 1; k < nargin; k++)
        {
          NDArray gnew = args(k).array_value ();

          v[k].resize(dv);

          for (int n = 0; n < nel; n++)
            {
              x(0) = g(n);
              x(1) = 1;
              x(2) = 0;

              y(0) = std::abs (gnew(n));
              y(1) = 0;
              y(2) = 1; 

              if (! is_integer_value (y(0)))
                {
                  error ("gcd: all arguments must be integer");
                  return retval;
                }

              while (y(0) > 0)
                {
                  RowVector r = x - y * std::floor (x(0) / y(0));
                  x = y;
                  y = r;
                }

              g(n) = x(0);

              for (int i = 0; i < k; i++) 
                v[i](n) *= x(1);

              v[k](n) = x(2) * signum (gnew(n));
            }
        }

      for (int k = 0; k < nargin; k++)
        retval(1+k) = v[k];

      retval (0) = g;
    }

  return retval;
}

/*

%!assert(gcd (200, 300, 50, 35), gcd ([200, 300, 50, 35]))
%!assert(gcd ([200, 300, 50, 35]), 5);
%!assert(gcd (single(200), single(300), single(50), single(35)), gcd (single([200, 300, 50, 35])))
%!assert(gcd (single([200, 300, 50, 35])), single(5));

%!error <Invalid call to gcd.*> gcd ();

%!test
%! s.a = 1;
%! fail("gcd (s)");

 */

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
