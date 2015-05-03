/*

Copyright (C) 2015 Carnë Draug

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

#include "ov.h"
#include "defun.h"
#include "error.h"
#include "dNDArray.h"
#include "fNDArray.h"

#include "lo-specfun.h"

DEFUN (psi, args, ,
"-*- texinfo -*-\n\
@deftypefn {Function File} {} psi (@var{z})\n\
Compute the psi (digamma) function.\n\
\n\
@tex\n\
$$\n\
\\Psi (z) = {d (log (\\Gamma (z))) \\over dx}\n\
$$\n\
@end tex\n\
@ifnottex\n\
@example\n\
@group\n\
psi (z) = d (log (gamma (z))) / dx\n\
@end group\n\
@end example\n\
@end ifnottex\n\
\n\
@seealso{gamma, gammainc, gammaln}\n\
@end deftypefn")
{
  octave_value retval;

  const octave_idx_type nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

#define FLOAT_BRANCH(T, A, M, E) \
  if (args(0).is_ ## T ##_type ()) \
    { \
      const A ## NDArray z = args(0).M ## array_value (); \
      A ## NDArray psi_z (z.dims ()); \
\
      const E* zv = z.data (); \
      E* psi_zv = psi_z.fortran_vec (); \
      const octave_idx_type n = z.numel (); \
      for (octave_idx_type i = 0; i < n; i++) \
        psi_zv[i] = psi (zv[i]); \
\
      retval = psi_z; \
    } 

  if (args(0).is_complex_type ())
    {
      FLOAT_BRANCH(double, Complex, complex_, Complex)
      else FLOAT_BRANCH(single, FloatComplex, float_complex_, FloatComplex)
      else
        {
          error ("psi: Z must be a floating point");
        }
    }
  else
    {
      FLOAT_BRANCH(double, , , double)
      else FLOAT_BRANCH(single, Float, float_, float)
      else
        {
          error ("psi: Z must be a floating point");
        }
    }

#undef FLOAT_BRANCH

  return retval;
}

/*
%!shared em
%! em = 0.577215664901532860606512090082402431042; # Euler-Mascheroni Constant

%!assert (psi (ones (7, 3, 5)), repmat (-em, [7 3 5]))
%!assert (psi ([0 1]), [-Inf -em])
%!assert (psi ([-20:1]), [repmat(-Inf, [1 21]) -em])
%!assert (psi (single ([0 1])), single ([-Inf -em]))

## Abramowitz and Stegun, page 258, eq 6.3.5
%!test
%! z = [-10:.1:-.1 .1:.1:20]; # drop the 0
%! assert (psi (z + 1), psi (z) + 1 ./ z, eps*1000)

## Abramowitz and Stegun, page 258, eq 6.3.2
%!assert (psi (1), -em)

## Abramowitz and Stegun, page 258, eq 6.3.3
%!assert (psi (1/2), -em - 2 * log (2))

## The following tests are from Pascal Sebah and Xavier Gourdon (2002)
## "Introduction to the Gamma Function"

## Interesting identities of the digamma function, in section of 5.1.3
%!assert (psi (1/3), - em - (3/2) * log(3) - ((sqrt (3) / 6) * pi), eps*10)
%!assert (psi (1/4), - em -3 * log (2) - pi /2)
%!assert (psi (1/6), - em -2 * log (2) - (3/2) * log (3) - ((sqrt (3) / 2) * pi), eps*10)

## First 6 zeros of the digamma function, in section of 5.1.5 (and also on
## Abramowitz and Stegun, page 258, eq 6.3.19)
%!assert (psi ( 1.46163214496836234126265954232572132846819620400644), 0, eps)
%!assert (psi (-0.504083008264455409258269304533302498955385182368579), 0, eps)
%!assert (psi (-1.573498473162390458778286043690434612655040859116846), 0, eps)
%!assert (psi (-2.610720868444144650001537715718724207951074010873480), 0, eps*10)
%!assert (psi (-3.635293366436901097839181566946017713948423861193530), 0, eps*10)
%!assert (psi (-4.653237761743142441714598151148207363719069416133868), 0, eps*100)

## Tests for complex values
%!shared z
%! z = [-10:.1:-.1 .1:.1:20]; # drop the 0

## Abramowitz and Stegun, page 259 eq 6.3.10
%!assert (real (psi (i*z)), real (psi (1 - i*z)))

## Abramowitz and Stegun, page 259 eq 6.3.11
%!assert (imag (psi (i*z)), 1/2 .* 1./z + 1/2 * pi * coth (pi * z), eps *10)

## Abramowitz and Stegun, page 259 eq 6.3.12
%!assert (imag (psi (1/2 + i*z)), 1/2 * pi * tanh (pi * z), eps)

## Abramowitz and Stegun, page 259 eq 6.3.13
%!assert (imag (psi (1 + i*z)), - 1./(2*z) + 1/2 * pi * coth (pi * z), eps*10)

*/
