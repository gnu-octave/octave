########################################################################
##
## Copyright (C) 2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{tf} =} isuniform (@var{v})
## @deftypefnx {} {[@var{tf}, @var{delta}] =} isuniform (@var{v})
## Return true if the real vector @var{v} is uniformly spaced and false
## otherwise.
##
## A vector is uniform if the mean difference (@var{delta}) between all
## elements is the same to within a tolerance of
## @w{@code{4 * eps (max (abs (@var{v})))}}.
##
## The optional output @var{delta} is the uniform difference between elements.
## If the vector is not uniform then @var{delta} is @code{NaN}.  @var{delta}
## is of the same class as @var{v} for floating point inputs and of class
## double for integer, logical, and character inputs.
##
## Programming Notes: The output is always false for the special cases of an
## empty input or a scalar input.  If any element is @code{NaN} then the output
## is false.  If @var{delta} is smaller than the calculated relative tolerance
## then an absolute tolerance of @code{eps} is used.
##
## @seealso{linspace, colon}
## @end deftypefn

function [tf, delta] = isuniform (v)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isreal (v) && (isvector (v) || isempty (v))))
    error ("isuniform: V must be a real vector");
  endif

  if (! isfloat (v))
    v = double (v);  # char, logical, integer inputs converted to double
  endif

  ## Handle special corner cases
  if (isempty (v) || isscalar (v))
    tf = false;
    delta = NaN (class (v));
    return;
  endif

  ## Compare mean delta to individual deltas with a tolerance
  d = diff (v, 1);
  delta = mean (d);
  if (isnan (delta))
    tf = false;
  else
    tol = 4 * eps (max (abs (v)));
    if (delta < tol)
      ## Switch to absolute tolerance for very small delta
      tol = eps (class (v));
    endif
    tf = ! any (abs (d - delta) > tol);
    if (! tf)
      delta = NaN (class (v));
    endif
  endif

endfunction


%!assert (isuniform ([]), false)                  # empty input
%!assert (isuniform (zeros (1,2,0,4)), false)     # empty input
%!assert (isuniform (1), false)                   # scalar input
%!assert (isuniform (1:5), true)
%!assert (isuniform (int8 (1:3:10)), true)
%!assert (isuniform ([false false false]), true)
%!assert (isuniform (['A', 'C', 'E']), true)

## Test return class of step
%!test
%! [tf, delta] = isuniform (single (10:-1.5:1));
%! assert (tf, true);
%! assert (delta, single (-1.5));
%! [tf, delta] = isuniform (single ([1 2 5 6]));
%! assert (tf, false);
%! assert (delta, single (NaN));

%!test
%! [tf, delta] = isuniform (int8 (1:3:15));
%! assert (tf, true);
%! assert (delta, double (3));
%! [tf, delta] = isuniform (int8 ([1 2 5 6]));
%! assert (tf, false);
%! assert (delta, double (NaN));

## Test for small delta smaller than tolerance
%!test
%! v = 1:eps:(1+8*eps);
%! [tf, delta] = isuniform (v);
%! assert (tf, true);
%! assert (delta, eps);
%! v(3) -= 2*eps;
%! [tf, delta] = isuniform (v);
%! assert (tf, false);
%! assert (delta, NaN);

## test input validation
%!error <Invalid call> isuniform ()
%!error <V must be a real vector> isuniform (magic (3))
%!error <V must be a real vector> isuniform ({1, 2, 3})
