########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} humps (@var{x})
## @deftypefnx {} {[@var{x}, @var{y}] =} humps (@var{x})
## Evaluate a function with multiple minima, maxima, and zero crossings.
##
## The output @var{y} is the evaluation of the rational function:
##
## @tex
## $$y = -{ {1200x^4 - 2880x^3 + 2036x^2 - 348x - 88} \over {200x^4 - 480x^3 + 406x^2 - 138x + 17} }$$
## @end tex
## @ifnottex
##
## @example
## @group
##         1200*@var{x}^4 - 2880*@var{x}^3 + 2036*@var{x}^2 - 348*@var{x} - 88
##  @var{y} = - ---------------------------------------------
##          200*@var{x}^4 - 480*@var{x}^3 + 406*@var{x}^2 - 138*@var{x} + 17
## @end group
## @end example
##
## @end ifnottex
##
## @var{x} may be a scalar, vector or array.  If @var{x} is omitted, the
## default range [0:0.05:1] is used.
##
## When called with two output arguments, [@var{x}, @var{y}], @var{x} will
## contain the input values, and @var{y} will contain the output from
## @code{humps}.
##
## Programming Notes: @code{humps} has two local maxima located near @var{x} =
## 0.300 and 0.893, a local minimum near @var{x} = 0.637, and zeros near
## @var{x} = -0.132 and 1.300.  @code{humps} is a useful function for testing
## algorithms which find zeros or local minima and maxima.
##
## Try @code{demo humps} to see a plot of the @code{humps} function.
## @seealso{fzero, fminbnd, fminunc, fminsearch}
## @end deftypefn

function [x, y] = humps (x = [0:0.05:1])

  y = - 4*( 300*x.^4 - 720*x.^3 + 509*x.^2 - 87*x - 22) ./ ...
          ((10*x.^2 - 6*x + 1).*(20*x.^2 - 36*x + 17));

  if (nargout <= 1)
    x = y;
  endif

endfunction


%!demo
%! clf;
%! fplot (@humps, [0, 2]);
%! title ("humps() function");

## value checks
%!assert (humps (0), 88/17, 10*eps)
%!assert (humps (1), 16, 10*eps)
%!assert (humps (-1), -6376/1241, 10*eps)
%!assert (humps (), [88/17, 16106/1769, 263/17, 82802/3133, 2432/53, ...
%!   2818/37, 193/2, 10538/137, 1376/29, 36434/1261, 19, 5258/377,  ...
%!   152/13, 24562/2173, 421/34, 250/17, 232/13, 1762/85, 803/37,   ...
%!   58354/2941, 16], 1000*eps)

## vector checks
%!assert (humps ([0, 1]), [88/17, 16], 10*eps)
%!assert (humps ([0, 1]'), [88/17, 16]', 10*eps)
%!assert (humps ([0, 1; 1, 0]'), [88/17, 16; 16, 88/17]', 10*eps)

## array checks
%!assert (humps (repmat (eye (2), 1, 1, 2)),
%!        repmat ([16, 88/17; 88/17, 16], 1, 1, 2), 10*eps)

## other checks
%!assert (humps (NaN), NaN)
%!assert (humps ([]), [])

## Test input validation
%!error humps (1,3)
