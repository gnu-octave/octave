########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{q} =} triplequad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb})
## @deftypefnx {} {@var{q} =} triplequad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb}, @var{tol})
## @deftypefnx {} {@var{q} =} triplequad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb}, @var{tol}, @var{quadf})
## @deftypefnx {} {@var{q} =} triplequad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb}, @var{tol}, @var{quadf}, @dots{})
## Numerically evaluate the triple integral of @var{f}.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must have the form
## @math{w = f(x,y,z)} where either @var{x} or @var{y} is a vector and the
## remaining inputs are scalars.  It should return a vector of the same length
## and orientation as @var{x} or @var{y}.
##
## @var{xa}, @var{ya}, @var{za} and @var{xb}, @var{yb}, @var{zb} are the lower
## and upper limits of integration for x, y, and z respectively.  The
## underlying integrator determines whether infinite bounds are accepted.
##
## The optional argument @var{tol} defines the absolute tolerance used to
## integrate each sub-integral.  The default value is 1e-6.
##
## The optional argument @var{quadf} specifies which underlying integrator
## function to use.  Any choice but @code{quad} is available and the default
## is @code{quadcc}.
##
## Additional arguments, are passed directly to @var{f}.  To use the default
## value for @var{tol} or @var{quadf} one may pass @qcode{':'} or an empty
## matrix ([]).
## @seealso{integral3, integral2, dblquad, quad, quadv, quadl, quadgk, quadcc,
##          trapz}
## @end deftypefn

function q = triplequad (f, xa, xb, ya, yb, za, zb, tol = 1e-6, quadf = @quadcc, varargin)

  if (nargin < 7)
    print_usage ();
  endif

  ## Allow use of empty matrix ([]) to indicate default
  if (isempty (tol))
    tol = 1e-6;
  endif
  if (isempty (quadf))
    quadf = @quadcc;
  endif

  inner = @__triplequad_inner__;
  if (ischar (f))
    f = @(x,y,z) feval (f, x, y, z, varargin{:});
    varargin = {};
  endif

  q = dblquad (@(y, z) inner (y, z, f, xa, xb, tol, quadf, varargin{:}), ...
               ya, yb, za, zb, tol);

endfunction

function q = __triplequad_inner__ (y, z, f, xa, xb, tol, quadf, varargin)
  q = zeros (size (y));
  for i = 1 : length (y)
    q(i) = feval (quadf, @(x) f (x, y(i), z, varargin{:}), xa, xb, tol);
  endfor
endfunction


%!assert (triplequad (@(x,y,z) exp (-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [], @quadcc), pi^(3/2) * erf (1).^3, 1e-6)

## These tests are too expensive to run normally (~30 sec each).  Disable them
%!#assert (triplequad (@(x,y,z) exp (-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [], @quadgk), pi^(3/2) * erf (1).^3, 1e-6)
%!#assert (triplequad (@(x,y,z) exp (-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [], @quadl), pi^(3/2) * erf (1).^3, 1e-6)
%!#assert (triplequad (@(x,y,z) exp (-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [], @quadv), pi^(3/2) * erf (1).^3, 1e-6)
