## Copyright (C) 2008-2011 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} triplequad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb}, @var{tol}, @var{quadf}, @dots{})
## Numerically evaluate a triple integral.  The function over which to
## integrate is defined by @code{@var{f}}, and the interval for the
## integration is defined by @code{[@var{xa}, @var{xb}, @var{ya},
## @var{yb}, @var{za}, @var{zb}]}.  The function @var{f} must accept a
## vector @var{x} and a scalar @var{y}, and return a vector of the same
## length as @var{x}.
##
## If defined, @var{tol} defines the absolute tolerance to which to
## which to integrate each sub-integral.
##
## Additional arguments, are passed directly to @var{f}.  To use the default
## value for @var{tol} one may pass an empty matrix.
## @seealso{dblquad,quad,quadv,quadl,quadgk,quadcc,trapz}
## @end deftypefn

function Q = triplequad(f, xa, xb, ya, yb, za, zb, tol, quadf, varargin)
  if (nargin < 7)
    print_usage ();
  endif
  if (nargin < 8 || isempty (tol))
    tol = 1e-6;
  endif
  if (nargin < 9 || isempty (quadf))
    quadf = @quadgk;
  endif

  inner = @__triplequad_inner__;
  if (ischar (f))
    f = @(x,y,z) feval (f, x, y, z, varargin{:});
    varargin = {};
  endif

  Q = dblquad(@(y, z) inner (y, z, f, xa, xb, tol, quadf, varargin{:}),ya, yb, za, zb, tol);
endfunction

function Q = __triplequad_inner__ (y, z, f, xa, xb, tol, quadf, varargin)
  Q = zeros (size(y));
  for i = 1 : length (y)
    Q(i) = feval (quadf, @(x) f (x, y(i), z, varargin{:}), xa, xb, tol);
  endfor
endfunction

%% These tests are too expensive to run normally. Disable them
% !#assert (triplequad (@(x,y,z) exp(-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [],  @quadgk), pi ^ (3/2) * erf(1).^3, 1e-6)
% !#assert (triplequad (@(x,y,z) exp(-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [],  @quadl), pi ^ (3/2) * erf(1).^3, 1e-6)
% !#assert (triplequad (@(x,y,z) exp(-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1, 1, [],  @quadv), pi ^ (3/2) * erf(1).^3, 1e-6)
