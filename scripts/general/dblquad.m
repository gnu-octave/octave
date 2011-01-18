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
## @deftypefn {Function File} {} dblquad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{tol}, @var{quadf}, @dots{})
## Numerically evaluate a double integral.  The function over with to
## integrate is defined by @code{@var{f}}, and the interval for the
## integration is defined by @code{[@var{xa}, @var{xb}, @var{ya},
## @var{yb}]}.  The function @var{f} must accept a vector @var{x} and a
## scalar @var{y}, and return a vector of the same length as @var{x}. 
##
## If defined, @var{tol} defines the absolute tolerance to which to
## which to integrate each sub-integral.
##
## Additional arguments, are passed directly to @var{f}.  To use the default
## value for @var{tol} one may pass an empty matrix.
## @seealso{triplequad,quad,quadv,quadl,quadgk,quadcc,trapz}
## @end deftypefn

function q = dblquad(f, xa, xb, ya, yb, tol, quadf, varargin) 
  if (nargin < 5)
    print_usage ();
  endif
  if (nargin < 6 || isempty (tol))
    tol = 1e-6; 
  endif
  if (nargin < 7 || isempty (quadf))
    quadf = @quadgk; 
  endif

  inner = @__dblquad_inner__;
  if (ischar (f))
    f = @(x,y) feval (f, x, y, varargin{:});
    varargin = {};
  endif

  q = feval (quadf, @(y) inner (y, f, xa, xb, tol, quadf,
                                varargin{:}), ya, yb, tol);
endfunction

function q = __dblquad_inner__ (y, f, xa, xb, tol, quadf, varargin)
  q = zeros (size(y));
  for i = 1 : length (y)
    q(i) = feval (quadf, @(x) f(x, y(i), varargin{:}), xa, xb, tol);
  endfor
endfunction

%% Nasty integrand to show quadgk off
%!assert (dblquad (@(x,y) 1 ./ (x+y), 0, 1, 0, 1), 2*log(2), 1e-6)

%!assert (dblquad (@(x,y) exp(-x.^2 - y.^2) , -1, 1, -1, 1, [],  @quadgk), pi * erf(1).^2, 1e-6)
%!assert (dblquad (@(x,y) exp(-x.^2 - y.^2) , -1, 1, -1, 1, [],  @quadl), pi * erf(1).^2, 1e-6)
%!assert (dblquad (@(x,y) exp(-x.^2 - y.^2) , -1, 1, -1, 1, [],  @quadv), pi * erf(1).^2, 1e-6)
