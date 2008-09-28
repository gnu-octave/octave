## Copyright (C) 2008 Jaroslav Hajek <highegg@gmail.com>
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
## @deftypefn{Function File} {@var{x}} = __dogleg__ (@var{r}, @var{b}, @var{x}, @var{d}, @var{delta})
## Solves the double dogleg trust-region problem:
## Minimize norm(r*x-b) subject to the constraint norm(d.*x) <= delta,
## x being a convex combination of the gauss-newton and scaled gradient.
## @end deftypefn

## TODO: error checks
## TODO: handle singularity, or leave it up to mldivide?

function x = __dogleg__ (r, b, d, delta)
  # get Gauss-Newton direction
  x = r \ b;
  xn = norm (d .* x);
  if (xn > delta)
    # GN is too big, get scaled gradient
    s = (r' * b) ./ d;
    sn = norm (s);
    if (sn > 0)
      # normalize and rescale 
      s = (s / sn) ./ d;
      # get the line minimizer in s direction
      tn = norm (r*s);
      snm = (sn / tn) / tn;
      if (snm < delta)
        # get the dogleg path minimizer 
        bn = norm (b);
        dxn = delta/xn; snmd = snm/delta;
        t = (bn/sn) * (bn/xn) * snmd;
        t -= dxn * snmd^2 - sqrt ((t-dxn)^2 + (1-dxn^2)*(1-snmd^2));
        alpha = dxn*(1-snmd^2) / t;
      else
        alpha = 0;
      endif
    else
      snm = 0;
    endif
    # form the appropriate convex combination
    x = alpha * x + ((1-alpha) * min (snm, delta)) * s;
  endif
endfunction

