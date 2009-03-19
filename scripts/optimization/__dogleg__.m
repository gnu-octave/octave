## Copyright (C) 2008, 2009 Jaroslav Hajek
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
## @deftypefn{Function File} {@var{x}} = __dogleg__ (@var{r}, @var{b}, @var{x}, @var{d}, @var{delta}, @var{ismin})
## Solve the double dogleg trust-region problem:
## Minimize 
## @example
## norm(@var{r}*@var{x}-@var{b}) 
## @end example
## subject to the constraint 
## @example
## norm(@var{d}.*@var{x}) <= @var{delta} ,
## @end example
## x being a convex combination of the gauss-newton and scaled gradient.
## If @var{ismin} is true (default false), minimizes instead
## @example
## norm(@var{r}*@var{x})^2-2*@var{b}'*@var{x} 
## @end example
## @end deftypefn


## TODO: error checks
## TODO: handle singularity, or leave it up to mldivide?

function x = __dogleg__ (r, b, d, delta, ismin = false)
  ## Get Gauss-Newton direction.
  if (ismin)
    g = b;
    b = r' \ g;
  endif
  x = r \ b;
  xn = norm (d .* x);
  if (xn > delta)
    ## GN is too big, get scaled gradient.
    if (ismin)
      s = g ./ d;
    else
      s = (r' * b) ./ d;
    endif
    sn = norm (s);
    if (sn > 0)
      ## Normalize and rescale.
      s = (s / sn) ./ d;
      ## Get the line minimizer in s direction.
      tn = norm (r*s);
      snm = (sn / tn) / tn;
      if (snm < delta)
	## Get the dogleg path minimizer.
        bn = norm (b);
        dxn = delta/xn; snmd = snm/delta;
        t = (bn/sn) * (bn/xn) * snmd;
        t -= dxn * snmd^2 - sqrt ((t-dxn)^2 + (1-dxn^2)*(1-snmd^2));
        alpha = dxn*(1-snmd^2) / t;
      else
        alpha = 0;
      endif
    else
      alpha = delta / xn;
      snm = 0;
    endif
    ## Form the appropriate convex combination.
    x = alpha * x + ((1-alpha) * min (snm, delta)) * s;
  endif
endfunction

