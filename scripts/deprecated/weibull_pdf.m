## Copyright (C) 2005, 2006, 2007, 2008 John W. Eaton
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
## @deftypefn {Function File} {} weibull_pdf (@var{x}, @var{shape}, @var{scale})
## Compute the probability density function (PDF) at @var{x} of the
## Weibull distribution with shape parameter @var{scale} and scale
## parameter @var{shape} which is given by
##
## @example
##    scale * shape^(-scale) * x^(scale-1) * exp(-(x/shape)^scale)
## @end example
##
## @noindent
## for @var{x} > 0.
## @end deftypefn

## Deprecated in version 3.0

function pdf = weibull_pdf (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "weibull_pdf is obsolete and will be removed from a future version of Octave; please use wblpdf instead");
  endif

  if (nargin == 2)
    varargin{3} = varargin{2};
    varargin{2} = 1;
  elseif (nargin > 2)
    tmp = varargin{3};
    varargin{3} = varargin{2};
    varargin{2} = tmp;
  endif

  pdf = wblpdf (varargin{:});

endfunction
