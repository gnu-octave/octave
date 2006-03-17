## Copyright (C) 2006 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} weibull_inv (@var{x}, @var{shape}, @var{scale})
## Compute the quantile (the inverse of the CDF) at @var{x} of the
## Weibull distribution with shape parameter @var{scale} and scale
## parameter @var{shape}.
## @end deftypefn

function inv = weibull_inv (varargin)

  if (nargin == 2)
    varargin{3} = varargin{2};
    varargin{2} = 1;
  elseif (nargin > 2)
    tmp = varargin{3};
    varargin{3} = varargin{2};
    varargin{2} = tmp;
  endif

  inv = wblinv (varargin{:});

endfunction
