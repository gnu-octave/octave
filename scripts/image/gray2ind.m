## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn {Function File} {[@var{img}, @var{map}] =} gray2ind (@var{I}, @var{n})
## Convert a gray scale intensity image to an Octave indexed image.
## The indexed image will consist of @var{n} different intensity values.  If not
## given @var{n} will default to 64.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [X, map] = gray2ind (I, n = 64)
  ## Check input
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  C = class(I);
  if (! ismatrix (I) || ndims (I) != 2)
    error ("gray2ind: first input argument must be a gray scale image");
  endif
  if (! isscalar (n) || n < 0)
    error ("gray2ind: second input argument must be a positive integer");
  endif
  ints = {"uint8", "uint16", "int8", "int16"};
  floats = {"double", "single"};
  if (! ismember (C, {ints{:}, floats{:}}))
    error ("gray2ind: invalid data type '%s'", C);
  endif
  if (ismember (C, floats) && (min (I(:)) < 0 || max (I(:)) > 1))
    error ("gray2ind: floating point images may only contain values between 0 and 1");
  endif

  ## Convert data
  map = gray (n);
  ## If @var{I} is an integer matrix convert it to a double matrix with values in [0, 1]
  if (ismember (C, ints))
    low = double (intmin (C));
    high = double (intmax (C));
    I = (double (I) - low) / (high - low);
  endif
  X = round (I*(n-1)) + 1;

endfunction
