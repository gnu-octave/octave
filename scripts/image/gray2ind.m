## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2005, 2006, 2007
##               John W. Eaton
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
## @deftypefn {Function File} {[@var{img}, @var{map}] =} gray2ind (@var{})
## Convert a gray scale intensity image to an Octave indexed image.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [X, map] = gray2ind (I, n)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (nargin == 1)
    n = 64;
  endif

  map = gray (n);

  X = round (I*(n-1)) + 1;

endfunction
