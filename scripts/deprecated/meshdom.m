## Copyright (C) 1996, 1997, 2007, 2008 John W. Eaton
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
## @deftypefn {Function File} {} meshdom (@var{x}, @var{y})
## This function has been deprecated.  Use @code{meshgrid} instead.
## @end deftypefn

## Author: jwe

## Deprecated in version 3.0

function [xx, yy] = meshdom (x, y)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "meshdom is obsolete and will be removed from a future version of Octave; please use meshgrid instead");
  endif

  if (nargin == 2)
    if (isvector (x) && isvector (y))
      xx = ones (length (y), 1) * x(:).';
      yy = flipud (y(:)) * ones (1, length (x));
    else
      error ("meshdom: arguments must be vectors");
    endif
  else
    print_usage ();
  endif

endfunction
