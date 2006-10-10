## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} polyder (@var{c})
## @deftypefnx {Function File} {[@var{q}] =} polyder (@var{b}, @var{a})
## @deftypefnx {Function File} {[@var{q}, @var{r}] =} polyder (@var{b}, @var{a})
## See polyderiv.
## @end deftypefn

## Author: John W. Eaton

function [q, r] = polyder (p, a)

  if (nargin == 1)
    q = polyderiv (p);
  elseif (nargin == 2)
    if (nargout == 2)
      [q, r] = polyderiv (p, a);
    else
      q = polyderiv (p, a);
    endif
  else
    print_usage ();
  endif

endfunction
