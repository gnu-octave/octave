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
## @deftypefn {Function File} {} gray (@var{n})
## Return a gray colormap with @var{n} entries corresponding to values from
## 0 to @var{n}-1.  The argument @var{n} should be a scalar.  If it is
## omitted, 64 is assumed.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function map = gray (number)

  if (nargin == 0)
    number = 64;
  elseif (nargin > 1)
    print_usage ();
  endif

  gr = [0:(number-1)]';

  map = colormap ([ gr, gr, gr ] / (number - 1));

endfunction
