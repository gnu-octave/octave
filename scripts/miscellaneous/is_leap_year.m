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
## @deftypefn {Function File} {} is_leap_year (@var{year})
## Return 1 if the given year is a leap year and 0 otherwise.  If no
## arguments are provided, @code{is_leap_year} will use the current year.
## For example,
##
## @example
## @group
## is_leap_year (2000)
##      @result{} 1
## @end group
## @end example
## @end deftypefn

## Author: jwe

function retval = is_leap_year (year)

  if (nargin > 1)
    usage ("is_leap_year (year)");
  endif

  if (nargin == 0)
    t = clock ();
    year = t (1);
  endif

  retval = ((rem (year, 4) == 0 & rem (year, 100) != 0) ...
            | rem (year, 400) == 0);

endfunction
