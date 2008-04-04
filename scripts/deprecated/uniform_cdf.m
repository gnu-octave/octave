## Copyright (C) 1995, 1996, 1997, 2005, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} uniform_cdf (@var{x}, @var{a}, @var{b})
## Return the CDF at @var{x} of the uniform distribution on [@var{a},
## @var{b}], i.e., PROB (uniform (@var{a}, @var{b}) <= x).
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the uniform distribution

## Deprecated in version 3.0

function cdf = uniform_cdf (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
        ["uniform_cdf is obsolete and will be removed from a future\n",
	       "version of Octave, please use unifcdf instead"]);
  endif

 cdf =  unifcdf (varargin{:});

endfunction
