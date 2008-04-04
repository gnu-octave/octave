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
## @deftypefn {Function File} {} exponential_inv (@var{x}, @var{lambda})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the exponential distribution with parameter
## @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the exponential distribution

## Deprecated in version 3.0

function inv = exponential_inv (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
        ["exponential_inv is obsolete and will be removed from a future\n",
	       "version of Octave, please use expinv instead"]);
  endif

 if (nargin > 1)
   varargin{2} = 1 ./ varargin{2};
 endif

 inv =  expinv (varargin{:});

endfunction
