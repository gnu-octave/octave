## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} exponential_cdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the exponential distribution with
## parameter @var{lambda}.
##
## The arguments can be of common size or scalar.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the exponential distribution

function cdf = exponential_cdf (varargin)

 if (nargin > 1)
   varargin{2} = 1 ./ varargin{2};
 endif

 cdf =  expcdf (varargin{:});

endfunction
