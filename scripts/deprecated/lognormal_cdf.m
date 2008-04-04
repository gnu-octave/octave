## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} lognormal_cdf (@var{x}, @var{a}, @var{v})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the lognormal distribution with
## parameters @var{a} and @var{v}.  If a random variable follows this
## distribution, its logarithm is normally distributed with mean
## @code{log (@var{a})} and variance @var{v}.
##
## Default values are @var{a} = 1, @var{v} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the log normal distribution

## Deprecated in version 3.0

function cdf = lognormal_cdf (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
        ["lognormal_cdf is obsolete and will be removed from a future\n",
	       "version of Octave, please use logncdf instead"]);
  endif

  if (nargin > 1)
    a = varargin{2};
    idx = a >= 0;
    a(idx) = log (a(idx));
    a(!idx) = NaN;
    varargin{2} = a;
  endif

  if (nargin > 2)
    v = varargin{3};
    idx = v >= 0;
    v(idx) = sqrt (v(idx));
    v(!idx) = NaN;
    varargin{3} = v;
  endif

  cdf = logncdf (varargin{:});

endfunction
