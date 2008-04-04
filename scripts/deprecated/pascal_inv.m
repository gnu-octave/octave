## Copyright (C) 1995, 1996, 1997, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} pascal_inv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile at @var{x} of the
## Pascal (negative binomial) distribution with parameters @var{n} and
## @var{p}.
##
## The number of failures in a Bernoulli experiment with success
## probability @var{p} before the @var{n}-th success follows this
## distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Pascal distribution

## Deprecated in version 3.0

function inv = pascal_inv (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
        ["pascal_inv is obsolete and will be removed from a future\n",
	       "version of Octave, please use nbininv instead"]);
  endif

  inv = nbininv(varargin{:});

endfunction
