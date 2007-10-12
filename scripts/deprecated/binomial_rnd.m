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
## @deftypefn {Function File} {} binomial_rnd (@var{n}, @var{p}, @var{r}, @var{c})
## @deftypefnx {Function File} {} binomial_rnd (@var{n}, @var{p}, @var{sz})
## Return an @var{r} by @var{c}  or a @code{size (@var{sz})} matrix of 
## random samples from the binomial distribution with parameters @var{n}
## and @var{p}.  Both @var{n} and @var{p} must be scalar or of size
## @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{n} and @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the binomial distribution

function rnd = binomial_rnd (varargin)

 rnd =  binornd (varargin{:});

endfunction
