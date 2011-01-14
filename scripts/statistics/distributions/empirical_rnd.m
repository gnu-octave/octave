## Copyright (C) 1996-2011 Kurt Hornik
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
## @deftypefn  {Function File} {} empirical_rnd (@var{n}, @var{data})
## @deftypefnx {Function File} {} empirical_rnd (@var{data}, @var{r}, @var{c})
## @deftypefnx {Function File} {} empirical_rnd (@var{data}, @var{sz})
## Generate a bootstrap sample of size @var{n} from the empirical
## distribution obtained from the univariate sample @var{data}.
##
## If @var{r} and @var{c} are given create a matrix with @var{r} rows and
## @var{c} columns.  Or if @var{sz} is a vector, create a matrix of size
## @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Bootstrap samples from the empirical distribution

function rnd = empirical_rnd (data, r, c)

  if (nargin == 2)
    if (isscalar(data))
      c = data;
      data = r;
      r = 1;
    endif
  elseif (nargin != 3)
    print_usage ();
  endif

  if (! isvector (data))
    error ("empirical_rnd: DATA must be a vector");
  endif

  rnd = discrete_rnd (data, ones (size (data)) / length (data), r, c);

endfunction
