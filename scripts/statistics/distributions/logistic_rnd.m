## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn  {Function File} {} logistic_rnd (@var{r}, @var{c})
## @deftypefnx {Function File} {} logistic_rnd (@var{sz})
## Return an @var{r} by @var{c} matrix of random numbers from the
## logistic distribution.  Or if @var{sz} is a vector, create a matrix of
## @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the logistic distribution

function rnd = logistic_rnd (r, c)


  if (nargin == 2)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("logistic_rnd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("logistic_rnd: C must be a positive integer");
    endif
    sz = [r, c];
  elseif (nargin == 1)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("logistic_rnd: R must be a positive integer or vector");
    endif
  else
    print_usage ();
  endif

  rnd = - log (1 ./ rand (sz) - 1);

endfunction
