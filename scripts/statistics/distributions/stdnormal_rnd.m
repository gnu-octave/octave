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
## @deftypefn  {Function File} {} stdnormal_rnd (@var{r}, @var{c})
## @deftypefnx {Function File} {} stdnormal_rnd (@var{sz})
## Return an @var{r} by @var{c} or @code{size (@var{sz})} matrix of 
## random numbers from the standard normal distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the standard normal distribution

function rnd = stdnormal_rnd (r, c)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargin == 2)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("stdnormal_rnd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("stdnormal_rnd: C must be a positive integer");
    endif
    sz = [r, c];
  else
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("stdnormal_rnd: R must be a positive integer or vector");
    endif
  endif

  rnd = randn (sz);

endfunction
