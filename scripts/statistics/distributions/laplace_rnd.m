## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006,
##               2007 Kurt Hornik
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
## @deftypefn  {Function File} {} laplace_rnd (@var{r}, @var{c})
## @deftypefnx {Function File} {} laplace_rnd (@var{sz});
## Return an @var{r} by @var{c} matrix of random numbers from the
## Laplace distribution.  Or if @var{sz} is a vector, create a matrix of
## @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Laplace distribution

function rnd = laplace_rnd (r, c)

  if (nargin == 2)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("laplace_rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("laplace_rnd: c must be a positive integer");
    endif
    sz = [r, c];
  elseif (nargin == 1)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("laplace_rnd: r must be a positive integer or vector");
    endif
  else
    print_usage ();
  endif

  tmp = rand (sz);
  rnd = ((tmp < 1/2) .* log (2 * tmp)
         - (tmp > 1/2) .* log (2 * (1 - tmp)));

endfunction
