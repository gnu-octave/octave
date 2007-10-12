## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2005, 2006, 2007
##               Kurt Hornik
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
## @deftypefn {Function File} {} fvl (@var{r}, @var{n}, @var{l})
## Return the future value at the end of @var{n} periods of an initial
## lump sum investment @var{l}, given a per-period interest rate
## @var{r}.
##
## Note that the rate @var{r} is specified as a fraction (i.e., 0.05,
## not 5 percent).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Future value of an initial lump sum investment

function v = fvl (r, n, l)

  if (nargin != 3)
    print_usage ();
  endif

  if (! (isscalar (r) && r > -1))
    error ("fvl: r has to be a scalar > -1");
  elseif (! (isscalar (n) && n > 0))
    error ("fvl: n has to be a positive scalar");
  elseif (! isscalar (l))
    error ("fvl: l has to be a scalar");
  endif

  v = l * (1 + r)^n;

endfunction