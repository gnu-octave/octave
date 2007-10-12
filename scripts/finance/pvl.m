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
## @deftypefn {Function File} {} pvl (@var{r}, @var{n}, @var{p})
## Return the present value of an investment that will pay off @var{p}
## in one lump sum at the end of @var{n} periods, given the interest
## rate @var{r}.
##
## Note that the rate @var{r} is specified as a fraction (i.e., 0.05,
## not 5 percent).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Present value of an investment that pays off at the end

function v = pvl (r, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! (isscalar (r) && (r > -1)))
    error ("pvl: r has to be a scalar > -1");
  elseif (! (isscalar (n) && n > 0))
    error ("pvl: n has to be a positive scalar");
  elseif (! isscalar (p))
    error ("pvl: p has to be a scalar");
  endif

  v = p / (1 + r)^n;

endfunction
