## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} pvl (@var{r}, @var{n}, @var{p})
## Return the present value of an investment that will pay off @var{p}
## in one lump sum at the end of @var{n} periods, given the interest
## rate @var{r}.
##
## Note that the rate @var{r} is specified as a fraction (i.e., 0.05,
## not 5 percent).
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Present value of an investment that pays off at the end

function v = pvl (r, n, p)

  if (nargin != 3)
    usage ("pvl (r, n, p)");
  endif

  if (! (is_scalar (r) && (r > -1)))
    error ("pvl: r has to be a scalar > -1");
  elseif (! (is_scalar (n) && (n > 0)))
    error ("pvl: n has to be a positive scalar");
  elseif (! is_scalar (p))
    error ("pvl: p has to be a scalar");
  endif

  v = p / (1 + r)^n;

endfunction
