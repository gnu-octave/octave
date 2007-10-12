## Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2005,
##               2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} npv (@var{r}, @var{p}, @var{i})
## Returns the net present value of a series of irregular (i.e., not
## necessarily identical) payments @var{p} which occur at the ends of @var{n}
## consecutive periods.  @var{r} specifies the one-period interest rates and
## can either be a scalar (constant rates) or a vector of the same
## length as @var{p}.
##
## The optional argument @var{i} may be used to specify an initial
## investment.
##
## Note that the rate @var{r} is specified as a fraction (i.e., 0.05,
## not 5 percent).
## @seealso{irr, pv}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Net present value of a series of payments

function v = npv (r, p, i)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (p)))
    error ("npv: p has to be a vector");
  else
    n = length (p);
    p = reshape (p, 1, n);
  endif

  if (any (any (r <= -1)))
    error ("npv: all interest rates must be > -1");
  endif
  if (isscalar (r))
    d = 1 ./ (1 + r) .^ (0 : n);
  elseif (isvector (r) && (length (r) == n))
    d = [1, (1 ./ cumprod (reshape (1 + r, 1, n)))];
  else
    error ("npv: r must be a scalar or a vector of the same length as p");
  endif

  if (nargin == 3)
    if (! isscalar (i))
      error ("npv: I_0 must be a scalar");
    endif
  else
    i = 0;
  endif

  p = [i, p];
  v = sum (d .* p);

endfunction
