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
## @deftypefn {Function File} {} pmt (@var{r}, @var{n}, @var{a}, @var{l}, @var{method})
## Return the amount of periodic payment necessary to amortize a loan
## of amount a with interest rate @var{r} in @var{n} periods.
##
## The optional argument @var{l} may be used to specify a terminal
## lump-sum payment.
##
## The optional argument @var{method} may be used to specify whether
## payments are made at the end (@var{"e"}, default) or at the beginning
## (@var{"b"}) of each period.
## @seealso{pv, nper, rate}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Amount of periodic payment needed to amortize a loan

function p = pmt (r, n, a, l, m)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (! (isscalar (r) && r > -1))
    error ("pmt: rate must be a scalar > -1");
  elseif (! (isscalar (n) && n > 0))
    error ("pmt: n must be a positive scalar");
  elseif (! (isscalar (a) && a > 0))
    error ("pmt: a must be a positive scalar");
  endif

  if (nargin == 5)
    if (! ischar (m))
      error ("pmt: `method' must be a string");
    endif
  elseif (nargin == 4)
    if (ischar (l))
      m = l;
      l = 0;
    else
      m = "e";
    endif
  else
    l = 0;
    m = "e";
  endif

  p = r * (a - l * (1 + r)^(-n)) / (1 - (1 + r)^(-n));

  if (strcmp (m, "b"))
    p = p / (1 + r);
  endif


endfunction




