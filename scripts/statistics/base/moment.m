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
## @deftypefn {Function File} {} moment (@var{x}, @var{p}, @var{opt}, @var{dim})
## If @var{x} is a vector, compute the @var{p}-th moment of @var{x}.
##
## If @var{x} is a matrix, return the row vector containing the
## @var{p}-th moment of each column.
##
## With the optional string opt, the kind of moment to be computed can
## be specified.  If opt contains @code{"c"} or @code{"a"}, central
## and/or absolute moments are returned.  For example,
##
## @example
## moment (x, 3, "ac")
## @end example
##
## @noindent
## computes the third central absolute moment of @var{x}.
##
## If the optional argument @var{dim} is supplied, work along dimension
## @var{dim}.
## @end deftypefn

## Can easily be made to work for continuous distributions (using quad)
## as well, but how does the general case work?

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute moments

function m = moment (x, p, opt1, opt2)

  if ((nargin < 2) || (nargin > 4))
    print_usage ();
  endif

  need_dim = 0;

  if (nargin == 2)
    opt = "";
    need_dim = 1;
  elseif (nargin == 3)
    if (ischar (opt1))
      opt = opt1;
      need_dim = 1;
    else
      dim = opt1;
      opt = "";
    endif
  elseif (nargin == 4)
    if (ischar (opt1))
      opt = opt1;
      dim = opt2;
    elseif (ischar (opt2))
      opt = opt2;
      dim = opt1;
    else
      error ("moment: expecting opt to be a string");
    endif
  else
    print_usage ();
  endif

  if (need_dim)
    t = find (size (x) != 1);
    if (isempty (t))
      dim = 1;
    else
      dim = t(1);
    endif
  endif

  sz = size (x);
  n = sz (dim);

  if (numel (x) < 1)
    error ("moment: x must not be empty");
  endif

  if any (opt == "c")
    rng = ones (1, length (sz));
    rng(dim) = sz(dim);
    x = x - repmat (sum (x, dim), rng) / n;
  endif
  if any (opt == "a")
    x = abs (x);
  endif

  m = sum (x .^ p, dim) / n;

endfunction
