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
## @deftypefn {Function File} {} moment (@var{x}, @var{p}, @var{opt})
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
## @end deftypefn

## Can easily be made to work for continuous distributions (using quad)
## as well, but how does the general case work?

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Compute moments

function m = moment (x, p, opt)

  if ((nargin < 2) || (nargin > 3))
    usage ("moment (x, p, type");
  endif

  [nr, nc] = size (x);
  if (nr == 0 || nc == 0)
    error ("moment: x must not be empty");
  elseif (nr == 1)
    x  = reshape (x, nc, 1);
    nr = nc;
  endif

  if (nargin == 3)
    tmp = implicit_str_to_num_ok;
    implicit_str_to_num_ok = "true";
    if any (opt == "c")
      x = x - ones (nr, 1) * sum (x) / nr;
    endif
    if any (opt == "a")
      x = abs (x);
    endif
    implicit_str_to_num_ok = tmp;
  endif

  m = sum(x .^ p) / nr;

endfunction
