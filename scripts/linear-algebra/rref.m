## Copyright (C) 2000, 2006, 2007, 2008, 2009 Paul Kienzle
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
## @deftypefn {Function File} {[@var{r}, @var{k}] =} rref (@var{A}, @var{tol})
##
## Returns the reduced row echelon form of @var{A}.  @var{tol} defaults
## to @code{eps * max (size (@var{A})) * norm (@var{A}, inf)}.
##
## Called with two return arguments, @var{k} returns the vector of
## "bound variables", which are those columns on which elimination 
## has been performed.
##
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
##         (based on an anonymous source from the public domain)

function [A, k] = rref (A, tol)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (ndims (A) > 2)
    error ("rref: expecting matrix argument");
  endif

  [rows, cols] = size (A);

  if (nargin < 2)
    if (isa (A, "single"))
      tol = eps ("single") * max (rows, cols) * norm (A, inf ("single"));
    else
      tol = eps * max (rows, cols) * norm (A, inf);
    endif
  endif

  used = zeros (1, cols);
  r = 1;
  for c = 1:cols
    ## Find the pivot row
    [m, pivot] = max (abs (A(r:rows,c)));
    pivot = r + pivot - 1;

    if (m <= tol)
      ## Skip column c, making sure the approximately zero terms are
      ## actually zero.
      A (r:rows, c) = zeros (rows-r+1, 1);
    else
      ## keep track of bound variables
      used (1, c) = 1;

      ## Swap current row and pivot row
      A ([pivot, r], c:cols) = A ([r, pivot], c:cols);

      ## Normalize pivot row
      A (r, c:cols) = A (r, c:cols) / A (r, c);

      ## Eliminate the current column
      ridx = [1:r-1, r+1:rows];
      A (ridx, c:cols) = A (ridx, c:cols) - A (ridx, c) * A(r, c:cols);

      ## Check if done
      if (r++ == rows)
        break;
      endif
    endif
  endfor
  k = find (used);

endfunction
