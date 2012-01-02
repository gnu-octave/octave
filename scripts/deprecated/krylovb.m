## Copyright (C) 1993-2012 A. Scottedward Hodel
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
## @deftypefn {Function File} {[@var{u}, @var{ucols}] =} krylovb (@var{A}, @var{V}, @var{k}, @var{eps1}, @var{pflg})
## See @code{krylov}.
## @end deftypefn

## Deprecated in version 3.4

function [Uret, Ucols] = krylovb (A, V, k, eps1, pflg)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "krylovb is obsolete and will be removed from a future version of Octave; please use [Uret, ~, Ucols] = krylov (...) instead");
  endif

  switch (nargin)
    case (3)
      [Uret, H, Ucols] = krylov (A, V, k);
    case (4)
      [Uret, H, Ucols] = krylov (A, V, k, eps1);
    case (5)
      [Uret, H, Ucols] = krylov (A, V, k, eps1, pflg);
    otherwise
      print_usage ();
  endswitch

endfunction
