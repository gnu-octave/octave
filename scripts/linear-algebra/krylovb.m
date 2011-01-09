## Copyright (C) 1993, 1998, 1999, 2000, 2003, 2005, 2006, 2007
##               A. Scottedward Hodel
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

function [Uret, Ucols] = krylovb (A, V, k, eps1, pflg)

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
