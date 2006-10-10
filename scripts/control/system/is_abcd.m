## Copyright (C) 1997 Kai P. Mueller
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{retval} =} is_abcd (@var{a}, @var{b}, @var{c}, @var{d})
##  Returns @var{retval} = 1 if the dimensions of @var{a}, @var{b},
##  @var{c}, @var{d} are compatible, otherwise @var{retval} = 0 with an
##  appropriate diagnostic message printed to the screen.  The matrices
##  @var{b}, @var{c}, or @var{d} may be omitted.
## @seealso{abcddim}
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: November 4, 1997
## based on is_controllable.m of Scottedward Hodel

function retval = is_abcd (a, b, c, d)

  retval = 0;
  switch (nargin)
    case (1)
      ## A only
      [na, ma] = size(a);
      if (na != ma)
        disp("Matrix A ist not square.")
      endif
    case (2)
      ## A, B only
      [na, ma] = size(a);  [nb, mb] = size(b);
      if (na != ma)
        disp("Matrix A ist not square.")
        return;
      endif
      if (na != nb)
        disp("A and B column dimension different.")
        return;
      endif
    case (3)
      ## A, B, C only
      [na, ma] = size(a);  [nb, mb] = size(b);  [nc, mc] = size(c);
      if (na != ma)
        disp("Matrix A ist not square.")
        return;
      endif
      if (na != nb)
        disp("A and B column dimensions not compatible.")
        return;
      endif
      if (ma != mc)
        disp("A and C row dimensions not compatible.")
        return;
      endif
    case (4)
      ## all matrices A, B, C, D
      [na, ma] = size(a);  [nb, mb] = size(b);
      [nc, mc] = size(c);  [nd, md] = size(d);
      if (na != ma)
        disp("Matrix A ist not square.")
        return;
      endif
      if (na != nb)
        disp("A and B column dimensions not compatible.")
        return;
      endif
      if (ma != mc)
        disp("A and C row dimensions not compatible.")
        return;
      endif
      if (mb != md)
        disp("B and D row dimensions not compatible.")
        return;
      endif
      if (nc != nd)
        disp("C and D column dimensions not compatible.")
        return;
      endif
    otherwise
      print_usage ();
  endswitch
  ## all tests passed, signal ok.
  retval = 1;
endfunction
