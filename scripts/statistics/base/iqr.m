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
## @deftypefn {Function File} {} iqr (@var{x})
## If @var{x} is a vector, return the interquartile range, i.e., the
## difference between the upper and lower quartile, of the input data.
##
## If @var{x} is a matrix, do the above for each column of @var{x}.
## @end deftypefn

## Author KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Interquartile range

function y = iqr (x)

  if (nargin != 1)
    usage ("iqr (x)");
  endif

 Å†if (rows (x) == 1)
 Å† Å†x = x.';
 Å†endif

 Å†[r, c] = size (x);
 Å†y = zeros (1, c);

 Å†for i = 1:c;
 Å† Å†y(i) = empirical_inv (3/4, x(:,i)) - empirical_inv (1/4, x(:,i));
 Å†endfor

endfunction
