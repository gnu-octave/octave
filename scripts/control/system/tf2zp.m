## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{zer}, @var{pol}, @var{k}] =} tf2zp (@var{num}, @var{den})
## Converts transfer functions to poles-and-zero representations.
##
## Returns the zeros and poles of the @acronym{SISO} system defined 
## by @var{num}/@var{den}.
## @var{k} is a gain associated with the system zeros.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>

function [zer, pol, k] = tf2zp (num, den)

  if (nargin == 2)
    if (length (den) > 1)
      pol = roots (den);
    else
      pol = [];
    endif

    if (length (num) > 1)
      zer = roots (num);
    else
      zer = [];
    endif
  else
    error ("Incorrect number of input arguments");
  endif

  [a, b, c, d] = tf2ss (num, den);
  [dum, k] = tzero (a, b, c, d);

endfunction
