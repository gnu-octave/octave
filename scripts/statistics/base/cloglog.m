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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} cloglog (@var{x})
## Return the complementary log-log function of @var{x}, defined as
##
## @example
## - log (- log (@var{x}))
## @end example
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Complementary log-log function

function y = cloglog (x)

  if (nargin != 1)
    usage ("cloglog (x)");
  endif

  y = - log (- log (x));

endfunction