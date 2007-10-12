## Copyright (C) 1996, 1998, 2000, 2005, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[a, b] =} zgsgiv (@var{c}, @var{s}, @var{a}, @var{b})
## Apply givens rotation c,s to row vectors @var{a}, @var{b}.
## No longer used in zero-balancing (__zgpbal__); kept for backward
## compatibility.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 29, 1992
## Convertion to Octave by R. Bruce Tenison July 3, 1994

function [a, b] = zgsgiv (c, s, a, b)

  t1 = c*a + s*b;
  t2 = -s*a + c*b;
  a = t1;
  b = t2;

endfunction
