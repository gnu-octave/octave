## Copyright (C) 1996, 1997, 2005, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} hypergeometric_pdf (@var{x}, @var{m}, @var{t}, @var{n})
## Compute the probability density function (PDF) at @var{x} of the
## hypergeometric distribution with parameters @var{m}, @var{t}, and
## @var{n}. This is the probability of obtaining @var{x} marked items
## when randomly drawing a sample of size @var{n} without replacement
## from a population of total size @var{t} containing @var{m} marked items.
##
## The arguments must be of common size or scalar.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the hypergeometric distribution

## Deprecated in version 3.0

function pdf = hypergeometric_pdf (varargin)

 pdf =  hygepdf (varargin{:});

endfunction
