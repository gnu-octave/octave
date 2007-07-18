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
## @deftypefn {Function File} {} exponential_rnd (@var{lambda}, @var{r}, @var{c})
## @deftypefnx {Function File} {} exponential_rnd (@var{lambda}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## exponential distribution with parameter @var{lambda}, which must be a
## scalar or of size @var{r} by @var{c}. Or if @var{sz} is a vector, 
## create a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the size of @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the exponential distribution

function rnd = exponential_rnd (varargin)

 if (nargin > 0)
   varargin{1} = 1 ./ varargin{1};
 endif

 rnd =  exprnd (varargin{:});

endfunction
