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
## @deftypefn {Function File} {} weibull_rnd (@var{alpha}, @var{sigma}, @var{r}, @var{c})
## @deftypefnx {Function File} {} weibull_rnd (@var{alpha}, @var{sigma}, @var{sz})
##
## This function has been deprecated.  Use weibrnd instead.
## 
## Return an @var{r} by @var{c} matrix of random samples from the
## Weibull distribution with parameters @var{alpha} and @var{sigma}
## which must be scalar or of size @var{r} by @var{c}. Or if @var{sz}
## is a vector return a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{alpha} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Weibull distribution

function rnd = weibull_rnd (varargin)

  rnd = weibrnd(varargin{:});

endfunction
