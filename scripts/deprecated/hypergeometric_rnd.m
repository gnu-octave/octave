## Copyright (C) 1997, 2005, 2006, 2007, 2008 Kurt Hornik
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
## @deftypefn {Function File} {} hypergeometric_rnd (@var{m}, @var{t}, @var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} hygernd (@var{m}, @var{t}, @var{n}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## hypergeometric distribution with parameters @var{m}, @var{t},
## and @var{n}.
##
## The parameters @var{m}, @var{t}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Deprecated in version 3.0

function rnd = hypergeometric_rnd (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "hypergeometric_rnd is obsolete and will be removed from a future version of Octave; please use hygernd instead");
  endif

  rnd = hygernd (varargin{:});

endfunction
