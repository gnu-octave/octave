## Copyright (C) 1995, 1996, 1997, 2005, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} f_rnd (@var{m}, @var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} f_rnd (@var{m}, @var{n}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the F
## distribution with @var{m} and @var{n} degrees of freedom.  Both
## @var{m} and @var{n} must be scalar or of size @var{r} by @var{c}.
## If @var{sz} is a vector the random samples are in a matrix of 
## size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{m} and @var{n}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the F distribution

## Deprecated in version 3.0

function rnd = f_rnd (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
        ["f_rnd is obsolete and will be removed from a future\n",
	       "version of Octave, please use frnd instead"]);
  endif

 rnd =  frnd (varargin{:});

endfunction
