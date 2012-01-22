## Copyright (C) 2012 John W. Eaton
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
## @deftypefn  {Function File} {[@var{int}, @var{err}, @var{nr_points}] =} cquad (@var{f}, @var{a}, @var{b}, @var{tol})
## @deftypefnx {Function File} {[@var{int}, @var{err}, @var{nr_points}] =} cquad (@var{f}, @var{a}, @var{b}, @var{tol}, @var{sing})
## This function is an alias for compatibility with older versions of
## Octave.  New programs should use @code{quadcc} instead.
## @seealso{quadcc}
## @end deftypefn

## Deprecated in version 3.4

function retval = cquad (varargin)
  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "cquad has been renamed to quadcc and this alias will be removed from a future version of Octave; please use quadcc instead");
  endif

  retval = quadcc (varargin{:});

endfunction
