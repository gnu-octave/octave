## Copyright (C) 2018 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {} java2mat (@var{javaobj})
## @code{java2mat} is deprecated and will be removed in Octave version 4.8.
##
## For the next two releases, use @code{__java2mat__} if necessary, and file
## a bug report explaining your programming use of @code{java2mat} and how it
## can't be done with other Octave functions.
## @end deftypefn

function retval = java2mat (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "java2mat is obsolete and will be removed from a future version of Octave");
  endif

  retval = __java2mat__ (varargin{:});

endfunction


## No tests needed for alias.
%!assert (1)
