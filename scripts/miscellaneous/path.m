## Copyright (C) 1997 John W. Eaton
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
## @deftypefn {Function File} {} path (@dots{})
## Modify or display Octave's @code{LOADPATH}.
##
## If @var{nargin} and @var{nargout} are zero, display the elements of
## Octave's @code{LOADPATH} in an easy to read format.
##
## If @var{nargin} is zero and nargout is greater than zero, return the
## current value of @code{LOADPATH}.
##
## If @var{nargin} is greater than zero, concatenate the arguments,
## separating them with @code{":"}.  Set @code{LOADPATH} to the result
## and also return it.
##
## No checks are made for duplicate elements.
## @end deftypefn

## Author: jwe

function retval = path (varargin)

  if (nargin > 0)
    p = varargin{1};
    for i = 2:nargin
      p = sprintf ("%s:%s", p, varargin{i});
    endfor
    LOADPATH = p;
  endif

  if (LOADPATH(1) == ":")
    p = strcat (DEFAULT_LOADPATH, LOADPATH);
  else
    t = findstr (LOADPATH, "::");
    if (any (t))
      loc = t(1);
      p = strcat (LOADPATH(1:loc), DEFAULT_LOADPATH, LOADPATH(loc+1:end));
    elseif (LOADPATH(end) == ":")
      p = strcat (LOADPATH, DEFAULT_LOADPATH);
    else
      p = LOADPATH;
    endif
  endif

  if (nargin == 0 && nargout == 0)
    puts ("\nOctave's search path contains the following directories:\n\n  ");
    puts (strrep (p, ":", "\n  "));
    puts ("\n\n");
  else
    retval = p;
  endif

endfunction
