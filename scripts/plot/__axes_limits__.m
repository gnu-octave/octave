## Copyright (C) 2007 David Bateman
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

## Undocumented internal function.

function retval = __axes_limits__ (fcn, varargin)
  retval = [];
  fcnmode = sprintf("%smode", fcn);

  if (nargin > 1 && isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin{1};
    off = 1;
    if (! strcmp (get (h, "type"), "axes"))
      error ("%s: expecting first argument to be an axes object", fcn);
    endif
  else
    off = 0;
    h = gca ();
  endif

  if (nargin == off + 1)
    retval = get (h, fcn);
  else
    arg = varargin{off + 1};

    if (ischar (arg))
      arg = tolower (arg);
      if (strcmp ("mode", arg))

	retval = get (h, fcnmode);
      elseif (strcmp ("auto", arg) ||  strcmp ("manual", arg))  
	set (h, fcnmode, arg);
      endif
    else
      if (!isnumeric (arg) && any (size(arg(:)) != [2, 1]))
	error ("%s: argument must be a 2 element vector", fcn);
      else
	set (h, fcn, arg (:));
      endif
    endif
  endif
endfunction
