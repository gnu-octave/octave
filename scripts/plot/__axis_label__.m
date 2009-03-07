## Copyright (C) 1996, 1997, 2000, 2003, 2005, 2006, 2007, 2008,
##               2009 John W. Eaton 
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
## @deftypefn {Function File} {} __axis_label__ (@var{caller}, @var{txt}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = __axis_label__ (caller, txt, varargin)

  if (ischar (txt))
    ca = gca ();

    h = get (gca (), caller);

    set (h, "fontangle", get (ca, "fontangle"),
         "fontname", get (ca, "fontname"),
         "fontsize", get (ca, "fontsize"),
         "fontunits", get (ca, "fontunits"),
         "fontweight", get (ca, "fontweight"),
         "string", txt,
	 varargin{:});

    if (nargout > 0)
      retval = h;
    endif
  else
    error ("%s: expecting first argument to be character string", caller);
  endif

endfunction
