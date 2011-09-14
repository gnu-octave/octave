## Copyright (C) 1993-2011 John W. Eaton
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
## @deftypefn  {Function File} {} title (@var{string})
## @deftypefnx {Function File} {} title (@var{string}, @var{p1}, @var{v1}, @dots{})
## Create a title object and return a handle to it.
## @end deftypefn

## Author: jwe

function h = title (string, varargin)

  if (rem (nargin, 2) == 1)
    if (nargout > 0)
      h = __axis_label__ ("title", string, varargin{:});
    else
      __axis_label__ ("title", string, varargin{:});
    endif
  else
    print_usage ();
  endif

endfunction

%!demo
%! clf ();
%! ax=axes();
%! xl = get(ax,"title");
%! title("Testing title")
%! assert(get(xl,"string"),"Testing title")

%!demo
%! clf ();
%! plot3 ([0,1], [0,1], [0,1]);
%! xl = get(gca (), "title");
%! title("Testing title")
%! assert(get(xl,"string"),"Testing title")

%!test
%! hf = figure ("visible", "off");
%! unwind_protect  
%!   ax=axes();
%!   xl = get(ax,"title");
%!   title("Testing title")
%!   assert(get(xl,"string"),"Testing title")
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect  
%!   plot3 ([0,1], [0,1], [0,1]);
%!   xl = get(gca (), "title");
%!   title("Testing title")
%!   assert(get(xl,"string"),"Testing title")
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
