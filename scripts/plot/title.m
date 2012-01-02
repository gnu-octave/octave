## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefnx {Function File} {} title (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} title (@dots{})
## Create a title object for a plot.
##
## The optional return value @var{h} is a graphics handle to the created object.
## @end deftypefn

## Author: jwe

function retval = title (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("title", varargin{:});

  if (rem (nargin, 2) != 1)
    print_usage ();
  endif

  tmp = __axis_label__ (h, "title", varargin{:});

  if (nargout > 0)
    retval = tmp;
  endif

endfunction


%!demo
%! clf ();
%! ax = axes();
%! xl = get (ax,"title");
%! title ("Testing title");
%! assert (get (xl,"string"), "Testing title");

%!demo
%! clf ();
%! plot3 ([0,1], [0,1], [0,1]);
%! xl = get(gca (), "title");
%! title ("Testing title");
%! assert (get (xl,"string"),"Testing title");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ax = axes();
%!   xl = get (ax,"title");
%!   title ("Testing title");
%!   assert (get (xl,"string"), "Testing title");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1], [0,1]);
%!   xl = get (gca (), "title");
%!   title("Testing title");
%!   assert (get (xl,"string"), "Testing title");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

