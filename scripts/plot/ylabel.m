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
## @deftypefn  {Function File} {} ylabel (@var{string})
## @deftypefnx {Function File} {} ylabel (@var{h}, @var{string})
## @deftypefnx {Function File} {@var{h} =} ylabel (@dots{})
## @seealso{xlabel}
## @end deftypefn

## Author: jwe

function retval = ylabel (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("ylabel", varargin{:});

  if (rem (nargin, 2) != 1)
    print_usage ();
  endif

  tmp = __axis_label__ (h, "ylabel", varargin{:},
                        "color", get (h, "ycolor"));

  if (nargout > 0)
    retval = tmp;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   y = ylabel ("ylabel_string");
%!   assert (get (gca, "ylabel"), y);
%!   assert (get (y, "type"), "text");
%!   assert (get (y, "visible"), "on");
%!   assert (get (y, "string"), "ylabel_string");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

