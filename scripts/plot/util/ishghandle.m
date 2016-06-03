## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {} {} ishghandle (@var{h})
## @deftypefnx {} {} ishghandle (@var{h}, @var{type})
## Return true if @var{h} is a graphics handle (of type @var{type}) and false
## otherwise.
##
## When no @var{type} is specified the function is equivalent to
## @code{ishandle}.
## @seealso{ishandle}
## @end deftypefn

function retval = ishghandle (h, type = "")

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  
  if (nargin == 2 && (! ischar (type) || ! isrow (type)))
    error ("ishghandle: TYPE must be a string");
  endif
  
  ## Octave has no Simulink equivalent so it is sufficient to call ishandle.
  retval = ishandle (h);
  
  if (nargin == 2 && any (retval))
    typematch = strcmpi (get (h(retval), "type"), type);
    retval(retval) = typematch;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (ishghandle (hf));
%!   assert (ishghandle (hf, "figure"));
%!   assert (! ishghandle (-hf));
%!   assert (! ishghandle (hf, "foo"));
%!   l = line;
%!   ax = gca;
%!   assert (ishghandle (ax));
%!   assert (ishghandle (ax, "axes"));
%!   assert (! ishghandle (-ax));
%!   assert (! ishghandle (ax, "foo"));
%!   assert (ishghandle (l));
%!   assert (ishghandle (l, "line"));
%!   assert (! ishghandle (-l));
%!   assert (! ishghandle (l, "foo"));
%!   p = patch;
%!   assert (ishghandle (p));
%!   assert (ishghandle (p, "patch"));
%!   assert (! ishghandle (-p));
%!   assert (! ishghandle (p, "foo"));
%!   s = surface;
%!   assert (ishghandle (s));
%!   assert (ishghandle (s, "surface"));
%!   assert (! ishghandle (-s));
%!   assert (! ishghandle (s, "foo"));
%!   t = text;
%!   assert (ishghandle (t));
%!   assert (ishghandle (t, "text"));
%!   assert (! ishghandle (-t));
%!   assert (! ishghandle (t, "foo"));
%!   i = image ([1]);
%!   assert (ishghandle (i));
%!   assert (ishghandle (i, "image"));
%!   assert (! ishghandle (-i));
%!   assert (! ishghandle (i, "foo"));
%!   hg = hggroup;
%!   assert (ishghandle (hg));
%!   assert (ishghandle (hg, "hggroup"));
%!   assert (! ishghandle (-hg));
%!   assert (! ishghandle (hg, "foo"));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! assert (ishghandle ([-1 0]), [false true]);
%! assert (ishghandle ([-1 0], "root"), [false true]);
%! assert (ishghandle ([-1 0], "foobar"), [false false]);

## Test input validation
%!error ishghandle ()
%!error ishghandle (1, 2, 3)
%!error <TYPE must be a string> ishghandle (0, 1)
%!error <TYPE must be a string> ishghandle (0, {1})

