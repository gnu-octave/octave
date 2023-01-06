########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{tf} =} isgraphics (@var{h})
## @deftypefnx {} {@var{tf} =} isgraphics (@var{h}, @var{type})
## Return true if @var{h} is a graphics handle (of type @var{type}) and false
## otherwise.
##
## When no @var{type} is specified the function is equivalent to
## @code{ishghandle}.
## @seealso{ishghandle, ishandle, isaxes, isfigure}
## @end deftypefn

function tf = isgraphics (h, type = "")

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 2 && ! (ischar (type) && isrow (type)))
    error ("isgraphics: TYPE must be a string");
  endif

  ## Octave has no Simulink equivalent so it is sufficient to call ishghandle.
  tf = ishghandle (h);

  if (nargin == 2 && any (tf))
    tf(tf) = strcmpi (get (h(tf), "type"), type);
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (isgraphics (hf));
%!   assert (isgraphics (hf, "figure"));
%!   assert (! isgraphics (-hf));
%!   assert (! isgraphics (hf, "foo"));
%!   l = line ();
%!   ax = gca ();
%!   assert (isgraphics (ax));
%!   assert (isgraphics (ax, "axes"));
%!   assert (! isgraphics (-ax));
%!   assert (! isgraphics (ax, "foo"));
%!   assert (isgraphics (l));
%!   assert (isgraphics (l, "line"));
%!   assert (! isgraphics (-l));
%!   assert (! isgraphics (l, "foo"));
%!   p = patch ();
%!   assert (isgraphics (p));
%!   assert (isgraphics (p, "patch"));
%!   assert (! isgraphics (-p));
%!   assert (! isgraphics (p, "foo"));
%!   s = surface ();
%!   assert (isgraphics (s));
%!   assert (isgraphics (s, "surface"));
%!   assert (! isgraphics (-s));
%!   assert (! isgraphics (s, "foo"));
%!   t = text ();
%!   assert (isgraphics (t));
%!   assert (isgraphics (t, "text"));
%!   assert (! isgraphics (-t));
%!   assert (! isgraphics (t, "foo"));
%!   i = image ([1]);
%!   assert (isgraphics (i));
%!   assert (isgraphics (i, "image"));
%!   assert (! isgraphics (-i));
%!   assert (! isgraphics (i, "foo"));
%!   hg = hggroup ();
%!   assert (isgraphics (hg));
%!   assert (isgraphics (hg, "hggroup"));
%!   assert (! isgraphics (-hg));
%!   assert (! isgraphics (hg, "foo"));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! assert (isgraphics ([-1 0]), [false true]);
%! assert (isgraphics ([-1 0], "root"), [false true]);
%! assert (isgraphics ([-1 0], "foobar"), [false false]);

## Test input validation
%!error <Invalid call> isgraphics ()
%!error <TYPE must be a string> isgraphics (0, 1)
%!error <TYPE must be a string> isgraphics (0, {1})
