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
## @deftypefn {} {@var{tf} =} ishandle (@var{h})
## Return true if @var{h} is a handle to a graphics or Java object and false
## otherwise.
##
## @var{h} may also be a matrix of handles in which case a logical array is
## returned that is true where the elements of @var{h} are handles to graphics
## or Java objects and false where they are not.
##
## Programming Note: It is often more useful to test for a specific object
## type.  To determine if a handle belongs to a graphics object use
## @code{ishghandle} or @code{isgraphics}.  To determine if a handle belongs
## to a Java object use @code{isjava}.
## @seealso{ishghandle, isgraphics, isjava}
## @end deftypefn

function tf = ishandle (h)

  if (nargin < 1)
    print_usage ();
  endif

  tf = ishghandle (h) | isjava (h);

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (ishandle (hf));
%!   assert (! ishandle (-hf));
%!   ax = gca ();
%!   l = line ();
%!   assert (ishandle (ax));
%!   assert (! ishandle (-ax));
%!   assert (ishandle ([l, -1, ax, hf]), logical ([1, 0, 1, 1]));
%!   assert (ishandle ([l, -1, ax, hf]'), logical ([1, 0, 1, 1]'));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!assert (ishandle ([-1 0]), [false true])

%!testif HAVE_JAVA; usejava ("jvm")
%! jobj = javaObject ("java.lang.Double", 1.0);
%! assert (ishandle (jobj));

## Test input validation
%!error <Invalid call> ishandle ()
