########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn {} {@var{res} =} isprop (@var{obj}, "@var{prop}")
## Return true if @var{prop} is a property of the object @var{obj}.
##
## @var{obj} may also be an array of objects in which case @var{res} will be a
## logical array indicating whether each handle has the property @var{prop}.
##
## For plotting, @var{obj} is a handle to a graphics object.  Otherwise,
## @var{obj} should be an instance of a class.  @code{isprop} reports whether
## the class defines a property, but @code{Access} permissions or visibility
## restrictions (@code{Hidden = true}) may prevent use by the programmer.
## @seealso{get, set, properties, ismethod, isobject}
## @end deftypefn

function res = isprop (obj, prop)

  if (nargin != 2)
    print_usage ();
  endif

  if (! ischar (prop))
    error ("isprop: PROP name must be a string");
  endif

  if (isobject (obj))
    ## Separate code for classdef objects because Octave doesn't handle arrays
    ## of objects and so can't use the generic code.
    warning ("off", "Octave:classdef-to-struct", "local");

    all_props = __fieldnames__ (obj);
    res = any (strcmp (prop, all_props));
  else
    warning ("error", "Octave:abbreviated-property-match", "local");

    res = false (size (obj));
    for i = 1:numel (res)
      if (ishghandle (obj(i)))
        try
          get (obj(i), prop);
          res(i) = true;
        end_try_catch
      endif
    endfor
  endif

endfunction


%!assert (isprop (0, "foobar"), false)
%!assert (isprop (0, "screenpixelsperinch"), true)
%!assert (isprop (zeros (2, 3), "visible"), true (2, 3))
%!assert (isprop ([-2, -1, 0], "visible"), [false, false, true])

%!test
%! m = containers.Map ();
%! assert (isprop (m, "KeyType"));
%! assert (! isprop (m, "FooBar"));

%!error <Invalid call> isprop ()
%!error <Invalid call> isprop (1)
%!error <PROP name must be a string> isprop (0, {"visible"})
