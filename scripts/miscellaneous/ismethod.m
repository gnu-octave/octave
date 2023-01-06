########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tf} =} ismethod (@var{obj}, @var{method})
## @deftypefnx {} {@var{tf} =} ismethod (@var{class_name}, @var{method})
## Return true if the string @var{method} is a valid method of the object
## @var{obj} or of the class @var{clsname}.
## @seealso{isprop, isobject, isjava, methods}
## @end deftypefn

function tf = ismethod (obj, method)

  if (nargin != 2)
    print_usage ();
  endif

  if (! (ischar (obj) || isobject (obj) || isjava (obj)))
    error ("ismethod: first argument must be object or class name");
  endif

  if (! ischar (method))
    error ("ismethod: METHOD must be a string");
  endif

  method_list = methods (obj);

  tf = ismember (method, method_list);

endfunction


%!testif HAVE_JAVA; usejava ("jvm")
%! assert (ismethod (javaObject ("java.lang.String", "Yo"), "hashCode"));

%!assert (ismethod (ftp (), "ascii"))
%!assert (ismethod (inputParser (), "addRequired"))
