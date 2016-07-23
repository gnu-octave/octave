## Copyright (C) 2012-2015 Rik Wehbring
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
## @deftypefn  {} {} methods (@var{obj})
## @deftypefnx {} {} methods ("@var{classname}")
## @deftypefnx {} {@var{mtds} =} methods (@dots{})
## List the names of the public methods for the object @var{obj} or the
## named class @var{classname}.
##
## @var{obj} may be an Octave class object or a Java object.
## @var{classname} may be the name of an Octave class or a Java class.
##
## When called with no output arguments, @code{methods} prints the list of
## method names to the screen.  Otherwise, the output argument @var{mtds}
## contains the list in a cell array of strings.
## @seealso{fieldnames}
## @end deftypefn

function mtds = methods (obj)

  if (nargin != 1)
    print_usage ();
  endif

  if (isobject (obj))
    ## Call internal C++ function for Octave objects
    mtds_list = __methods__ (obj);
  elseif (ischar (obj))
    ## Could be a classname for an Octave class or Java class.
    ## Try Octave class first.
    mtds_list = __methods__ (obj);
    if (isempty (mtds_list))
      mtds_str = javaMethod ("getMethods", "org.octave.ClassHelper", obj);
      mtds_list = ostrsplit (mtds_str, ';');
    endif
  elseif (isjava (obj))
    ## FIXME: Function prototype accepts java obj, but doesn't work if obj
    ##        is e.g., java.lang.String.  Convert obj to classname then.
    try
      mtds_str = javaMethod ("getMethods", "org.octave.ClassHelper", obj);
    catch
      obj = class (obj);
      mtds_str = javaMethod ("getMethods", "org.octave.ClassHelper", obj);
    end_try_catch
    mtds_list = strsplit (mtds_str, ';');
  else
    error ("methods: Invalid input argument");
  endif

  if (nargout == 0)
    classname = ifelse (ischar (obj), obj, class (obj));
    printf ("Methods for class %s:\n", classname);
    disp (list_in_columns (mtds_list));
  else
    mtds = mtds_list;
  endif

endfunction


## test Octave classname
%!test
%! mtds = methods ("ftp");
%! assert (mtds{1}, "ascii");

## test Java classname
%!testif HAVE_JAVA
%! mtds = methods ("java.lang.Double");
%! search = strfind (mtds, "java.lang.Double valueOf");
%! assert (! isempty ([search{:}]));

