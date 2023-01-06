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
## @deftypefn  {} {@var{names} =} fieldnames (@var{struct})
## @deftypefnx {} {@var{names} =} fieldnames (@var{obj})
## @deftypefnx {} {@var{names} =} fieldnames (@var{javaobj})
## @deftypefnx {} {@var{names} =} fieldnames ("@var{javaclassname}")
## Return a cell array of strings with the names of the fields in the specified
## input.
##
## When the input is a structure @var{struct}, the @var{names} are the elements
## of the structure.
##
## When the input is an Octave object @var{obj}, the @var{names} are the public
## properties of the object.
##
## When the input is a Java object @var{javaobj} or a string containing the
## name of a Java class @var{javaclassname}, the @var{names} are the public
## fields (data members) of the object or class.
## @seealso{numfields, isfield, orderfields, struct, properties}
## @end deftypefn

function names = fieldnames (obj)

  if (nargin < 1)
    print_usage ();
  endif

  if (isstruct (obj))
    names = __fieldnames__ (obj);
  elseif (isobject (obj))
    try
      names = properties (obj);      # classdef object
    catch
      names = __fieldnames__ (obj);  # @class object
    end_try_catch
  elseif (isjava (obj) || ischar (obj))
    ## FIXME: Function prototype that accepts java obj exists, but doesn't
    ##        work if obj is java.lang.String.  Convert obj to classname.
    ## FIXME: this is now working for objects whose class is in the dynamic
    ##        classpath but will continue to fail if such classnames are used
    ##        instead (see bug #42710).
    if (isa (obj, "java.lang.String"))
      obj = class (obj);
    endif
    names_str = javaMethod ("getFields", "org.octave.ClassHelper", obj);
    names = ostrsplit (names_str, ';');
  else
    error ("fieldnames: Invalid input argument");
  endif

endfunction


## Test preservation of fieldname order
%!test
%! x(3).d=1;  x(2).a=2;  x(1).b=3;  x(2).c=3;
%! assert (fieldnames (x), {"d"; "a"; "b"; "c"});

## Test empty structure
%!test
%! s = struct ();
%! assert (fieldnames (s), cell (0, 1));

## Test classdef object
%!test
%! m = containers.Map ();
%! f = fieldnames (m);
%! assert (f, {"Count"; "KeyType"; "ValueType"; "map"; "numeric_keys"});

## Test old-style @class object
%!test
%! obj = ftp ();
%! f = fieldnames (obj);
%! assert (f, {"host"; "username"; "password"; "curlhandle"});

## Test Java classname by passing classname
%!testif HAVE_JAVA; usejava ("jvm")
%! names = fieldnames ("java.lang.Double");
%! assert (any (strcmp (names, "MAX_VALUE")));

## Test Java classname by passing java object
%!testif HAVE_JAVA; usejava ("jvm")
%! names = fieldnames (javaObject ("java.lang.Double", 10));
%! assert (any (strcmp (names, "MAX_VALUE")));
%! assert (all (ismember ({"POSITIVE_INFINITY", "NEGATIVE_INFINITY", ...
%!                         "NaN", "MAX_VALUE", "MIN_NORMAL", "MIN_VALUE", ...
%!                         "MAX_EXPONENT", "MIN_EXPONENT", "SIZE", "TYPE"},
%!                        names)));

%!testif HAVE_JAVA; usejava ("jvm")
%! names = fieldnames (javaObject ("java.lang.String", "Hello"));
%! assert (any (strcmp (names, "CASE_INSENSITIVE_ORDER")));
