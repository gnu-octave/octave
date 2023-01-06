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
## @deftypefn  {} {} methods (@var{obj})
## @deftypefnx {} {} methods ("@var{classname}")
## @deftypefnx {} {} methods (@dots{}, "-full")
## @deftypefnx {} {@var{mtds} =} methods (@dots{})
## List the names of the public methods for the object @var{obj} or the
## named class @var{classname}.
##
## @var{obj} may be an Octave class object or a Java object.
## @var{classname} may be the name of an Octave class or a Java class.
##
## If the optional argument @qcode{"-full"} is given then Octave returns
## full method signatures which include output type, name of method,
## and the number and type of inputs.
##
## When called with no output arguments, @code{methods} prints the list of
## method names to the screen.  Otherwise, the output argument @var{mtds}
## contains the list in a cell array of strings.
## @seealso{ismethod, properties, fieldnames}
## @end deftypefn

function mtds = methods (obj, fullopt)

  if (nargin < 1)
    print_usage ();
  endif

  havesigs = false;
  showsigs = false;
  if (nargin == 2)
    if (! strcmp (fullopt, "-full"))
      error ("methods: invalid option");
    endif
    showsigs = true;
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
      mtds_list = mtds_list(:);  # return a column vector for compatibility
      havesigs = true;
    endif
  elseif (isjava (obj))
    ## If obj is a String or a subclass of String, then get the methods of its
    ## class name, not the methods of the class that may be named by the
    ## content of the string.
    if (isa (obj, "java.lang.String"))
      klass = class (obj);
      mtds_str = javaMethod ("getMethods", "org.octave.ClassHelper", klass);
    else
      mtds_str = javaMethod ("getMethods", "org.octave.ClassHelper", obj);
    endif
    mtds_list = strsplit (mtds_str, ';');
    mtds_list = mtds_list(:);  # return a column vector for compatibility
    havesigs = true;
  else
    error ("methods: invalid input argument");
  endif

  if (havesigs && ! showsigs)
    ## Extract only the method name for ordinary class methods, delete the
    ## return type and the argument list.
    mtds_list = regexprep (mtds_list, '^(?:[^(]+) (\w+) ?\(.*$', '$1');

    ## Extract only the class name for class constructors, delete the optional
    ## "org.example." package prefix and the argument list.
    mtds_list = regexprep (mtds_list, '^(?:[\.\w]+\.)?(\w+) ?\(.*$', '$1');

    mtds_list = unique (mtds_list);
  else
    ## Delete the "org.example." package prefix if present.
    mtds_list = regexprep (mtds_list, '^(?:[\.\w]+\.)(\w+ ?\(.*)$', '$1');
  endif

  if (nargout == 0)
    classname = ifelse (ischar (obj), obj, class (obj));
    printf ("Methods for class %s:\n", classname);
    disp (list_in_columns (mtds_list));
  else
    mtds = mtds_list;
  endif

endfunction


## test old-style @classname
%!test
%! mtds = methods ("ftp");
%! assert (mtds{1}, "ascii");

## test Java classname
%!testif HAVE_JAVA; usejava ("jvm")
%! mtds = methods ("java.lang.Double");
%! search = strfind (mtds, "valueOf");
%! assert (! isempty ([search{:}]));

## test Java classname with -full prototypes
%!testif HAVE_JAVA; usejava ("jvm")
%! mtds = methods ("java.lang.Double", "-full");
%! search = strfind (mtds, "java.lang.Double valueOf");
%! assert (! isempty ([search{:}]));

## test that methods does the right thing when passed a String object
%!testif HAVE_JAVA; usejava ("jvm") <*48758>
%! object = javaObject ("java.lang.String", "java.lang.Integer");
%! assert (methods (object), methods ("java.lang.String"));

## test classdef classname
%!assert (methods ("inputParser"),
%!        {"addOptional"; "addParamValue"; "addParameter";
%!         "addRequired"; "addSwitch"; "add_missing"; "delete";
%!         "disp"; "error"; "is_argname"; "parse"; "validate_arg";
%!         "validate_name"});

## Test input validation
%!error <Invalid call> methods ()
%!error methods ("a", "b", "c")
%!error <invalid option> methods ("ftp", "option1")
%!error <invalid input argument> methods (1)
