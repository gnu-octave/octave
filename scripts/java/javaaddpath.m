## Copyright (C) 2007-2019 Michael Goffioul
## Copyright (C) 2010-2019 Martin Hepperle
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

## -*- texinfo -*-
## @deftypefn  {} {} javaaddpath (@var{clspath})
## @deftypefnx {} {} javaaddpath (@var{clspath1}, @dots{})
## @deftypefnx {} {} javaaddpath (@dots{}, "-end")
## Add @var{clspath} to the dynamic class path of the Java virtual machine.
##
## @var{clspath} may either be a directory where @file{.class} files are
## found, or a @file{.jar} file containing Java classes.  Multiple paths may
## be added at once by specifying additional arguments.
##
## If the final argument is @code{"-end"}, append the new element to the
## end of the current classpath.  Otherwise, new elements are added at
## the beginning of the path.
## @seealso{javarmpath, javaclasspath}
## @end deftypefn

function javaaddpath (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! iscellstr (varargin))
    error ("javaaddpath: arguments must all be character strings");
  endif

  if (strcmp (varargin{end}, "-end"))
    at_end = true;
    nel = nargin - 1;
    rng = 1:nel;
  else
    ## Note that when prepending, we iterate over the arguments in
    ## reverse so that a call like
    ##
    ##   javaaddpath ("/foo", "/bar")
    ##
    ## results in "/foo" first in the path followed by "/bar".
    nel = nargin;
    at_end = false;
    rng = nel:-1:1;
  endif

  for i = rng
    clspath = varargin{i};
    new_path = canonicalize_file_name (tilde_expand (clspath));
    if (isfolder (new_path))
      if (new_path(end) != filesep ())
        new_path = [new_path, filesep()];
      endif
    elseif (! exist (new_path, "file"))
      error ("javaaddpath: CLSPATH does not exist: %s", clspath);
    endif

    success = javaMethod ("addClassPath", "org.octave.ClassHelper",
                          new_path, at_end);

    if (! success)
      warning ("javaaddpath: failed to add '%s' to Java classpath", new_path);
    endif
  endfor

endfunction
