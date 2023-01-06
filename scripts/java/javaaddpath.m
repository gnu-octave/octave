########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} javaaddpath (@var{clspath})
## @deftypefnx {} {} javaaddpath (@var{clspath1}, @dots{})
## @deftypefnx {} {} javaaddpath (@{@var{clspath1}, @dots{}@})
## @deftypefnx {} {} javaaddpath (@dots{}, "-end")
## Add @var{clspath} to the beginning of the dynamic class path of the
## Java virtual machine.
##
## @var{clspath} may either be a directory where @file{.class} files are
## found, or a @file{.jar} file containing Java classes.  Multiple paths may
## be added at once by specifying additional arguments, or by using a cell
## array of strings.
##
## If the final argument is @qcode{"-end"}, append the new element to the
## end of the current classpath.
##
## @seealso{javarmpath, javaclasspath}
## @end deftypefn

function javaaddpath (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! all (cellfun (@(c) ischar (c) || iscellstr (c), varargin)))
    error ("javaaddpath: arguments must be strings or cell array of strings");
  endif

  if (strcmp (varargin{end}, "-end"))
    at_end = true;
    varargin(end) = [];
  else
    ## Note that when prepending, we iterate over the arguments in
    ## reverse so that a call like
    ##
    ##   javaaddpath ("/foo", "/bar")
    ##
    ## results in "/foo" first in the path followed by "/bar".
    at_end = false;
    varargin = fliplr (varargin);
  endif

  for arg = varargin
    if (iscellstr (arg{1}))
      if (at_end)
        ## Guarantee cellstr array is a row vector
        arg = arg{1}(:).';
      else
        ## Iterate in reverse over cell arrays
        arg = fliplr (arg{1}(:).');
      endif
    endif

    for clspath = arg
      clspath = clspath{1};

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
  endfor

endfunction


## FIXME: These tests may fail if either TEMPDIR or HOME have already
##        been added to the Java class path.

## Basic prepend test with single string
%!testif HAVE_JAVA; usejava ("jvm")
%! pth = tempdir ();
%! unwind_protect
%!   clspth1 = javaclasspath ("-dynamic");
%!   javaaddpath (pth);
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (clspth2{1}, canonicalize_file_name (pth));
%!   assert (clspth2(2:end), clspth1(1:end));
%! unwind_protect_cleanup
%!   javarmpath (pth);
%! end_unwind_protect

## Prepend test with two strings
%!testif HAVE_JAVA; usejava ("jvm")
%! pth1 = tempdir ();
%! pth2 = tilde_expand ("~");
%! unwind_protect
%!   clspth1 = javaclasspath ("-dynamic");
%!   javaaddpath (pth1, pth2);
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (clspth2([1, 2]),
%!           {canonicalize_file_name(pth1), canonicalize_file_name(pth2)});
%!   assert (clspth2(3:end), clspth1(1:end));
%! unwind_protect_cleanup
%!   javarmpath (pth1, pth2);
%! end_unwind_protect

## Prepend test with cell array of two strings
%!testif HAVE_JAVA; usejava ("jvm")
%! pth1 = tempdir ();
%! pth2 = tilde_expand ("~");
%! unwind_protect
%!   clspth1 = javaclasspath ("-dynamic");
%!   javaaddpath ({pth1, pth2});
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (clspth2([1, 2]),
%!           {canonicalize_file_name(pth1), canonicalize_file_name(pth2)});
%!   assert (clspth2(3:end), clspth1(1:end));
%! unwind_protect_cleanup
%!   javarmpath (pth1, pth2);
%! end_unwind_protect

## Append test with two strings
%!testif HAVE_JAVA; usejava ("jvm")
%! pth1 = tempdir ();
%! pth2 = tilde_expand ("~");
%! unwind_protect
%!   clspth1 = javaclasspath ("-dynamic");
%!   javaaddpath (pth1, pth2, "-end");
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (clspth2([end-1, end]),
%!           {canonicalize_file_name(pth1), canonicalize_file_name(pth2)});
%!   assert (clspth2(1:end-2), clspth1(1:end));
%! unwind_protect_cleanup
%!   javarmpath (pth1, pth2);
%! end_unwind_protect

## Append test with cell array of two strings
%!testif HAVE_JAVA; usejava ("jvm")
%! pth1 = tempdir ();
%! pth2 = tilde_expand ("~");
%! unwind_protect
%!   clspth1 = javaclasspath ("-dynamic");
%!   javaaddpath ({pth1, pth2}, "-end");
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (clspth2([end-1, end]),
%!           {canonicalize_file_name(pth1), canonicalize_file_name(pth2)});
%!   assert (clspth2(1:end-2), clspth1(1:end));
%! unwind_protect_cleanup
%!   javarmpath (pth1, pth2);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> javaaddpath ()
%!error <arguments must be strings> javaaddpath (5)
%!error <arguments must be .* cell array of strings> javaaddpath ({5})
%!error <CLSPATH does not exist> javaaddpath ("%_A_Very_Unlikely_Name_%")
