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
## @deftypefn  {} {} javarmpath (@var{clspath})
## @deftypefnx {} {} javarmpath (@var{clspath1}, @dots{})
## @deftypefnx {} {} javarmpath (@{@var{clspath1}, @dots{}@})
## Remove @var{clspath} from the dynamic class path of the Java virtual
## machine.
##
## @var{clspath} may either be a directory where @file{.class} files are found,
## or a @file{.jar} file containing Java classes.  Multiple paths may be
## removed at once by specifying additional arguments, or by using a cell array
## of strings.
## @seealso{javaaddpath, javaclasspath}
## @end deftypefn

function javarmpath (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! all (cellfun (@(c) ischar (c) || iscellstr (c), varargin)))
    error ("javarmpath: arguments must be strings or cell array of strings");
  endif

  for arg = varargin
    if (iscellstr (arg{1}))
      arg = arg{1}(:).';  # Guarantee cellstr array is a row vector
    endif

    for clspath = arg
      clspath = clspath{1};

      old_path = canonicalize_file_name (tilde_expand (clspath));
      if (isfolder (old_path))
        if (old_path(end) != filesep ())
          old_path = [old_path, filesep()];
        endif
      endif

      success = javaMethod ("removeClassPath", "org.octave.ClassHelper",
                            old_path);

      if (! success)
        warning ("javarmpath: %s: not found in Java classpath", old_path);
      endif
    endfor
  endfor

endfunction


## FIXME: These tests may fail if either TEMPDIR or HOME have already
##        been added to the Java class path.

## Basic test with single string
%!testif HAVE_JAVA; usejava ("jvm")
%! pth = tempdir ();
%! unwind_protect
%!   javaaddpath (pth);
%!   clspth1 = javaclasspath ("-dynamic");
%!   javarmpath (pth);
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (numel (clspth2), numel (clspth1) - 1);
%!   assert (clspth2(1:end), clspth1(2:end));
%! unwind_protect_cleanup
%!   javarmpath (pth);
%! end_unwind_protect

## Remove two strings
%!testif HAVE_JAVA; usejava ("jvm")
%! pth1 = tempdir ();
%! pth2 = tilde_expand ("~");
%! unwind_protect
%!   javaaddpath (pth1, pth2);
%!   clspth1 = javaclasspath ("-dynamic");
%!   javarmpath (pth1, pth2);
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (numel (clspth2), numel (clspth1) - 2);
%!   assert (clspth2(1:end), clspth1(3:end));
%! unwind_protect_cleanup
%!   javarmpath (pth1, pth2);
%! end_unwind_protect

## Remove cell array of two strings
%!testif HAVE_JAVA; usejava ("jvm")
%! pth1 = tempdir ();
%! pth2 = tilde_expand ("~");
%! unwind_protect
%!   javaaddpath (pth1, pth2);
%!   clspth1 = javaclasspath ("-dynamic");
%!   javarmpath ({pth1, pth2});
%!   clspth2 = javaclasspath ("-dynamic");
%!   assert (numel (clspth2), numel (clspth1) - 2);
%!   assert (clspth2(1:end), clspth1(3:end));
%! unwind_protect_cleanup
%!   javarmpath (pth1, pth2);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> javarmpath ()
%!error <arguments must be strings> javarmpath (5)
%!error <arguments must be .* cell array of strings> javarmpath ({5})
