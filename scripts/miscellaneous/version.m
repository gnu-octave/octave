########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{v} =} version ()
## @deftypefnx {} {[@var{v}, @var{d}] =} version ()
## @deftypefnx {} {@var{v} =} version (@var{feature})
## Get version information for Octave.
##
## If called without input argument, the first return value @var{v} gives the
## version number of Octave as a string.  The second return value @var{d} holds
## the release date as a string.
##
## The following options can be passed for @var{feature}:
##
## @table @asis
## @item @qcode{"-date"}
## for the release date of the running build,
##
## @item @qcode{"-description"}
## for a description of the release (always an empty string),
##
## @item @qcode{"-release"}
## for the name of the running build (always an empty string),
##
## @item @qcode{"-java"}
## for version information of the Java @nospell{VM},
##
## @item @qcode{"-fftw"}
## for version information for the linked @sc{fftw},
##
## @item @qcode{"-blas"}
## for version information for the linked @sc{blas},
##
## @item @qcode{"-lapack"}
## for version information for the linked @sc{lapack}.
##
## @item @qcode{"-hgid"}
## the mercurial ID of the sources used to build Octave.
## @end table
##
## The information returned for the @qcode{"-blas"} and @qcode{"-lapack"}
## options might be unreliable.  It might report which library was linked in
## when Octave was built instead of which library is currently used.
##
## The variant with no input and output argument is an alias for the function
## @w{@env{OCTAVE_VERSION}} provided for compatibility.
## @seealso{OCTAVE_VERSION, ver}
## @end deftypefn

function [v, d] = version (feature)

  if (nargin == 1 && (nargout > 1 || ! ischar (feature)))
    print_usage ();
  endif

  if (nargin == 0)
    v = OCTAVE_VERSION ();

    if (nargout > 1)
      d = __octave_config_info__ ("release_date");
    endif
  else
    switch (lower (feature))
      case "-date"
        v = __octave_config_info__ ("release_date");
      case "-description"
        v = "";
      case "-release"
        v = "";
      case "-java"
        try
          jversion = javaMethod ("getProperty", "java.lang.System", ...
                                 "java.runtime.version");
          jvendor = javaMethod ("getProperty", "java.lang.System", ...
                                "java.vendor");
          jname = javaMethod ("getProperty", "java.lang.System", ...
                              "java.vm.name");
          jjitmode = javaMethod ("getProperty", "java.lang.System", ...
                                 "java.vm.info");
          v = ["Java " jversion " with " jvendor " " jname " " jjitmode];
        catch err
          v = sprintf ("no usable Java Runtime Environment (%s) found:\n%s", ...
                       uname ().machine, err.message);
        end_try_catch
      case "-fftw"
        v = __octave_config_info__ ("fftw_version");
      case "-blas"
        v = __blas_version__ ();
      case "-lapack"
        v = __lapack_version__ ();
      case "-hgid"
        v = __octave_config_info__ ("hg_id");
      otherwise
        error ("version: invalid FEATURE");
    endswitch
  endif

endfunction


%!assert (ischar (version ()))

%!test
%! [v, d] = version ();
%! assert (v, OCTAVE_VERSION);
%! assert (d, __octave_config_info__ ("release_date"));

%!assert (version ("-date"), __octave_config_info__ ("release_date"))

%!assert (version ("-description"), "")
%!assert (version ("-release"), "")
%!assert (ischar (version ("-blas")))
%!assert (ischar (version ("-LAPACK")))

## Test input validation
%!error version ("-date", "-release")
%!error [v, d] = version ("-date")
%!error version (1)
%!error <invalid FEATURE> version ("-foobar")
