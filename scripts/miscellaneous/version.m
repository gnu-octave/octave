## Copyright (C) 1994-2015 John W. Eaton
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
## for the name of the running build,
##
## @item @qcode{"-java"}
## for version information of the Java VM,
##
## @item @qcode{"-fftw"}
## for version information for the linked @sc{fftw},
##
## @item @qcode{"-blas"}
## for version information for the linked @sc{blas} (not implemented),
##
## @item @qcode{"-lapack"}
## for version information for the linked @sc{lapack} (not implemented).
## @end table
##
## The variant with no input and output argument is an alias for the function
## @w{@env{OCTAVE_VERSION}} provided for compatibility.
## @seealso{OCTAVE_VERSION, ver}
## @end deftypefn

## Author: jwe

function [v, d] = version (feature)

  if (nargin > 1 || ((nargin != 0) && ((nargout > 1) || ! ischar (feature))))
    print_usage ();
  endif

  if (nargin == 0)
    v = OCTAVE_VERSION ();

    if (nargout > 1)
      d = __octave_config_info__ ("release_date");
    end
  else
    switch (feature)
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
        catch
          v = "no java available";
        end_try_catch
      case "-fftw"
        v = __octave_config_info__ ("fftw_version");
      case "-blas"
        v = "";
        warning ("version: option '-blas' not implemented");
      case "-lapack"
        v = "";
        warning ("version: option '-lapack' not implemented");
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

## Test input validation
%!error version ("-date", "-release")
%!error [v, d] = version ("-date")
%!error version (1)
%!warning <option '-blas' not implemented> version ("-blas");
%!warning <option '-lapack' not implemented> version ("-lapack");
%!error <invalid FEATURE> version ("-foobar")

