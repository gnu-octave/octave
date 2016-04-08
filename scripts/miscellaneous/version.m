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
## Get version information for Octave
##
## If called without input argument, the first return value @var{v} gives the
## version number of Octave as a string. The second return value @var{d} holds
## the release date as a string.
##
## The following options can be passed for @var{feature}:
## @table @asis
## @item @qcode{"-date"}
## for the release date of the running build,
##
## @item @qcode{"-description"}
## for a description of the release (empty string),
##
## @item @qcode{"-release"}
## for the name of the running build,
##
## @item @qcode{"-java"}
## for version information of the Java VM,
##
## @item @qcode{"-blas"}
## for version information for the linked BLAS (not implemented),
##
## @item @qcode{"-lapack"}
## for version information for the linked LAPACK (not implemented).
## @end table
##
## The variant with no input and output argument is an alias for the function
## @w{@env{OCTAVE_VERSION}} provided for compatibility.
## @seealso{OCTAVE_VERSION, ver}
## @end deftypefn

## Author: jwe

function [vs, d] = version (feature)

  if (nargin > 1 || ((nargin != 0) && ((nargout > 1) || ! ischar (feature))))
    print_usage ();
  endif

  if (nargin == 0)
    vs = OCTAVE_VERSION;

    if (nargout > 1)
      d = __octave_config_info__.releasedate;
    end
  else
    switch (feature)
      case "-date"
        vs = __octave_config_info__.releasedate;
      case "-description"
        vs = "";
      case "-release"
        vs = "";
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
          vs = ["Java " jversion " with " jvendor " " jname " " jjitmode];
        catch
          vs = "no java available";
        end_try_catch
      case "-blas"
        vs = "";
        warning(["version: option '" feature "' not implemented"])
      case "-lapack"
        vs = "";
        warning(["version: option '" feature "' not implemented"])
      otherwise
        error ("version: Invalid input argument");
    endswitch
  endif


endfunction


%!assert (ischar (version ()))

%!test
%! [v, d] = version ();
%! assert (v, OCTAVE_VERSION)
%! assert (d, __octave_config_info__.releasedate)

%!assert (version ("-date"), __octave_config_info__.releasedate)

%!error version (1);

