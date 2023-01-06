########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn  {} {} computer ()
## @deftypefnx {} {@var{comp} =} computer ()
## @deftypefnx {} {[@var{comp}, @var{maxsize}] =} computer ()
## @deftypefnx {} {[@var{comp}, @var{maxsize}, @var{endian}] =} computer ()
## @deftypefnx {} {@var{arch} =} computer ("arch")
## Print or return a string of the form @var{cpu}-@var{vendor}-@var{os} that
## identifies the type of computer that Octave is running on.
##
## If invoked with an output argument, the value is returned instead of
## printed.  For example:
##
## @example
## @group
## computer ()
##    @print{} x86_64-pc-linux-gnu
##
## mycomp = computer ()
##    @result{} mycomp = x86_64-pc-linux-gnu
## @end group
## @end example
##
## If two output arguments are requested, also return the maximum number of
## elements for an array.  This will depend on whether Octave has been
## compiled with 32-bit or 64-bit index vectors.
##
## If three output arguments are requested, also return the byte order of the
## current system as a character (@qcode{"B"} for big-endian or @qcode{"L"}
## for little-endian).
##
## If the argument @qcode{"arch"} is specified, return a string indicating the
## architecture of the computer on which Octave is running.
##
## Results may be different if Octave was invoked with the --traditional
## option.
## @seealso{isunix, ismac, ispc}
## @end deftypefn

function [comp, maxsize, endian] = computer (arch)

  if (nargin == 1 && ! strcmpi (arch, "arch"))
    error ('computer: "arch" is only valid argument');
  endif

  canonical_host_type = __octave_config_info__ ("canonical_host_type");
  traditional = __traditional__ ();
  enable_64 = __octave_config_info__ ("ENABLE_64");
  host_parts = ostrsplit (canonical_host_type, "-");

  if (nargin == 0)

    host = "";
    if (traditional && enable_64)
      if (ismac ())
        host = "MACI64";
      elseif (ispc ())
        host = "PCWIN64";
      elseif (strcmp ([host_parts{3} "-" host_parts{1}], "linux-x86_64"))
        host = "GLNXA64";
      endif
    endif
    if (isempty (host))
      host = canonical_host_type;
    elseif (strcmp (host, "unknown"))
      host = "Hi Dave, I'm a HAL-9000";
    endif

    if (nargout == 0)
      disp (host);
    else
      comp = host;
      if (nargout > 1)
        if (enable_64)
          if (traditional)
            maxsize = 2^48-1;
          else
            maxsize = 2^63-1;
          endif
        else
          maxsize = 2^31-1;
        endif
      endif
      if (nargout > 2)
        if (__octave_config_info__ ("words_big_endian"))
          endian = "B";
        elseif (__octave_config_info__ ("words_little_endian"))
          endian = "L";
        else
          endian = "?";
        endif
      endif
    endif

  else

    ## "arch" case.
    comp = "";
    if (traditional && enable_64)
      if (ismac ())
        comp = "maci64";
      elseif (ispc ())
        comp = "win64";
      elseif (strcmp ([host_parts{3} "-" host_parts{1}], "linux-x86_64"))
        comp = "glnxa64";
      endif
    endif
    if (isempty (comp))
      comp = [host_parts{3} "-" host_parts{1}];
      if (numel (host_parts) == 4)
        comp = [host_parts{4} "-" comp];
      endif
    endif

  endif

endfunction


%!assert (ischar (computer ()))
%!assert (computer (), __octave_config_info__ ("canonical_host_type"))
%!assert (ischar (computer ("arch")))

%!error computer (1,2)
%!error <"arch" is only valid argument> computer ("xyz")
