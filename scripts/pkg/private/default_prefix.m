########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{prefix}, @var{archprefix} =} default_prefix (@var{global_install}, @var{desc})
## Undocumented internal function.
## @end deftypefn

## FIXME: second input "desc" does not appear to be used.
function [prefix, archprefix] = default_prefix (global_install, desc)

  if (global_install)
    prefix = fullfile (OCTAVE_HOME (), "share", "octave", "packages");
    if (nargin == 2)
      archprefix = fullfile (__octave_config_info__ ("libdir"), "octave",
                             "packages", [desc.name "-" desc.version]);
    else
      archprefix = fullfile (__octave_config_info__ ("libdir"), "octave",
                             "packages");
    endif
  else
    prefix = fullfile (user_data_dir (), "octave", ...
                       __octave_config_info__ ("api_version"), "packages");
    archprefix = prefix;
  endif

endfunction
