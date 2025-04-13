########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn {} @var{__pkg__} = get_validated_pkg_list ()
## Download list of current packages and validate that it fits expected
## patterns.
##
## Return @code{__pkg__} struct used by other functions.
## @end deftypefn

function retval = get_validated_pkg_list ()

  ## The __pkg__ struct is what we return with all the package information.
  ## We make it persistent to avoid querying the server each time.
  persistent __pkg__;

  if (! isempty (__pkg__))
    ## This function has been called already and __pkg__ exists.
    ## No need to query the server again.
    retval = __pkg__;
    return;
  endif

  [list, succ] = urlread ("https://packages.octave.org/packages.json");
  if (! succ)
    error ("pkg: could not read URL, please verify internet connection");
  endif

  __pkg__ = jsondecode (list, "makeValidName", false);

  ## A sanity check before the calling location uses this.
  if (! isstruct (__pkg__))
    error ("pkg: server returned data of unknown format");
  endif

  retval = __pkg__;

endfunction
