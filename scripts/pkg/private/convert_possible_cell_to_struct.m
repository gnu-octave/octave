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
## @deftypefn {} @var{retval} = convert_possible_cell_to_struct ()
## Workaround for a complication that happens with some packages, where a part
## of the package metadata returned by the server contains a cell array instead
## of a struct for reasons internal to the server, so this private function
## is used to work around that unavoidable inconsistency in the metadata format.
## @end deftypefn

function retval = convert_possible_cell_to_struct (obj)
  if (isstruct (obj))
    retval = obj;
  elseif (iscell (obj))
    retval = obj{1};
  else
    error ("pkg: internal error: obj is neither cell nor struct but type '%s'.", class (obj));
  endif

  if (! isstruct (retval))
    error ("pkg: internal error: expected retval to be struct but got '%s'.", class (retval));
  endif
endfunction
