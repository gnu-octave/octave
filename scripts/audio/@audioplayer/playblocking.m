########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn  {} {} playblocking (@var{player})
## @deftypefnx {} {} playblocking (@var{player}, @var{start})
## @deftypefnx {} {} playblocking (@var{player}, [@var{start}, @var{end}])
## Play audio stored in the audioplayer object @var{player} with blocking
## (synchronous I/O).
##
## If the optional argument @var{start} is provided, begin playing
## @var{start} samples into the recording.
##
## If the optional argument @var{end} is provided, stop playing at
## @var{end} samples into the recording.
## @seealso{@audioplayer/play, @audioplayer/pause, @audioplayer/stop,
## @audioplayer/audioplayer}
## @end deftypefn

function playblocking (player, start)

  if (nargin == 1)
    __player_playblocking__ (struct (player).player);
  else
    __player_playblocking__ (struct (player).player, start);
  endif

endfunction


## No tests possible for this function
%!assert (1)
