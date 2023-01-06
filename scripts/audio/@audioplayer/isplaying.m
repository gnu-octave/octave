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
## @deftypefn {} {@var{tf} =} isplaying (@var{player})
## Return true if the audioplayer object @var{player} is currently playing back
## audio and false otherwise.
## @seealso{@audioplayer/pause, @audioplayer/audioplayer}
## @end deftypefn

function tf = isplaying (player)

  tf = __player_isplaying__ (struct (player).player);

endfunction


%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! assert (isplaying (player), false);
