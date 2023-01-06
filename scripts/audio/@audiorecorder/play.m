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
## @deftypefn  {} {@var{player} =} play (@var{recorder})
## @deftypefnx {} {@var{player} =} play (@var{recorder}, @var{start})
## @deftypefnx {} {@var{player} =} play (@var{recorder}, [@var{start}, @var{end}])
## Play the audio recorded in @var{recorder} without blocking and return a
## corresponding audioplayer object.
##
## If the optional argument @var{start} is provided, begin playing
## @var{start} seconds into the recording.
##
## If the optional argument @var{end} is provided, stop playing at
## @var{end} seconds into the recording.
## @seealso{@audiorecorder/getplayer, @audioplayer/audioplayer,
## @audiorecorder/audiorecorder}
## @end deftypefn

function player = play (recorder, start)

  data = getaudiodata (recorder);
  player = audioplayer (data,
                        get (recorder, "SampleRate"),
                        get (recorder, "BitsPerSample"));

  if (nargin == 1)
    play (player);
  else
    play (player, start);
  endif

endfunction


## No tests possible for this function
%!assert (1)
