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
## @deftypefn {} {@var{props} =} __get_properties__ (@var{player})
## Return a struct containing all named properties of the audioplayer object
## @var{player}.
## @seealso{@audioplayer/get, @audioplayer/set, @audioplayer/audioplayer}
## @end deftypefn

function props = __get_properties__ (player)

  hplayer = struct (player).player;

  props = struct ("BitsPerSample",
                  __player_get_nbits__ (hplayer),

                  "CurrentSample",
                  __player_get_sample_number__ (hplayer),

                  "DeviceID",
                  __player_get_id__ (hplayer),

                  "NumberOfChannels",
                  __player_get_channels__ (hplayer),

                  "Running",
                  ifelse (__player_isplaying__ (hplayer), "on", "off"),

                  "SampleRate",
                  __player_get_fs__ (hplayer),

                  "TotalSamples",
                  __player_get_total_samples__ (hplayer),

                  "Tag",
                  __player_get_tag__ (hplayer),

                  "Type",
                  "audioplayer",

                  "UserData",
                  __player_get_userdata__ (hplayer));

endfunction
