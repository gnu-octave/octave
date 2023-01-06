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
## @deftypefn {} {@var{props} =} __get_properties__ (@var{recorder})
## Return a struct containing all named properties of the recorder object
## @var{recorder}.
## @seealso{@audiorecorder/get, @audiorecorder/set,
## @audiorecorder/audiorecorder}
## @end deftypefn

function props = __get_properties__ (recorder)

  hrecorder = struct (recorder).recorder;

  props = struct ("BitsPerSample",
                  __recorder_get_nbits__ (hrecorder),

                  "CurrentSample",
                  __recorder_get_sample_number__ (hrecorder),

                  "DeviceID",
                  __recorder_get_id__ (hrecorder),

                  "NumberOfChannels",
                  __recorder_get_channels__ (hrecorder),

                  "Running",
                  ifelse (__recorder_isrecording__ (hrecorder), "on", "off"),

                  "SampleRate",
                  __recorder_get_fs__ (hrecorder),

                  "TotalSamples",
                  __recorder_get_total_samples__ (hrecorder),

                  "Tag",
                  __recorder_get_tag__ (hrecorder),

                  "Type",
                  "audiorecorder",

                  "UserData",
                  __recorder_get_userdata__ (hrecorder));

endfunction
