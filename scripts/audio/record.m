########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{data} =} record (@var{sec})
## @deftypefnx {} {@var{data} =} record (@var{sec}, @var{fs})
## Record @var{sec} seconds of audio from the system's default audio input at
## a sampling rate of 8000 samples per second.
##
## If the optional argument @var{fs} is given, it specifies the sampling rate
## for recording.
##
## For more control over audio recording, use the @code{audiorecorder} class.
## @seealso{@audiorecorder/audiorecorder, sound, soundsc}
## @end deftypefn

function data = record (sec, fs = 8000)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isscalar (sec) && sec >= 0))
    error ("record: recording duration SEC must be a non-negative number");
  endif

  if (! (isscalar (fs) && fs > 0))
    error ("record: sample rate FS must be a positive number");
  endif

  data = [];

  if (sec > 0)
    rec = audiorecorder (fs, 16, 1);

    recordblocking (rec, sec);

    data = getaudiodata (rec);
  endif

endfunction


## Tests of record must not actually record anything.
%!assert (isempty (record (0)))

## Test input validation
%!error <SEC must be a non-negative number> record (ones (2,2))
%!error <SEC must be a non-negative number> record (-1)
%!error <FS must be a positive number> record (1, ones (2,2))
%!error <FS must be a positive number> record (1, -1)
