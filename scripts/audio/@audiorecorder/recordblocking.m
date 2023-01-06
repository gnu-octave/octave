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
## @deftypefn {} {} recordblocking (@var{recorder}, @var{length})
## Record audio with blocking (synchronous I/O).
##
## The length of the recording in seconds (@var{length}) must be specified.
## @seealso{@audiorecorder/record, @audiorecorder/audiorecorder}
## @end deftypefn

function recordblocking (recorder, length)

  if (nargin != 2)
    print_usage ();
  endif

  __recorder_recordblocking__ (struct (recorder).recorder, length);

endfunction


## No tests possible for this function
%!assert (1)
