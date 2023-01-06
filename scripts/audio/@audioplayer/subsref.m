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
## @deftypefn {} {@var{value} =} subsref (@var{player}, @var{idx})
## Perform subscripted selection on the audio player object @var{player}.
##
## Return the player property value named by @var{idx}.
## @seealso{@audioplayer/audioplayer}
## @end deftypefn

function value = subsref (player, idx)

  if (nargin != 2)
    print_usage ();
  endif

  if (isempty (idx))
    error ("@audioplayer/subsref: missing index");
  endif

  if (strcmp (idx(1).type, "."))
    field = idx.subs;
    value = get (player, field);
  else
    error ("@audioplayer/subsref: invalid subscript type");
  endif

endfunction


%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! set (player, "Tag", "mytag");
%! assert (player.Tag, "mytag");

## Test input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! fail ("player(1).Tag", "invalid subscript type");
%! fail ("player{1}.Tag", "invalid subscript type");
