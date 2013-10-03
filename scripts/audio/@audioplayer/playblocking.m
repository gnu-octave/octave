## Copyright (C) 2013 Vytautas Jančauskas
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} playblocking (@var{player})
## Play back audio stored in the audioplayer object with blocking.
## @end deftypefn
## @deftypefn {Function File} {} playblocking (@var{player}, @var{start})
## Play back audio stored in the audioplayer object starting at the time in
## seconds specified by @var{start}.
## @end deftypefn
## @deftypefn {Function File} {} playblocking (@var{player}, [@var{start}, @var{end}])
## Play back audio stored in the audioplayer object starting at the time in
## seconds specified by @var{start} and ending at the time specified by
## @var{end}.
## @end deftypefn

function playblocking (varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  __player_playblocking__ (struct (varargin{1}).player, varargin{2:end});
endfunction
