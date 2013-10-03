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
## @deftypefn {Function File} {} record (@var{recorder})
## Record audio without blocking. The recording will continue until you use
## the stop method on @var{recorder}.
## @end deftypefn
## @deftypefn {Function File} {} record (@var{recorder}, @var{length})
## Record audio without blocking. The recording will continue for
## @var{length} seconds.
## @end deftypefn

function record (varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  __recorder_record__ (struct (varargin{1}).recorder, varargin{2:end});
endfunction
