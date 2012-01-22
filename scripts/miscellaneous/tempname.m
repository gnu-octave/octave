## Copyright (C) 2003-2012 John W. Eaton
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
## @deftypefn  {Function File} {} tempname ()
## @deftypefnx {Function File} {} tempname (@var{dir})
## @deftypefnx {Function File} {} tempname (@var{dir}, @var{prefix})
## This function is an alias for @code{tmpnam}.
## @seealso{tmpnam}
## @end deftypefn

function filename = tempname (varargin)

  filename = tmpnam (varargin{:});

endfunction


%% No tests needed for alias.
%!assert (1)
