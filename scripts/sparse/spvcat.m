## Copyright (C) 2004 David Bateman & Andy Adler
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
## @deftypefn {Function File} {@var{y} =} spvcat (@var{a1}, @var{a2}, @dots{}, @var{aN})
## Return the vertical concatenation of sparse matrices. This function
## is obselete and @code{vertcat} should be used
## @seealso{sphcat, vertcat, horzcat, cat}
## @end deftypefn

function y = spvcat (varargin)

  persistent spvcat_warned = false;

  if (! spvcat_warned)
    spvcat_warned = true;
    warning ("spvcat: This function is depreciated. Use vertcat instead");
  endif

  y = vertcat (varargin{:});
endfunction
