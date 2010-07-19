## Copyright (C) 2007, 2008, 2009 David Bateman
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
## @deftypefn  {Function File} {@var{xl} =} zlim ()
## @deftypefnx {Function File} {} zlim (@var{xl})
## @deftypefnx {Function File} {@var{m} =} zlim ('mode')
## @deftypefnx {Function File} {} zlim (@var{m})
## @deftypefnx {Function File} {} zlim (@var{h}, @dots{})
## Get or set the limits of the z-axis of the current plot.  Called without
## arguments @code{zlim} returns the z-axis limits of the current plot.
## If passed a two element vector @var{xl}, the limits of the z-axis are set
## to this value.
##
## The current mode for calculation of the z-axis can be returned with a
## call @code{zlim ('mode')}, and can be either 'auto' or 'manual'.  The 
## current plotting mode can be set by passing either 'auto' or 'manual' 
## as the argument.
##
## If passed an handle as the first argument, then operate on this handle
## rather than the current axes handle.
## @seealso{xlim, ylim, set, get, gca}
## @end deftypefn

function retval = zlim (varargin)
  ret = __axes_limits__ ("zlim", varargin{:});

  if (! isempty (ret))
    retval = ret;
  endif
endfunction
