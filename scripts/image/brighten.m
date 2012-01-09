## Copyright (C) 1999-2012 Kai Habel
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
## @deftypefn  {Function File} {@var{map_out} =} brighten (@var{map}, @var{beta})
## @deftypefnx {Function File} {@var{map_out} =} brighten (@var{h}, @var{beta})
## @deftypefnx {Function File} {@var{map_out} =} brighten (@var{beta})
## Darken or brighten the given colormap.  If the @var{map} argument
## is omitted, the function is applied to the current colormap.  The first
## argument can also be a valid graphics handle @var{h}, in which case
## @code{brighten} is applied to the colormap associated with this handle.
##
## Should the resulting colormap @var{map_out} not be assigned, it will be
## written to the current colormap.
##
## The argument @var{beta} should be a scalar between -1 and 1,
## where a negative value darkens and a positive value brightens
## the colormap.
## @seealso{colormap}
## @end deftypefn

function rmap = brighten (arg1, beta)
  h = -1;
  if (nargin == 1)
    beta = arg1;
    m = colormap;
    h = gcf ();
  elseif (nargin == 2)
    if (ishandle (arg1))
      h = arg1;
      m = get (h, "colormap");
    elseif (ismatrix (arg1) && columns (arg1) == 3)
      m = arg1;
    else
      error ("brighten: first argument must be an Nx3 matrix or a handle");
    endif
  else
    print_usage ();
  endif

  if (! isscalar (beta) || beta <= -1 || beta >= 1)
    error ("brighten: BETA must be a scalar in the range (-1,1)");
  endif

  if (beta > 0)
    gamma = 1 - beta;
  else
    gamma = 1 / (1 + beta);
  endif

  if (nargout == 0)
    if (ishandle (h))
      set (h, "colormap", m .^ gamma);
    else
      colormap (m .^ gamma);
    endif
  else
    rmap = m .^ gamma;
  endif

endfunction
