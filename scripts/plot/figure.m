## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} figure (@var{n})
## Set the current plot window to plot window @var{n}.  If @var{n} is
## not specified, the next available window number is chosen.
## @end deftypefn

## Author: jwe, Bill Denney

function h = figure (varargin)

  nargs = nargin;

  if (nargs == 0)
    f = __uiobject_init_figure__ ();
  elseif (mod (nargs, 2) == 1)
    tmp = varargin{1};
    if (ishandle (tmp) && strcmp (get (tmp, "type"), "figure"))
      f = tmp;
      varargin(1) = [];
      nargs--;
    elseif (isnumeric (tmp) && tmp > 0 && round (tmp) == tmp)
      f = tmp;
      __uiobject_init_figure__ (f);
      varargin(1) = [];
      nargs--;
    else
      error ("figure: expecting figure handle or figure number");
    endif
  endif

  if (nargout > 0)
    h = f;
  endif

  if (rem (nargs, 2) == 0)
    if (nargs > 0)
      set (f, varargin{:});
    endif
    __uiobject_adopt__ (0, f);
    set (0, "currentfigure", f);
    drawnow ();
  else
    print_usage ();
  endif

endfunction
