## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2003,
##               2004, 2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} xlabel (@var{string})
## @deftypefnx {Function File} {} ylabel (@var{string})
## @deftypefnx {Function File} {} zlabel (@var{string})
## @deftypefnx {Function File} {} xlabel (@var{h}, @var{string})
## Specify x, y, and z axis labels for the current figure. If @var{h} is
## specified then label the axis defined by @var{h}.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs,  ylabel, title}
## @end deftypefn

## Author: jwe

function h = xlabel (varargin)

  if (isscalar (varargin{1}) && ishandle (varargin{1}))
    ax = varargin{1};
    if (! strcmp (get (ax, "type"), "axes"))
      error ("xlabel: expecting first argument to be an axes object");
    endif
    if (rem (nargin, 2) == 1)
      print_usage ();
    endif
    oldh = gca ();
    unwind_protect
      axes (ax);
      tmp = __axis_label__ ("xlabel", varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    if (rem (nargin, 2) != 1)
      print_usage ();
    endif
    tmp = __axis_label__ ("xlabel", varargin{1:end});
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
