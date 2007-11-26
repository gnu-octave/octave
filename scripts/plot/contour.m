## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002,
##               2003, 2004, 2005, 2006, 2007 Shai Ayal
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
## @deftypefn {Function File} {@var{c} =} contour (@var{z})
## @deftypefnx {Function File} {@var{c} =} contour (@var{z}, @var{vn})
## @deftypefnx {Function File} {@var{c} =} contour (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{c} =} contour (@var{x}, @var{y}, @var{z}, @var{vn})
## @deftypefnx {Function File} {@var{c} =} contour (@var{h}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contour (@dots{})
## Plot level curves (contour lines) of the matrix @var{z}, using the
## contour matrix @var{c} computed by @code{contourc} from the same
## arguments; see the latter for their interpretation.  The set of
## contour levels, @var{c}, is only returned if requested.  For example:
##
## @example
## @group
## x = 0:2;
## y = x;
## z = x' * y;
## contour (x, y, z, 2:3)
##
## @end group
## @end example
##
## The optional input and output argument @var{h} allows an axis handle to 
## be passed to @code{contour} and the handles to the contour objects to be
## returned.
## @seealso{contourc, patch, plot}
## @end deftypefn

## Author: shaia

function [c, h] = contour (varargin)

  if (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin{1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("contour: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      [ctmp, htmp] = __contour__ (h, varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    newplot ();
    [ctmp, htmp] = __contour__ (gca (), NaN, varargin{:});
  endif

  if (nargout > 0)
    c = ctmp;
    h = htmp;
  endif

endfunction
