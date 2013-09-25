## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {[@var{x}, @var{y}, @var{buttons}] =} ginput (@var{n})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{buttons}] =} ginput ()
## Return the position and type of mouse button clicks and/or key strokes
## in the current figure window.
##
## If @var{n} is defined, then capture @var{n} events before returning.
## When @var{n} is not defined @code{ginput} will loop until the return key
## @key{RET} is pressed.
##
## The return values @var{x}, @var{y} are the coordinates where the mouse
## was clicked in the units of the current axes.  The return value @var{button}
## is 1, 2, or 3 for the left, middle, or right button.  If a key is pressed
## the ASCII value is returned in @var{button}.
## @seealso{gtext, waitforbuttonpress}
## @end deftypefn

function varargout = ginput (n)

  if (nargin > 1)
    print_usage ();
  endif

  f = gcf ();
  a = gca ();  # Create an axis, if necessary
  drawnow ();
  toolkit = get (f, "__graphics_toolkit__");

  varargout = cell (1, nargout);
  if (nargin == 0)
    [varargout{:}] = feval (["__" toolkit "_ginput__"], f);
  else
    [varargout{:}] = feval (["__" toolkit "_ginput__"], f, n);
  endif

endfunction


## Remove from test statistics.  No real tests possible.
%!test
%! assert (1);

