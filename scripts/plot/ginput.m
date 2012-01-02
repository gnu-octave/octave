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
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{buttons}] =} ginput (@var{n})
## Return which mouse buttons were pressed and keys were hit on the current
## figure.  If @var{n} is defined, then wait for @var{n} mouse clicks
## before returning.  If @var{n} is not defined, then @code{ginput} will
## loop until the return key @key{RET} is pressed.
## @end deftypefn

function varargout = ginput (n)

  if (nargin > 1)
    print_usage ();
  endif

  f = gcf ();
  drawnow ();
  toolkit = (get (f, "__graphics_toolkit__"));

  varargout = cell (1, nargout);
  if (nargin == 0)
    [varargout{:}] = feval (strcat ("__", toolkit, "_ginput__"), f);
  else
    [varargout{:}] = feval (strcat ("__", toolkit, "_ginput__"), f, n);
  endif

endfunction

## Remove from test statistics.  No real tests possible.
%!test
%! assert (1);
