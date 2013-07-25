## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} stem3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} stem3 (@var{x}, @var{y}, @var{z}, @var{linespec})
## @deftypefnx {Function File} {} stem3 (@dots{}, "filled")
## @deftypefnx {Function File} {} stem3 (@dots{}, "@var{prop}", "@var{val}", @dots{})
## @deftypefnx {Function File} {} stem3 (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} stem3 (@dots{})
## Plot a 3-D stem graph.
##
## The default color is @code{"r"} (red), the default line style is
## @code{"-"}, and the default marker is @code{"o"}.  The line style can
## be altered by the @code{linespec} argument in the same manner as the
## @code{plot} command.
##
## Multiple property/value pairs may be specified, but they must appear in
## pairs.
##
## If the first argument @var{hax} is an axis handle, then plot into these axes,
## rather than the current axis handle returned by @code{gca}.
##
## The optional return value @var{h} is a vector with the handles of the line
## and marker objects used to draw the stems as "stem series" object.
##
## Example:
##
## @example
## @group
## theta = 0:0.2:6;
## stem3 (cos (theta), sin (theta), theta)
## @end group
## @end example
##
## @noindent
## plots 31 stems with heights from 0 to 6 lying on a circle.
##
## Implementation Note: Color definitions with RGB-triples are not valid.
##
## @seealso{stem, bar, hist, plot}
## @end deftypefn

function h = stem3 (varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  htmp = __stem__ (true, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! theta = 0:0.2:6;
%! stem3 (cos (theta), sin (theta), theta);

