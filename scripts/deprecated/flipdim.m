## Copyright (C) 2004-2013 David Bateman
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} flipdim (@var{x})
## @deftypefnx {Function File} {} flipdim (@var{x}, @var{dim})
## Return a copy of @var{x} flipped about the dimension @var{dim}.
## @var{dim} defaults to the first non-singleton dimension.
##
## @strong{Warning:} @code{flipdim} is scheduled for removal in version 4.6.
## Use @code{flip} which can be used as a drop-in replacement.
##
## @seealso{fliplr, flipud, rot90, rotdim}
## @end deftypefn

## Author: David Bateman, Jaroslav Hajek

function y = flipdim (x, dim)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "flipdim is deprecated and will be removed from a future version of Octave; please use flip (x, dim) instead");
  endif

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin == 1)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  elseif (! (isscalar (dim) && isindex (dim)))
    error ("flipdim: DIM must be a positive integer");
  endif

  idx(1:max(nd, dim)) = {':'};
  idx{dim} = size (x, dim):-1:1;
  y = x(idx{:});

endfunction

