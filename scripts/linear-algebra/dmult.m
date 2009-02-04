## Copyright (C) 2008 VZLU Prague, a.s., Czech Republic
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
## @deftypefn {Function File} {@var{c} =} dmult (@var{a}, @var{b})
## @deftypefnx {Function File} {@var{c} =} dmult (@var{a}, @var{b}, @var{ind})
## Scale a matrix by rows or columns, or a multidimensional tensor along 
## a specified dimension.
## If @var{a} is a vector of length @code{rows (@var{b})}, return
## @code{diag (@var{a}) * @var{b}} (but computed much more efficiently).
## Similarly, if @var{b} is a vector of length @code{columns(@var{a})},
## return @code{@var{a} * diag(@var{b})}. 
##
## If @var{b} is a multidimensional array and @var{a} a vector,
## @var{c} will have the same shape as @var{b}, with 
## @code{@var{C}(i,:,@dots{}) = @var{a}(i)*@var{b}(i,:,@dots{})}.
##
## If @var{a} is a multidimensional array and @var{b} a vector,
## @var{c} will have the same shape as @var{a}, with 
## @code{@var{C}(:,@dots{},i) = @var{a}(:,@dots{},i)*@var{b}(i)}.
##
## If @var{ind} is supplied, @var{a} should be an array and @var{b}
## a vector of length @code{size (@var{a},index)}. The result is then
## @code{@var{C}(:,@dots{},i,:,@dots{}) = @var{a}(:,@dots{},i,:,@dots{})*@var{b}(i)}
## where i indexes the @var{ind}-th dimension.
## @end deftypefn

## Author: Jaroslav Hajek <highegg@gmail.com>
## Description: Scale a tensor along a dimension

### Original Author: KH <Kurt.Hornik@wu-wien.ac.at>
### Original Description: Rescale the rows of a matrix

function m = dmult (a, b, ind)
  if (nargin == 2)
    sa = size (a);
    sb = size (b);
    if (isvector (a) && length (a) == sb(1))
      a = a(:);
      m = reshape (kron (ones (prod (sb(2:end)), 1), a), sb) .* b;
    elseif (isvector (b) && length (b) == sa(end))
      b = b(:);
      m = reshape (kron (b, ones (prod (sa (1:end-1)), 1)), sa) .* a;
    else
      error ("dmult: dimensions mismatch");
    endif

  elseif (nargin == 3 && isscalar (ind))
    if (isvector (b) && ind > 0 && ind <= ndims (a)
	&& length (b) == size (a, ind))
      b = b(:);
      sa = size (a); 
      sal = prod (sa(1:ind-1)); sat = prod (sa(ind+1:end));
      s = kron (ones (sat, 1), kron (b, ones (sal, 1)));
      m = reshape (s, sa) .* a;
    else
      error ("dmult: dimensions mismatch or index out of range");
    endif
  else
    print_usage ();
  endif

endfunction

%!test
%! assert (dmult ([1,2,3], ones(3)), [1,1,1;2,2,2;3,3,3])
%! assert (dmult ([1,2,3]', ones(3)), [1,1,1;2,2,2;3,3,3])
%!test
%! assert (dmult ([1,2,3], ones(3,2,2)), reshape ([1,1,1,1;2,2,2,2;3,3,3,3], [3,2,2]))
%!test
%! assert (dmult (ones(3), [1,2,3]), [1,2,3;1,2,3;1,2,3])
%! assert (dmult (ones(3), [1,2,3]'), [1,2,3;1,2,3;1,2,3])
%!test
%! assert (dmult (ones(2,2,3), [1,2,3]), reshape ([1,2,3;1,2,3;1,2,3;1,2,3], [2,2,3]))
%!test
%! assert (dmult (ones(3,4,2), [1 2 3 4], 2),...
%! reshape ([1 1 1 2 2 2 3 3 3 4 4 4 1 1 1 2 2 2 3 3 3 4 4 4], [3,4,2]))
