## Copyright (C) 2000  Etienne Grossmann
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
## @deftypefn {Built-in Function} {} [@var{v1},...] =
## @code{getfield (@var{s}, 'k1',...)} extract fields from a structure.
## For example
##
## @example
## @group
## ss(1,2).fd(3).b=5;
## getfield(ss,@{1,2@},'fd',@{3@},'b')
## @result{} ans = 5
## @end group
## @end example
##
## Note that this function could be written as
##
## @example
##          i1= @{1,2@}; i2= 'fd'; i3= @{3@}; i4= 'b';
##          ss( i1@{:@} ).( i2 )( i3@{:@} ).( i4 )
## @end example
## @seealso{setfield,rmfield,isfield,isstruct,fields,struct}
## @end deftypefn

## Author: Etienne Grossmann <etienne@cs.uky.edu>

function s = getfield (s, varargin)

  for idx = 1:nargin-1
    i = varargin{idx};
    if (iscell (i))
      s = s(i{:});
    else
      s = s.(i);
    endif
  endfor

endfunction

%!test
%! x.a = "hello";
%! assert(getfield(x,"a"),"hello");
%!test
%! ss(1,2).fd(3).b = 5;
%! assert(getfield(ss,{1,2},'fd',{3},'b'),5)
