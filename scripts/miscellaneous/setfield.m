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
## @deftypefn {Built-in Function} {[@var{k1},..., @var{v1}] =} setfield (@var{s}, @var{k1}, @var{v1}, @dots{})
## Set field members in a structure.
##
## @example
## @group
## oo(1,1).f0= 1;
## oo = setfield(oo,@{1,2@},'fd',@{3@},'b', 6);
## oo(1,2).fd(3).b == 6
## @result{} ans = 1
## @end group
## @end example
##
## Note that this function could be written
##
## @example
##          i1= @{1,2@}; i2= 'fd'; i3= @{3@}; i4= 'b';
##          oo( i1@{:@} ).( i2 )( i3@{:@} ).( i4 ) == 6;
## @end example
## @seealso{getfield, rmfield, isfield, isstruct, fieldnames, struct}
## @end deftypefn

## Author:  Etienne Grossmann <etienne@cs.uky.edu>

function obj = setfield (obj, varargin)
   field = "obj";
   for i = 1:nargin-2
     v = varargin{i};
     if (iscell (v))
       sep = "(";
       for j = 1:length (v)
	 field = sprintf ("%s%s%s", field, sep, num2str (v{j}));
         sep = ",";
       endfor
       field = sprintf ("%s)", field);
     else
       field = sprintf ("%s.%s", field, v);
     endif
   endfor
   val = varargin{nargin-1};
   eval (sprintf ("%s=val;", field));
endfunction

%!test
%! x.a = "hello";
%! x = setfield(x,"b","world");
%! y = struct('a','hello','b','world');
%! assert(x,y);
%!test
%! oo(1,1).f0= 1;
%! oo = setfield(oo,{1,2},'fd',{3},'b', 6);
%! assert (oo(1,2).fd(3).b, 6)
