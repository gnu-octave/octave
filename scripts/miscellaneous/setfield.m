## Copyright (C) 2000-2012 Etienne Grossmann
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
## @deftypefn {Function File} {[@var{k1}, @dots{}, @var{v1}] =} setfield (@var{s}, @var{k1}, @var{v1}, @dots{})
## Set a field member in a (nested) structure array.  For example:
##
## @example
## @group
## oo(1,1).f0 = 1;
## oo = setfield (oo, @{1,2@}, "fd", @{3@}, "b", 6);
## oo(1,2).fd(3).b == 6
##      @result{} ans = 1
## @end group
## @end example
##
## Note that the same result as in the above example could be achieved by:
##
## @example
## @group
## i1 = @{1,2@}; i2 = "fd"; i3 = @{3@}; i4 = "b";
## oo(i1@{:@}).(i2)(i3@{:@}).(i4) == 6
##      @result{} ans = 1
## @end group
## @end example
## @seealso{getfield, rmfield, isfield, isstruct, fieldnames, struct}
## @end deftypefn

## Author:  Etienne Grossmann <etienne@cs.uky.edu>

function obj = setfield (obj, varargin)
  if (nargin < 3)
    print_usage ();
  endif
  subs = varargin(1:end-1);
  rhs = varargin{end};
  flds = cellfun ("isclass", subs, "char");
  idxs = cellfun ("isclass", subs, "cell");
  if (all (flds | idxs))
    typs = merge (flds, {"."}, {"()"});
    obj = subsasgn (obj, struct ("type", typs, "subs", subs), rhs);
  else
    error ("setfield: invalid index");
  endif
endfunction

%!test
%! x.a = "hello";
%! x = setfield(x,"b","world");
%! y = struct("a","hello","b","world");
%! assert(x,y);
%!test
%! oo(1,1).f0= 1;
%! oo = setfield(oo,{1,2},"fd",{3},"b", 6);
%! assert (oo(1,2).fd(3).b, 6)
