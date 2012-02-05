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
## @deftypefn {Function File} {[@var{v1}, @dots{}] =} getfield (@var{s}, @var{key}, @dots{})
## Extract a field from a structure (or a nested structure).  For example:
##
## @example
## @group
## ss(1,2).fd(3).b = 5;
## getfield (ss, @{1,2@}, "fd", @{3@}, "b")
##    @result{} 5
## @end group
## @end example
##
## Note that the function call in the previous example is equivalent to
## the expression
##
## @example
## @group
## i1 = @{1,2@}; i2 = "fd"; i3 = @{3@}; i4= "b";
## ss(i1@{:@}).(i2)(i3@{:@}).(i4)
##    @result{} 5
##
## @end group
## @end example
## @seealso{setfield, rmfield, isfield, isstruct, fieldnames, struct}
## @end deftypefn

## Author: Etienne Grossmann <etienne@cs.uky.edu>

function obj = getfield (s, varargin)
  if (nargin < 2)
    print_usage ();
  endif
  subs = varargin;
  flds = cellfun ("isclass", subs, "char");
  idxs = cellfun ("isclass", subs, "cell");
  if (all (flds | idxs))
    typs = merge (flds, {"."}, {"()"});
    obj = subsref (s, struct ("type", typs, "subs", subs));
  else
    error ("getfield: invalid index");
  endif
endfunction

%!test
%! x.a = "hello";
%! assert(getfield(x,"a"),"hello");
%!test
%! ss(1,2).fd(3).b = 5;
%! assert(getfield(ss,{1,2},'fd',{3},'b'),5)
