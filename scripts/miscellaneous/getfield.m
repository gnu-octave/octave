## Copyright (C) 2000-2013 Etienne Grossmann
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
## @deftypefn  {Function File} {[@var{val}] =} getfield (@var{s}, @var{field})
## @deftypefnx {Function File} {[@var{val}] =} getfield (@var{s}, @var{idx1}, @var{field1}, @var{idx2}, @var{field2}, @dots{})
## Extract a field from a structure (or a nested structure).  The syntax
## is the same as @code{setfield}, except it omits the final @var{val}
## argument, returning this value instead of setting it.
##
## @seealso{setfield, rmfield, isfield, fieldnames, isstruct, struct}
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
%! assert (getfield (x, "a"), "hello");
%!test
%! ss(1,2).fd(3).b = 5;
%! assert (getfield (ss,{1,2},"fd",{3},"b"), 5);

