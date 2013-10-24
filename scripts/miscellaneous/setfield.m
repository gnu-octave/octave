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
## @deftypefn  {Function File} {@var{s} =} setfield (@var{s}, @var{field}, @var{val})
## @deftypefnx {Function File} {@var{s} =} setfield (@var{s}, @var{idx1}, @var{field1}, @var{idx2}, @var{field2}, @dots{}, @var{val})
##
## Set a field member @var{field} in a structure @var{s} equal to @var{val}. 
## For example:
##
## @example
## @group
## @var{s} = struct ();
## @var{s} = setfield (@var{s}, "foo bar", 42);
## @end group
## @end example
##
## @noindent
## This is equivalent to
##
## @example
## @var{s}.("foo bar") = 42;
## @end example
##
## @noindent
## Note that ordinary structure syntax @code{@var{s}.foo bar = 42} cannot be
## used here, as the field name is not a valid Octave identifier.  Using
## arbitrary strings for field name is incompatible with @sc{matlab}, so
## this usage will warn if the @code{Octave:matlab-incompatible} warning
## is set.  @xref{XREFwarning_ids}.
##
## With the second calling form, set a field on a structure array,
## possibly nested, with successive nested indices @var{idx1},
## @var{idx2}, @dots{} and fields @var{field1}, @var{field2}, @dots{}
## The indices must be cells containing the desired index at this
## nesting depth.
##
## Thus consider instead,
##
## @example
## @group
## @var{s} = struct ("baz", 42);
## setfield (@var{s}, @{1@}, "foo", @{1@}, "bar", 5)
##     @result{} ans =
##     scalar structure containing the fields:
##       baz =  42
##       foo =
##         scalar structure containing the fields:
##           bar =  54
## @end group
## @end example
##
## Here we first have an ordinary structure array with one field
## @code{baz} set to 42.  Then we set another field in a nested scalar structure
## indexing with two single cells containing the unique desired indices.
##
## Finally an example with nested structure arrays,
##
## @example
## @group
## @var{sa}.foo = 1;
## @var{sa} = setfield (@var{sa}, @{2@}, "bar", @{3@}, "baz", 6);
## @var{sa}(2).bar(3)
##      @result{} ans =
##      scalar structure containing the fields:
##        baz =  6
## @end group
## @end example
##
## Here @var{sa} is a structure array whose field @code{fd} at elements
## 1 and 2 field is in turn
## another structure array whose third element is a structure
##
## Note that the same result as in the above example could be achieved by:
##
## @example
## @group
## @var{SA}.foo = 1;
## @var{SA}(2).bar(3).baz = 6
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
%! x = setfield (x, "b", "world");
%! y = struct ("a", "hello", "b", "world");
%! assert (x,y);
%!test
%! oo(1,1).f0 = 1;
%! oo = setfield (oo,{1,2},"fd",{3},"b", 6);
%! assert (oo(1,2).fd(3).b, 6);

