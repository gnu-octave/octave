## Copyright (C) 2009-2012 Jaroslav Hajek
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
## @deftypefn  {Function File} {[@var{cstr}] =} strsplit (@var{s}, @var{sep})
## @deftypefnx {Function File} {[@var{cstr}] =} strsplit (@var{s}, @var{sep}, @var{strip_empty})
## Split the string @var{s} using one or more separators @var{sep} and return
## a cell array of strings.  Consecutive separators and separators at
## boundaries result in empty strings, unless @var{strip_empty} is true.
## The default value of @var{strip_empty} is false.
##
## 2-D character arrays are split at separators and at the original column
## boundaries.
##
## Example:
##
## @example
## @group
## strsplit ("a,b,c", ",")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
##
## strsplit (["a,b" ; "cde"], ",")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = cde
##           @}
## @end group
## @end example
## @seealso{strtok}
## @end deftypefn

function cstr = strsplit (s, sep, strip_empty = false)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  elseif (! ischar (s) || ! ischar (sep))
    error ("strsplit: S and SEP must be string values");
  elseif (! isscalar (strip_empty))
    error ("strsplit: STRIP_EMPTY must be a scalar value");
  endif

  if (isempty (s))
    cstr = cell (size (s));
  else
    if (rows (s) > 1)
      ## For 2-D arrays, add separator character at line boundaries
      ## and transform to single string
      s(:, end+1) = sep(1);
      s = reshape (s.', 1, numel (s));
      s(end) = []; 
    endif

    ## Split s according to delimiter
    if (isscalar (sep))
      ## Single separator
      idx = find (s == sep);
    else
      ## Multiple separators
      idx = strchr (s, sep);
    endif

    ## Get substring lengths.
    if (isempty (idx))
      strlens = length (s);
    else
      strlens = [idx(1)-1, diff(idx)-1, numel(s)-idx(end)];
    endif
    ## Remove separators.
    s(idx) = [];
    if (strip_empty)
      ## Omit zero lengths.
      strlens = strlens(strlens != 0);
    endif

    ## Convert!
    cstr = mat2cell (s, 1, strlens);
  endif

endfunction


%!assert (strsplit ("road to hell", " "), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", " ^"), {"road", "to", "hell"})
%!assert (strsplit ("road   to--hell", " -", true), {"road", "to", "hell"})
%!assert (strsplit (["a,bc";",de"], ","), {"a", "bc", ones(1,0), "de "})
%!assert (strsplit (["a,bc";",de"], ",", true), {"a", "bc", "de "})
%!assert (strsplit (["a,bc";",de"], ", ", true), {"a", "bc", "de"})

%% Test input validation
%!error strsplit ()
%!error strsplit ("abc")
%!error strsplit ("abc", "b", true, 4)
%!error <S and SEP must be string values> strsplit (123, "b")
%!error <S and SEP must be string values> strsplit ("abc", 1)
%!error <STRIP_EMPTY must be a scalar value> strsplit ("abc", "def", ones(3,3))

