## Copyright (C) 2009 Jaroslav Hajek
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{s}] =} strsplit (@var{p}, @var{sep}, @var{strip_empty})
## Splits a single string using one or more delimiters.
## The result is returned as a cell array of strings. Consecutive delimiters
## and delimiters at boundaries result in empty strings, unless @var{strip_empty} is true.
## The default value of @var{strip_empty} is false.
## @end deftypefn

function s = strsplit (p, sep, strip_empty = false)
  if (nargin < 2 || nargin > 3 || ! ischar (p) || rows (p) > 1 \
    || ! ischar (sep) || ! islogical (strip_empty))
    print_usage ();
  endif

  if (isempty (p))
    s = cell (size (p));
  else
    ## split p according to delimiter.
    if (isscalar (sep))
      ## single separator
      idx = find (p == sep);
    else
      ## multiple separators
      idx = strchr (p, sep);
    endif

    ## get substring sizes.
    if (isempty (idx))
      sizes = numel (p);
    else
      sizes = [idx(1)-1, diff(idx)-1, numel(p)-idx(end)];
    endif
    ## remove separators.
    p(idx) = []; 
    if (strip_empty)
      ## omit zero lengths.
      sizes = sizes (sizes != 0); 
    endif
    ## convert!
    s = mat2cell (p, 1, sizes);
  endif
endfunction

%!assert (all (strcmp (strsplit ("road to hell", " "), {"road", "to", "hell"})))

%!assert (all (strcmp (strsplit ("road to^hell", " ^"), {"road", "to", "hell"})))

%!assert (all (strcmp (strsplit ("road   to--hell", " -", true), {"road", "to", "hell"})))
