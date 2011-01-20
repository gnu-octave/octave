## Copyright (C) 2009-2011 Jaroslav Hajek
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
## @deftypefn {Function File} {[@var{s}] =} strsplit (@var{p}, @var{sep}, @var{strip_empty})
## Split a single string using one or more delimiters and return a cell
## array of strings.  Consecutive delimiters and delimiters at
## boundaries result in empty strings, unless @var{strip_empty} is true.
## The default value of @var{strip_empty} is false.
## @seealso{strtok}
## @end deftypefn

function s = strsplit (p, sep, strip_empty = false)

  if (nargin < 2 || nargin > 3 || ! ischar (p) || rows (p) > 1
      || ! ischar (sep) || ! islogical (strip_empty))
    print_usage ();
  endif

  if (isempty (p))
    s = cell (size (p));
  else
    ## Split p according to delimiter.
    if (isscalar (sep))
      ## Single separator.
      idx = find (p == sep);
    else
      ## Multiple separators.
      idx = strchr (p, sep);
    endif

    ## Get substring sizes.
    if (isempty (idx))
      sizes = numel (p);
    else
      sizes = [idx(1)-1, diff(idx)-1, numel(p)-idx(end)];
    endif
    ## Remove separators.
    p(idx) = [];
    if (strip_empty)
      ## Omit zero lengths.
      sizes = sizes (sizes != 0);
    endif
    ## Convert!
    s = mat2cell (p, 1, sizes);
  endif

endfunction

%!assert (all (strcmp (strsplit ("road to hell", " "), {"road", "to", "hell"})))

%!assert (all (strcmp (strsplit ("road to^hell", " ^"), {"road", "to", "hell"})))

%!assert (all (strcmp (strsplit ("road   to--hell", " -", true), {"road", "to", "hell"})))
