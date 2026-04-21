########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{opts} =} __bfsdfs_parse_opts__ (@var{fname}, @var{nv_cell})
## Private helper that parses the @code{Name-Value} trailing arguments of
## @code{bfsearch} and @code{dfsearch}.
##
## @var{fname} is a short prefix used in error messages (usually
## @qcode{"bfsearch"} or @qcode{"dfsearch"}).  @var{nv_cell} is the
## @code{varargin} cell array containing alternating name / value
## entries.
##
## Returns a scalar struct with fields:
##
## @itemize
## @item
## @code{restart} (logical scalar, default @code{false})
## @item
## @code{edgecolors} (logical scalar, default @code{false})
## @end itemize
##
## Recognised option names (case-insensitive) are @qcode{"Restart"} and
## @qcode{"EdgeColors"}.  Any other name raises an error, as does an
## odd-length @var{nv_cell} or a value that is not a logical scalar.
##
## @seealso{bfsearch, dfsearch, __bfsearch_events_impl__,
## __dfsearch_events_impl__}
## @end deftypefn

function opts = __bfsdfs_parse_opts__ (fname, nv_cell)

  if (nargin != 2)
    print_usage ();
  endif

  opts = struct ("restart", false, "edgecolors", false);

  n = numel (nv_cell);
  if (n == 0)
    return;
  endif

  if (mod (n, 2) != 0)
    error ("Octave:invalid-input-arg", ...
           "%s: Name-Value pairs require an even number of arguments; '%s' is missing its value", ...
           fname, ...
           ifelse_str (ischar (nv_cell{end}) && isrow (nv_cell{end}), ...
                       nv_cell{end}, "<?>"));
  endif

  for k = 1:2:n
    name = nv_cell{k};
    value = nv_cell{k + 1};

    if (! (ischar (name) && isrow (name)))
      error ("Octave:invalid-input-arg", ...
             "%s: Name-Value option name must be a character row vector", ...
             fname);
    endif

    if (strcmpi (name, "Restart"))
      if (! (islogical (value) && isscalar (value)))
        error ("Octave:invalid-input-arg", ...
               "%s: Restart value must be a logical scalar", fname);
      endif
      opts.restart = logical (value);
    elseif (strcmpi (name, "EdgeColors"))
      if (! (islogical (value) && isscalar (value)))
        error ("Octave:invalid-input-arg", ...
               "%s: EdgeColors value must be a logical scalar", fname);
      endif
      opts.edgecolors = logical (value);
    else
      error ("Octave:invalid-input-arg", ...
             "%s: unknown option '%s'; valid names are 'Restart' and 'EdgeColors'", ...
             fname, name);
    endif
  endfor

endfunction

function s = ifelse_str (cond, a, b)
  if (cond)
    s = a;
  else
    s = b;
  endif
endfunction


## Private-helper smoke tests.

## Default options: both false.
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {});
%! assert (opts.restart, false);
%! assert (opts.edgecolors, false);

## Restart true parsed.
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {"Restart", true});
%! assert (opts.restart, true);

## EdgeColors true parsed.
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {"EdgeColors", true});
%! assert (opts.edgecolors, true);

## Case-insensitive names.
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {"restart", true});
%! assert (opts.restart, true);
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {"edgecolors", true});
%! assert (opts.edgecolors, true);

## Both options parsed, either order.
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {"Restart", true, "EdgeColors", true});
%! assert (opts.restart, true);
%! assert (opts.edgecolors, true);
%!test
%! opts = __bfsdfs_parse_opts__ ("bfsearch", {"EdgeColors", true, "Restart", true});
%! assert (opts.restart, true);
%! assert (opts.edgecolors, true);

## Error: unknown option name.
%!error <unknown option> ...
%! __bfsdfs_parse_opts__ ("bfsearch", {"Bogus", true})

## Error: odd argument count.
%!error <Name-Value|missing> ...
%! __bfsdfs_parse_opts__ ("bfsearch", {"Restart"})

## Error: non-char Name.
%!error <Name-Value|Name|option name> ...
%! __bfsdfs_parse_opts__ ("bfsearch", {7, true})

## Error: non-logical Restart value.
%!error <Restart.*logical> ...
%! __bfsdfs_parse_opts__ ("bfsearch", {"Restart", "yes"})

## Error: non-logical EdgeColors value.
%!error <EdgeColors.*logical> ...
%! __bfsdfs_parse_opts__ ("bfsearch", {"EdgeColors", "yes"})
