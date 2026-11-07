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
## @deftypefn {} {@var{order} =} __toposort_parse_opts__ (@var{nv_cell})
## Private helper that parses the @code{Name-Value} trailing arguments of
## @code{toposort}.
##
## @var{nv_cell} is the @code{varargin} cell array containing alternating
## name / value entries.  Returns the selected order as a lower-case char
## row (either @qcode{"stable"} or @qcode{"lexicographic"}, defaulting to
## @qcode{"stable"}).
##
## Recognised option names (case-insensitive) are @qcode{"Order"}.  Any
## other name raises an error, as does an odd-length @var{nv_cell}, a
## non-char name, or an invalid value.
##
## @seealso{toposort}
## @end deftypefn

function order = __toposort_parse_opts__ (nv_cell)

  if (nargin != 1)
    print_usage ();
  endif

  order = "stable";

  n = numel (nv_cell);
  if (n == 0)
    return;
  endif

  if (mod (n, 2) != 0)
    if (ischar (nv_cell{end}) && isrow (nv_cell{end}))
      last = nv_cell{end};
    else
      last = "<?>";
    endif
    error ("Octave:invalid-input-arg", ...
           "toposort: Name-Value pairs expected pairs; '%s' is missing its value", ...
           last);
  endif

  for k = 1:2:n
    name = nv_cell{k};
    value = nv_cell{k + 1};

    if (! (ischar (name) && isrow (name)))
      error ("Octave:invalid-input-arg", ...
             "toposort: Name-Value option name must be a string");
    endif

    if (strcmpi (name, "Order"))
      if (! (ischar (value) && isrow (value)))
        error ("Octave:invalid-input-arg", ...
               "toposort: Order value must be a string");
      endif
      if (strcmpi (value, "stable"))
        order = "stable";
      elseif (strcmpi (value, "lexicographic"))
        order = "lexicographic";
      else
        error ("Octave:invalid-input-arg", ...
               "toposort: Order value must be 'stable' or 'lexicographic'");
      endif
    else
      error ("Octave:invalid-input-arg", ...
             "toposort: unknown option '%s'; valid names are 'Order'", ...
             name);
    endif
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Default: order = "stable".
%!test
%! assert (__toposort_parse_opts__ ({}), "stable");

## Explicit Order=stable.
%!test
%! assert (__toposort_parse_opts__ ({"Order", "stable"}), "stable");

## Explicit Order=lexicographic.
%!test
%! assert (__toposort_parse_opts__ ({"Order", "lexicographic"}), "lexicographic");

## Case-insensitive name.
%!test
%! assert (__toposort_parse_opts__ ({"ORDER", "stable"}), "stable");
%!test
%! assert (__toposort_parse_opts__ ({"order", "lexicographic"}), "lexicographic");

## Case-insensitive value.
%!test
%! assert (__toposort_parse_opts__ ({"Order", "STABLE"}), "stable");
%!test
%! assert (__toposort_parse_opts__ ({"Order", "Lexicographic"}), "lexicographic");

## Unknown option name.
%!error <unknown option|Order>
%! __toposort_parse_opts__ ({"Bogus", "stable"});

## Odd argument count.
%!error <pairs|missing>
%! __toposort_parse_opts__ ({"Order"});

## Non-char Name.
%!error <name.*must be a string>
%! __toposort_parse_opts__ ({7, "stable"});

## Non-char Order value.
%!error <Order.*string>
%! __toposort_parse_opts__ ({"Order", 7});

## Invalid Order value.
%!error <Order.*stable|lexicographic>
%! __toposort_parse_opts__ ({"Order", "fast"});
