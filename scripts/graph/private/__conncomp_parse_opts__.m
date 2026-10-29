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
## @deftypefn {} {@var{opts} =} __conncomp_parse_opts__ (@var{is_digraph}, @var{nv_cell})
## Private helper that parses the @code{Name-Value} trailing arguments of
## @code{conncomp}.
##
## @var{is_digraph} is a logical scalar: @code{true} when called from the
## @code{digraph} class method, @code{false} when called from the
## @code{graph} class method.  For @code{graph}, only
## @qcode{"Type","weak"} is accepted.
##
## @var{nv_cell} is the @code{varargin} cell array containing alternating
## name / value entries.
##
## Returns a scalar struct with fields:
##
## @itemize
## @item
## @code{type} (char row, either @qcode{"weak"} or @qcode{"strong"};
## default @qcode{"weak"}).
## @item
## @code{outputform} (char row, either @qcode{"vector"} or
## @qcode{"cell"}; default @qcode{"vector"}).
## @end itemize
##
## Recognised option names (case-insensitive) are @qcode{"Type"} and
## @qcode{"OutputForm"}.  Any other name raises an error, as does an
## odd-length @var{nv_cell}, a non-char name, or an invalid value.
##
## @seealso{conncomp}
## @end deftypefn

function opts = __conncomp_parse_opts__ (is_digraph, nv_cell)

  if (nargin != 2)
    print_usage ();
  endif

  opts = struct ("type", "weak", "outputform", "vector");

  n = numel (nv_cell);
  if (n == 0)
    return;
  endif

  if (mod (n, 2) != 0)
    error ("Octave:invalid-input-arg", ...
           "conncomp: Name-Value pairs expected pairs; '%s' is missing its value", ...
           ifelse_str (ischar (nv_cell{end}) && isrow (nv_cell{end}), ...
                       nv_cell{end}, "<?>"));
  endif

  for k = 1:2:n
    name = nv_cell{k};
    value = nv_cell{k + 1};

    if (! (ischar (name) && isrow (name)))
      error ("Octave:invalid-input-arg", ...
             "conncomp: Name-Value option name must be a string");
    endif

    if (strcmpi (name, "Type"))
      if (! (ischar (value) && isrow (value)))
        error ("Octave:invalid-input-arg", ...
               "conncomp: Type value must be a string");
      endif
      if (strcmpi (value, "weak"))
        opts.type = "weak";
      elseif (strcmpi (value, "strong"))
        if (! is_digraph)
          error ("Octave:invalid-input-arg", ...
                 "conncomp: Type 'strong' is only valid for a digraph; must be 'weak' for a graph");
        endif
        opts.type = "strong";
      else
        error ("Octave:invalid-input-arg", ...
               "conncomp: Type value must be 'weak' or 'strong'");
      endif
    elseif (strcmpi (name, "OutputForm"))
      if (! (ischar (value) && isrow (value)))
        error ("Octave:invalid-input-arg", ...
               "conncomp: OutputForm value must be a string");
      endif
      if (strcmpi (value, "vector"))
        opts.outputform = "vector";
      elseif (strcmpi (value, "cell"))
        opts.outputform = "cell";
      else
        error ("Octave:invalid-input-arg", ...
               "conncomp: OutputForm value must be 'vector' or 'cell'");
      endif
    else
      error ("Octave:invalid-input-arg", ...
             "conncomp: unknown option '%s'; valid names are 'Type' and 'OutputForm'", ...
             name);
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


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Default options: type='weak', outputform='vector'.
%!test
%! opts = __conncomp_parse_opts__ (true, {});
%! assert (opts.type, "weak");
%! assert (opts.outputform, "vector");

## Type='strong' for digraph.
%!test
%! opts = __conncomp_parse_opts__ (true, {"Type", "strong"});
%! assert (opts.type, "strong");

## Type='weak' explicit.
%!test
%! opts = __conncomp_parse_opts__ (true, {"Type", "weak"});
%! assert (opts.type, "weak");

## OutputForm='cell'.
%!test
%! opts = __conncomp_parse_opts__ (true, {"OutputForm", "cell"});
%! assert (opts.outputform, "cell");

## Case-insensitive names and values.
%!test
%! opts = __conncomp_parse_opts__ (true, {"type", "STRONG"});
%! assert (opts.type, "strong");
%!test
%! opts = __conncomp_parse_opts__ (true, {"TYPE", "Weak"});
%! assert (opts.type, "weak");
%!test
%! opts = __conncomp_parse_opts__ (true, {"outputform", "CELL"});
%! assert (opts.outputform, "cell");

## Both options parsed, either order.
%!test
%! opts = __conncomp_parse_opts__ (true, {"Type", "strong", "OutputForm", "cell"});
%! assert (opts.type, "strong");
%! assert (opts.outputform, "cell");
%!test
%! opts = __conncomp_parse_opts__ (true, {"OutputForm", "cell", "Type", "strong"});
%! assert (opts.type, "strong");
%! assert (opts.outputform, "cell");

## For graph, 'strong' is rejected.
%!error <Type 'strong' is only valid for a digraph|must be 'weak'>
%! __conncomp_parse_opts__ (false, {"Type", "strong"});

## For graph, 'weak' is fine.
%!test
%! opts = __conncomp_parse_opts__ (false, {"Type", "weak"});
%! assert (opts.type, "weak");

## Unknown option name.
%!error <unknown option>
%! __conncomp_parse_opts__ (true, {"Bogus", "weak"});

## Odd argument count.
%!error <Name-Value|expected pairs|missing>
%! __conncomp_parse_opts__ (true, {"Type"});

## Non-char Name.
%!error <Name|option name|must be a string>
%! __conncomp_parse_opts__ (true, {7, "weak"});

## Non-char Type value.
%!error <Type.*must be a string>
%! __conncomp_parse_opts__ (true, {"Type", 7});

## Invalid Type value.
%!error <Type.*must be.*weak|strong>
%! __conncomp_parse_opts__ (true, {"Type", "foo"});

## Invalid OutputForm value.
%!error <OutputForm.*must be.*vector|cell>
%! __conncomp_parse_opts__ (true, {"OutputForm", "matrix"});

## Non-char OutputForm value.
%!error <OutputForm.*must be a string>
%! __conncomp_parse_opts__ (true, {"OutputForm", 7});
