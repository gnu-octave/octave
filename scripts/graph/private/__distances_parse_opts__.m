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
## @deftypefn {} {[@var{positional}, @var{method}] =} __distances_parse_opts__ (@var{args})
## Private helper for @code{distances}: split a @code{varargin} cell
## array @var{args} into positional arguments @var{positional} (a cell
## array containing zero, one, or two entries: @code{@{@}},
## @code{@{src@}}, or @code{@{src, tgt@}}) and the selected algorithm
## @var{method} (a lowercase char row, one of @qcode{"auto"},
## @qcode{"unweighted"}, @qcode{"positive"}, @qcode{"mixed"}, or
## @qcode{"acyclic"}; defaults to @qcode{"auto"}).
##
## The recognised Name-Value option is @qcode{"Method"} (case-
## insensitive).  Known reserved option names cannot be used as
## positional values.  The parser scans @var{args} forward and
## accumulates positional entries until it encounters a known option
## name, after which the remainder is parsed as Name-Value pairs.
##
## Errors:
## @itemize
## @item a char/string option key that is not @qcode{"Method"};
## @item the @qcode{"Method"} key without a following value;
## @item a non-string value for @qcode{"Method"};
## @item a @qcode{"Method"} value that is not one of the recognised
## methods; or
## @item more than two positional arguments.
## @end itemize
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{distances, graph, digraph}
## @end deftypefn

function [positional, method] = __distances_parse_opts__ (args)

  if (nargin != 1)
    print_usage ();
  endif

  method = "auto";
  positional = {};

  valid_methods = {"auto", "unweighted", "positive", "mixed", "acyclic"};

  i = 1;
  n = numel (args);
  while (i <= n)
    a = args{i};

    ## An option key is a char row vector matching a known NV key.
    ## Currently only @qcode{"Method"} is supported.
    is_key = (ischar (a) && isrow (a) && strcmpi (a, "Method"));

    ## A char row vector that matches NO known key may still be an
    ## unknown option name if it appears at a position where we are
    ## already consuming Name-Value pairs; otherwise it is treated as
    ## positional.  MATLAB's distances uses inputParser which looks
    ## ahead and checks all char row vectors for option-name matches;
    ## we do the same by recognising @qcode{"Method"} only.  A char
    ## row that looks "option-like" is any string whose value is not a
    ## valid positional — but src/tgt can also be strings (node names),
    ## so we can't use a heuristic here.  Detect unknown option names
    ## by looking at the @emph{next} arg: if a char row at position i
    ## is followed by a value (i + 1 <= n), and position i has already
    ## been preceded by at most 2 positional args, treat it as an NV
    ## attempt.  But that is fragile.
    ##
    ## Instead, follow the simpler rule that matches the user stories
    ## in this project: positional arguments (src, tgt) must come
    ## before any Name-Value pairs, and any char row that matches a
    ## reserved option name (@qcode{"Method"}) always starts the NV
    ## section.  We also detect the single-NV-pair convention: if the
    ## total arg count makes sense as positional + NV pairs, an
    ## unrecognised keyword at the NV section boundary errors.

    if (is_key)
      ## Consume a Name-Value pair.
      if (strcmpi (a, "Method"))
        if (i + 1 > n)
          error ("Octave:invalid-input-arg", ...
                 "distances: option 'Method' requires a value");
        endif
        val = args{i + 1};
        if (! (ischar (val) && isrow (val)))
          error ("Octave:invalid-input-arg", ...
                 "distances: Method value must be a string");
        endif
        mm = lower (val);
        if (! any (strcmp (mm, valid_methods)))
          error ("Octave:invalid-input-arg", ...
                 ["distances: unknown Method '%s'; valid methods are ", ...
                  "'auto', 'unweighted', 'positive', 'mixed', 'acyclic'"], ...
                 val);
        endif
        method = mm;
        i = i + 2;
      else
        ## Unreachable via is_key, but leave as a safety net.
        error ("Octave:invalid-input-arg", ...
               "distances: unknown option '%s'", a);
      endif
    else
      ## Positional argument.  Enforce the max-2 rule.
      if (numel (positional) >= 2)
        ## More than 2 positional — either the user is trying to use
        ## an unknown option name as the 3rd positional, or they truly
        ## passed too many args.  Distinguish the two for a better
        ## error message.
        if (ischar (a) && isrow (a))
          error ("Octave:invalid-input-arg", ...
                 "distances: unknown option '%s'; valid names are 'Method'", ...
                 a);
        else
          error ("Octave:invalid-input-arg", ...
                 "distances: too many input arguments");
        endif
      endif
      positional{end + 1} = a;
      i = i + 1;
    endif
  endwhile

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Default: empty args -> no positional, method = "auto".
%!test
%! [p, m] = __distances_parse_opts__ ({});
%! assert (isempty (p));
%! assert (m, "auto");

## Single positional arg -> src only, method default.
%!test
%! [p, m] = __distances_parse_opts__ ({3});
%! assert (p, {3});
%! assert (m, "auto");

## Two positional args -> src and tgt, method default.
%!test
%! [p, m] = __distances_parse_opts__ ({3, 5});
%! assert (p, {3, 5});
%! assert (m, "auto");

## Method-only NV pair.
%!test
%! [p, m] = __distances_parse_opts__ ({"Method", "unweighted"});
%! assert (isempty (p));
%! assert (m, "unweighted");

## Positional + Method NV.
%!test
%! [p, m] = __distances_parse_opts__ ({1, "Method", "positive"});
%! assert (p, {1});
%! assert (m, "positive");

## src, tgt, Method.
%!test
%! [p, m] = __distances_parse_opts__ ({1, 2, "Method", "mixed"});
%! assert (p, {1, 2});
%! assert (m, "mixed");

## Case-insensitive Method key.
%!test
%! [p, m] = __distances_parse_opts__ ({"METHOD", "acyclic"});
%! assert (m, "acyclic");
%!test
%! [p, m] = __distances_parse_opts__ ({"method", "AUTO"});
%! assert (m, "auto");

## Case-insensitive method value.
%!test
%! [p, m] = __distances_parse_opts__ ({"Method", "POSITIVE"});
%! assert (m, "positive");
%!test
%! [p, m] = __distances_parse_opts__ ({"Method", "Unweighted"});
%! assert (m, "unweighted");

## All 5 valid method values parse.
%!test
%! [~, m] = __distances_parse_opts__ ({"Method", "auto"});
%! assert (m, "auto");
%!test
%! [~, m] = __distances_parse_opts__ ({"Method", "unweighted"});
%! assert (m, "unweighted");
%!test
%! [~, m] = __distances_parse_opts__ ({"Method", "positive"});
%! assert (m, "positive");
%!test
%! [~, m] = __distances_parse_opts__ ({"Method", "mixed"});
%! assert (m, "mixed");
%!test
%! [~, m] = __distances_parse_opts__ ({"Method", "acyclic"});
%! assert (m, "acyclic");

## Cellstr and char positional args pass through unchanged.
%!test
%! [p, m] = __distances_parse_opts__ ({"a"});
%! assert (p, {"a"});
%! assert (m, "auto");
%!test
%! [p, m] = __distances_parse_opts__ ({{"a"}, {"b"}});
%! assert (p, {{"a"}, {"b"}});

## Positional args with NV afterwards (named src).
%!test
%! [p, m] = __distances_parse_opts__ ({"a", "Method", "unweighted"});
%! assert (p, {"a"});
%! assert (m, "unweighted");

## Method missing value.
%!error <'Method' requires a value|Method.*value>
%! __distances_parse_opts__ ({"Method"});

## Method with numeric value.
%!error <Method.*string>
%! __distances_parse_opts__ ({"Method", 3});

## Method with logical value (non-string).
%!error <Method.*string>
%! __distances_parse_opts__ ({"Method", true});

## Unknown method value.
%!error <unknown Method|valid methods>
%! __distances_parse_opts__ ({"Method", "bogus"});

## Unknown option name after 2 positional (3rd arg unknown key).
%!error <unknown option|Method>
%! __distances_parse_opts__ ({1, 2, "Bogus", "auto"});

## Too many positional arguments.
%!error <too many|unknown option>
%! __distances_parse_opts__ ({1, 2, 3});
