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
## @deftypefn {} {@var{method} =} __shortestpath_parse_method__ (@var{caller}, @var{args})
## Private helper for the @code{shortestpath} and
## @code{shortestpathtree} methods: parse an optional
## @qcode{"Method"} Name-Value pair from the trailing @var{args} cell
## array and return the resolved method name @var{method} (a
## lowercase char row vector: one of @qcode{"auto"}, @qcode{"positive"},
## or @qcode{"mixed"}).  The default is @qcode{"auto"}.
##
## @var{caller} is the calling function's name used in error
## messages (@qcode{"shortestpath"} or @qcode{"shortestpathtree"}).
##
## Only the @qcode{"Method"} NV key is recognised here; other options
## (for example @qcode{"OutputForm"} on
## @code{shortestpathtree}) must be handled by the caller before or
## after invoking this helper.  This helper treats its input as a
## flat list of NV pairs and errors if any unexpected key is present.
##
## Supported Method values are @qcode{"auto"}, @qcode{"positive"},
## and @qcode{"mixed"}.  @qcode{"unweighted"} and @qcode{"acyclic"}
## are reserved names from the @code{distances} method set; they are
## accepted as NV values here but raise a @qcode{not yet supported}
## error so the caller can surface a useful message.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{shortestpath, shortestpathtree, graph, digraph}
## @end deftypefn

function method = __shortestpath_parse_method__ (caller, args)

  if (nargin != 2)
    print_usage ();
  endif

  method = "auto";

  n = numel (args);
  if (n == 0)
    return;
  endif

  ## The only recognised option is 'Method'.  Scan NV pairs.
  i = 1;
  while (i <= n)
    a = args{i};
    if (! (ischar (a) && isrow (a)))
      error ("Octave:invalid-input-arg", ...
             "%s: option names must be strings", caller);
    endif
    if (strcmpi (a, "Method"))
      if (i + 1 > n)
        error ("Octave:invalid-input-arg", ...
               "%s: option 'Method' requires a value", caller);
      endif
      val = args{i + 1};
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "%s: Method value must be a string", caller);
      endif
      mm = lower (val);
      switch (mm)
        case {"auto", "positive", "mixed"}
          method = mm;
        case {"unweighted", "acyclic"}
          error ("Octave:invalid-input-arg", ...
                 "%s: Method '%s' is not yet supported", caller, val);
        otherwise
          error ("Octave:invalid-input-arg", ...
                 ["%s: unknown Method '%s'; valid methods are ", ...
                  "'auto', 'positive', 'mixed'"], caller, val);
      endswitch
      i = i + 2;
    else
      error ("Octave:invalid-input-arg", ...
             "%s: unknown option '%s'", caller, a);
    endif
  endwhile

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty args: default is 'auto'.
%!test
%! m = __shortestpath_parse_method__ ("shortestpath", {});
%! assert (m, "auto");

## Explicit 'auto' returns 'auto'.
%!test
%! m = __shortestpath_parse_method__ ("shortestpath", {"Method", "auto"});
%! assert (m, "auto");

## 'positive' and 'mixed' round-trip.
%!test
%! m = __shortestpath_parse_method__ ("shortestpath", {"Method", "positive"});
%! assert (m, "positive");
%!test
%! m = __shortestpath_parse_method__ ("shortestpath", {"Method", "mixed"});
%! assert (m, "mixed");

## Method name and value are case-insensitive.
%!test
%! m = __shortestpath_parse_method__ ("shortestpath", {"METHOD", "MIXED"});
%! assert (m, "mixed");
%!test
%! m = __shortestpath_parse_method__ ("shortestpath", {"method", "Positive"});
%! assert (m, "positive");

## Missing value for Method.
%!error <Method.*value|value>
%! __shortestpath_parse_method__ ("shortestpath", {"Method"});

## Numeric value for Method.
%!error <Method.*string|string>
%! __shortestpath_parse_method__ ("shortestpath", {"Method", 7});

## Unknown Method value.
%!error <Method|unknown>
%! __shortestpath_parse_method__ ("shortestpath", {"Method", "bogus"});

## 'unweighted' and 'acyclic' are reserved and error as "not yet supported".
%!error <not yet supported>
%! __shortestpath_parse_method__ ("shortestpath", {"Method", "unweighted"});
%!error <not yet supported>
%! __shortestpath_parse_method__ ("shortestpath", {"Method", "acyclic"});

## Unknown option name.
%!error <unknown option>
%! __shortestpath_parse_method__ ("shortestpath", {"Bogus", "auto"});

## Non-string option key.
%!error <option names must be strings>
%! __shortestpath_parse_method__ ("shortestpath", {42, "auto"});

## Caller name is used in error messages.
%!error <shortestpathtree: unknown Method>
%! __shortestpath_parse_method__ ("shortestpathtree", {"Method", "bogus"});

## No-args call errors.
%!error __shortestpath_parse_method__ ()

## Wrong argcount errors.
%!error __shortestpath_parse_method__ ("shortestpath")
