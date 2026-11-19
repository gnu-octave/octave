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
## @deftypefn {} {@var{algorithm} =} __maxflow_parse_algorithm__ (@var{args})
## Private helper for @code{maxflow}: validate the optional positional
## @var{algorithm} argument and return a canonical lowercase name.
##
## @var{args} is the @code{varargin} cell array captured after the
## required @code{(G, s, t)} triple.  A missing algorithm argument
## returns the default @qcode{"augmentpath"}.  A single char row
## vector is matched case-insensitively against the accepted names:
## @qcode{"augmentpath"} and @qcode{"searchtrees"}.
##
## Errors:
## @itemize
## @item more than one extra positional argument;
## @item a non-string algorithm value (numeric, cell array, etc.); or
## @item an unknown algorithm name (including the MATLAB-only
## @qcode{"pushrelabel"}).
## @end itemize
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{maxflow}
## @end deftypefn

function algorithm = __maxflow_parse_algorithm__ (args)

  if (nargin != 1)
    print_usage ();
  endif

  valid = {"augmentpath", "searchtrees"};

  algorithm = "augmentpath";

  n = numel (args);
  if (n == 0)
    return;
  endif
  if (n > 1)
    error ("Octave:invalid-input-arg", ...
           "maxflow: too many input arguments");
  endif

  a = args{1};
  if (! (ischar (a) && isrow (a)) || isempty (a))
    error ("Octave:invalid-input-arg", ...
           "maxflow: algorithm must be a non-empty string");
  endif

  al = lower (a);
  if (! any (strcmp (al, valid)))
    error ("Octave:invalid-input-arg", ...
           ["maxflow: unknown algorithm '%s'; valid algorithms are ", ...
            "'augmentpath', 'searchtrees'"], a);
  endif

  algorithm = al;

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Default: no extra args -> 'augmentpath'.
%!test
%! assert (__maxflow_parse_algorithm__ ({}), "augmentpath");

## Explicit 'augmentpath' passes through.
%!test
%! assert (__maxflow_parse_algorithm__ ({"augmentpath"}), "augmentpath");

## Explicit 'searchtrees' passes through.
%!test
%! assert (__maxflow_parse_algorithm__ ({"searchtrees"}), "searchtrees");

## Case-insensitive matching on 'augmentpath'.
%!test
%! assert (__maxflow_parse_algorithm__ ({"AUGMENTPATH"}), "augmentpath");
%! assert (__maxflow_parse_algorithm__ ({"AugmentPath"}), "augmentpath");
%! assert (__maxflow_parse_algorithm__ ({"augmentPATH"}), "augmentpath");

## Case-insensitive matching on 'searchtrees'.
%!test
%! assert (__maxflow_parse_algorithm__ ({"SEARCHTREES"}), "searchtrees");
%! assert (__maxflow_parse_algorithm__ ({"SearchTrees"}), "searchtrees");
%! assert (__maxflow_parse_algorithm__ ({"searchTREES"}), "searchtrees");

## Too many args errors.
%!error <too many input arguments>
%! __maxflow_parse_algorithm__ ({"augmentpath", "extra"});

## Numeric arg errors.
%!error <algorithm.*string>
%! __maxflow_parse_algorithm__ ({42});

## Logical arg errors (non-string).
%!error <algorithm.*string>
%! __maxflow_parse_algorithm__ ({true});

## Empty-string arg errors.
%!error <algorithm.*string>
%! __maxflow_parse_algorithm__ ({""});

## Cell-array arg errors.
%!error <algorithm.*string>
%! __maxflow_parse_algorithm__ ({{"augmentpath"}});

## Unknown algorithm errors.
%!error <unknown algorithm|valid algorithms>
%! __maxflow_parse_algorithm__ ({"bogus"});

## MATLAB-only 'pushrelabel' errors (not supported here).
%!error <unknown algorithm|valid algorithms>
%! __maxflow_parse_algorithm__ ({"pushrelabel"});

## No args errors (print_usage).
%!error __maxflow_parse_algorithm__ ()
