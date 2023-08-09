########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{val} =} optimget (@var{options}, @var{par})
## @deftypefnx {} {@var{val} =} optimget (@var{options}, @var{par}, @var{default})
## Return the value of the specific parameter @var{par} from the optimization
## options structure @var{options} created by @code{optimset}.
##
## If @var{par} is not defined then return the @var{default} value if
## supplied, otherwise return an empty matrix.
##
## If @var{par} does not exactly match the name of a standard parameter,
## @code{optimget} will attempt to expand the name to match a standard
## parameter and will return that parameter's value if a match is found.
## @code{optimget} produces an error if it finds multiple ambiguous matches.
## If no standard parameter matches are found a warning is issued.  See
## @code{optimset} for information about the standard options list.
##
## Note that any non-standard field names in @var{options} are ignored when
## matching option names, and @var{par} will always be expanded
## to match a standard option even if an exact non-standard match exists.  The
## value of a non-standard parameter that is ambigious with one or more
## standard parameters cannot be returned by @code{optimget} and can only be
## accessed using @code{getfield} or dot notation for structs.
## @seealso{optimset}
## @end deftypefn

function optval = optimget (options, optname, default)

  if (nargin < 2 || ! isstruct (options) || ! ischar (optname))
    print_usage ();
  endif

  ## Expand partial-length names into full names
  opts = __all_opts__ ();
  idx = strncmpi (opts, optname, length (optname));
  nmatch = sum (idx);

  if (nmatch == 1)
    optname = opts{idx};
  elseif (nmatch == 0)
    warning ("optimget: unrecognized option: '%s'", optname);
  else
    fmt = sprintf ("optimget: ambiguous option: '%%s' (%s%%s)",
                   repmat ("%s, ", 1, nmatch-1));
    error (fmt, optname, opts{idx});
  endif

  if (isfield (options, optname) && ! isempty (options.(optname)))
    optval = options.(optname);
  elseif (nargin > 2)
    optval = default;
  else
    optval = [];
  endif

endfunction


%!shared opts
%! opts = optimset ("tolx", 0.1, "maxit", 100);
%!assert (optimget (opts, "TolX"), 0.1)
%!assert (optimget (opts, "maxit"), 100)
%!assert (optimget (opts, "MaxITer"), 100)
%!assert (optimget (opts, "TolFun"), [])
%!assert (optimget (opts, "TolFun", 1e-3), 1e-3)

## Test input validation
%!error <Invalid call> optimget ()
%!error <Invalid call> optimget (1)
%!error optimget (1, "name")
%!error optimget (struct (), 2)
%!warning <unrecognized option: 'foobar'> (optimget (opts, "foobar"));
%!error <ambiguous option: 'Max'> (optimget (opts, "Max"));
