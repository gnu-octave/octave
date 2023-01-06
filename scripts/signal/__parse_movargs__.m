########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn {} {@var{args} =} __parse_movargs__ (@var{caller}, @var{varargin})
##
## Parse arguments for movXXX functions before passing to @code{movfun}.
##
## The input @var{caller} is a string with the name of the calling function
## and is used to personalize any error messages.
## @seealso{movfun}
## @end deftypefn

function args = __parse_movargs__ (caller, varargin)

  args = {};

  have_dim = have_nancond = false;
  imax = numel (varargin);
  i = 1;
  while (i <= imax)
    arg = varargin{i};
    if (ischar (arg))
      if (any (strcmpi (arg, {"omitnan", "includenan"})))
        args(end+(1:2)) = {"nancond", arg};
        have_nancond = true;
      else
        i += 1;  # Prop/Val pair
        if (i > imax)
          error ([caller ": property '%s' missing value argument"], arg);
        endif
        args(end+(1:2)) = {arg, varargin{i}};
      endif
    elseif (isnumeric (arg))
      args(end+(1:2)) = {"dim", arg};
      have_dim = true;
    else
      error ("Octave:invalid-input-arg",
             [caller ": invalid input at position %d"], i);
    endif

    i += 1;  # Advance to next element
    if (have_nancond && have_dim)
      args = [args, varargin(i:end)];
      break;
    endif
  endwhile

endfunction


%!test
%! caller = "tstblock";
%! vararg = {5};
%! assert (__parse_movargs__ (caller, vararg{:}), {"dim", 5});
%! vararg = {"Endpoints", "shrink", 3};
%! assert (__parse_movargs__ (caller, vararg{:}),
%!         {"Endpoints", "shrink", "dim", 3});
%! vararg = {"includenan", 2};
%! assert (__parse_movargs__ (caller, vararg{:}),
%!         {"nancond", "includenan", "dim", 2});

%!test
%! caller = "tstblock";
%! vararg = {"INCLUDENAN"};
%! assert (__parse_movargs__ (caller, vararg{:}), {"nancond", "INCLUDENAN"});
%! vararg = {"Endpoints", "fill", "OMITNAN"};
%! assert (__parse_movargs__ (caller, vararg{:}),
%!         {"Endpoints", "fill", "nancond", "OMITNAN"});
%! vararg = {2, "includenan"};
%! assert (__parse_movargs__ (caller, vararg{:}),
%!         {"dim", 2, "nancond", "includenan"});

%!test
%! caller = "tstblock";
%! vararg = {};
%! assert (__parse_movargs__ (caller, vararg{:}), {});
%! vararg = {"Endpoints", "fill"};
%! assert (__parse_movargs__ (caller, vararg{:}), {"Endpoints", "fill"});
%! vararg = {5, "omitnan", "Endpoints", "fill"};
%! assert (__parse_movargs__ (caller, vararg{:}),
%!         {"dim", 5, "nancond", "omitnan", "Endpoints", "fill"});
