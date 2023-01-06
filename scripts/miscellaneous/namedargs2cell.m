########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn {} {@var{c} =} namedargs2cell (@var{s})
## Create a cell array of field name/value pairs from a scalar structure.
##
## Example:
##
## @example
## @group
## @c doctest: +SKIP
## s.Name = "Peter";
## s.Height = 185;
## s.Age = 42;
##
## c = namedargs2cell (s)
##   @result{} @{ "Name", "Peter", "Height", 185, "Age", 42 @}
## @end group
## @end example
##
## @seealso{struct2cell}
## @end deftypefn

function c = namedargs2cell (s)

  if (nargin < 1)
    print_usage ();
  elseif (! isstruct (s) || ! isscalar (s))
    error ("namedargs2cell: S must be a scalar structure");
  endif

  c = reshape ([fieldnames(s), struct2cell(s)].', 1, []);

endfunction


%!test
%! data = { "Name", "Peter", "Height", 185, "Age", 42};
%! s = struct (data{:});
%! c = namedargs2cell (s);
%! assert (isequal (c, data));

## Test input validation
%!error <Invalid call> namedargs2cell ()
%!error <called with too many inputs> namedargs2cell (1, 2)
%!error <S must be a scalar structure> namedargs2cell (true)
%!error <S must be a scalar structure> namedargs2cell (struct ("name", {1, 2}))
