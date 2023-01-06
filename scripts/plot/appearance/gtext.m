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
## @deftypefn  {} {} gtext (@var{s})
## @deftypefnx {} {} gtext (@{@var{s1}, @var{s2}, @dots{}@})
## @deftypefnx {} {} gtext (@{@var{s1}; @var{s2}; @dots{}@})
## @deftypefnx {} {} gtext (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {@var{h} =} gtext (@dots{})
## Place text on the current figure using the mouse.
##
## The string argument @var{s} may be a character array or a cell array
## of strings.  If @var{s} has more than one row, each row is used
## to create a separate text object after a mouse click.  For example:
##
## Place a single string after one mouse click
##
## @example
## gtext ("I clicked here")
## @end example
##
## Place two strings after two mouse clicks
##
## @example
## gtext (@{"I clicked here"; "and there"@})
## @end example
##
## Place two strings, each with two lines, after two mouse clicks
##
## @example
## gtext (@{"I clicked", "here"; "and", "there"@})
## @end example
##
## Optional @var{property}/@var{value} pairs are passed directly to the
## underlying text objects.
##
## The full list of text object properties is documented at
## @ref{Text Properties}.
##
## The optional return value @var{h} holds the graphics handle(s) to the
## created text object(s).
## @seealso{ginput, text}
## @end deftypefn

function h = gtext (s, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (ischar (s) || iscellstr (s)))
    error ("gtext: S must a character array or cell array of strings");
  endif

  htmp = [];
  if (! isempty (s))
    for i = 1:rows (s)
      [x, y] = ginput (1);
      htmp = [htmp, text(x, y, s(i,:), varargin{:})];
    endfor
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


## Remove from test statistics.  No real tests possible.
%!assert (1)
