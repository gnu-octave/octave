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
## @deftypefn {} {@var{vars} =} symvar (@var{str})
## Identify the symbolic variable names in the string @var{str}.
##
## Common constant names such as @code{i}, @code{j}, @code{pi}, @code{Inf} and
## Octave functions such as @code{sin} or @code{plot} are ignored.
##
## Any names identified are returned in a cell array of strings.  The array is
## empty if no variables were found.
##
## Example:
##
## @example
## @group
## symvar ("x^2 + y^2 == 4")
## @result{} @{
##      [1,1] = x
##      [2,1] = y
##    @}
## @end group
## @end example
## @end deftypefn

function vars = symvar (str)

  warning ("off", "Octave:legacy-function", "local");  # using inline below.
  vars = argnames (inline (str));
  ## Correct for auto-generated 'x' variable when no symvar was found.
  if (numel (vars) == 1 && strcmp (vars{1}, "x") && ! any (str == "x"))
    vars = {};
  endif

endfunction


%!assert (symvar ("3*x + 4*y + 5*cos (z)"), {"x"; "y"; "z"})
%!assert (symvar ("sin ()^2 + cos ()^2 == 1"), {})
%!assert (symvar ("1./x"), {"x"})
