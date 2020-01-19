########################################################################
##
## Copyright (C) 2019-2020 The Octave Project Developers
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
## @deftypefn {} {@var{x} =} mustBeNonzero (@var{x})
##
## Requires that input @var{x} is not zero.
##
## Raises an error if any element of the input @var{x} is zero, as determined
## by @code{@var{x} != 0}.
##
## @end deftypefn

function x = mustBeNonzero (x)
  tf = x != 0;
  tf = tf(:);
  if ! all (tf)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    ix_bad = find (! tf);
    errmsg = sprintf ( ...
      "%s must be non-zero; got %d elements that were zero: indexes %s", ...
      label, numel (ix_bad), mat2str (ix_bad));
    error (errmsg);
  endif
endfunction
