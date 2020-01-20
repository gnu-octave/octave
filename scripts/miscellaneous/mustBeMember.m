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
## @deftypefn {} {} mustBeMember (@var{x}, @var{valid})
##
## Requires that input @var{x} is a member of a set of given valid values.
##
## Raises an error if any element of the input @var{x} is not a member
## of @var{valid}, as determined by @code{ismember (@var{x})}.
##
## Note that char inputs may behave weirdly, because of the interaction
## between chars and cellstrings when calling ismember() on them.  But it
## will probably "do what you mean" if you just use it naturally.
##
## @end deftypefn

function mustBeMember (x, valid)
  tf = ismember (x, valid);
  if ! all (tf)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    n_bad = numel (find (! tf));
    # TODO: Fancy inclusion of bad & valid values in the error message.
    # Probably use mat2str() in a try/catch for that.
    error ( ...
      "%s must be one of the specified valid values; got %d elements that weren't", ...
      label, n_bad);
  endif
endfunction

%!test
%! mustBeMember (42, 38:50)
%! mustBeMember ('foo', {'foo', 'bar', 'baz'})
%! mustBeMember (38:42, 37:43)
%! mustBeMember ({'foo','bar'}, {'foo', 'bar', 'baz'})

%!error mustBeMember ()
%!error mustBeMember (42)
%!error mustBeMember (42, 1:5)
%!error mustBeMember ('nope', {'foo', 'bar', 'baz'})
