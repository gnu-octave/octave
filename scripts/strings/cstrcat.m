########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn {} {@var{str} =} cstrcat (@var{s1}, @var{s2}, @dots{})
## Return a string containing all the arguments concatenated horizontally
## with trailing white space preserved.
##
## For example:
##
## @example
## @group
## cstrcat ("ab   ", "cd")
##       @result{} "ab   cd"
## @end group
## @end example
##
## @example
## @group
## s = [ "ab"; "cde" ];
## cstrcat (s, s, s)
##       @result{} "ab ab ab "
##          "cdecdecde"
## @end group
## @end example
## @seealso{strcat, char, strvcat}
## @end deftypefn

function str = cstrcat (varargin)

  if (nargin == 0)
    ## Special case because if varargin is empty, iscellstr still returns
    ## true but then "[varargin{:}]" would be of class double.
    str = "";
  elseif (iscellstr (varargin))
    str = [varargin{:}];
  else
    error ("cstrcat: arguments must be character strings");
  endif

endfunction


## Test the dimensionality
## 1-D
%!assert (cstrcat ("ab ", "ab "), "ab ab ")
## 2-D
%!assert (cstrcat (["ab ";"cde"], ["ab ";"cde"]), ["ab ab ";"cdecde"])

%!assert (cstrcat ("foo", "bar"), "foobar")
%!assert (cstrcat (["a "; "bb"], ["foo"; "bar"]), ["a foo"; "bbbar"])

## Special null case
%!assert (cstrcat (), "")

## Test input validation
%!error <arguments must be character strings> cstrcat (1, 2)
