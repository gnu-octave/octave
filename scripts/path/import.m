## Copyright (C) 2017 Rik Wehbring
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {} import PACKAGE.FUNCTION
## @deftypefn {} {} import PACKAGE.CLASS
## @deftypefn {} {} import PACKAGE.*
## @deftypefn {} {} import
## @deftypefn {} {@var{list} =} import
##
## Import function or classes into the current scope.
##
## @strong{Warning:} This functionality is not yet implemented, and invoking
## the function will emit an error.
##
## When invoked with the name of a PACKAGE and a FUNCTION or CLASS name, that
## name is made available in the current code without having to use namespace
## qualifiers.  This can facilitate the readability of the code, and require
## less typing by programmers.
##
## Example
##
## @example
## import containers.Map;
##
## m = Map ({"A", "B"}, {[1], [2]});
## @end example
##
## When called with no inputs and no outputs @code{import} prints a list of 
## any import definitions.
##
## When called with no inputs and one output, a cell array of strings
## @var{list} is returned with any import definitions. 
##
## @end deftypefn

function list = import (varargin)

  error ("the import function is not yet implemented in Octave");

endfunction


%!error <not yet implemented> import ("foobar"); 
