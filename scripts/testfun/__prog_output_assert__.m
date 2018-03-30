## Copyright (C) 2005-2018 David Bateman
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

## -*- texinfo -*-
## @deftypefn {} {} __prog_output_assert__ (@var{str})
## Undocumented internal function.
## @end deftypefn

function ret = __prog_output_assert__ (str)
  global __assert_printf__ = "";

  if (isempty (__assert_printf__))
    ret = isempty (str);
  elseif (__assert_printf__(end) == "\n")
    ret = strcmp (__assert_printf__(1:(end-1)), str);
  else
    ret = strcmp (__assert_printf__, str);
  endif

  __assert_printf__ = "";

endfunction


## No test coverage for internal function.  It is tested through calling fcn.
%!assert (1)
