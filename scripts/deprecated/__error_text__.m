## Copyright (C) 2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Built-in Function} {[@var{msg}, @var{msgid}] =} __error_text__ (@var{msg}, @var{msgid})
## This function has been deprecated.  Use @code{lasterr} instead.
## @seealso{lasterr}
## @end deftypefn

function [msg, msgid] = __error_text__ (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "__error_text__ is obsolete and will be removed from a future version of Octave, please use lasterr instead");
  endif

  [msg, msgid] = lasterr (varargin{:});

endfunction
