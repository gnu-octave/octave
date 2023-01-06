########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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

%!test <*57988>
%! fh = @get;
%! fh = @set;
%! fh = @enumeration;
%! fh = @events;
%! fh = @methods;
%! fh = @properties;

%!test <*57988>
%!  kwords = iskeyword ();
%!  for i = 1:numel (kwords)
%!    unexpected_success = true;
%!    kword = kwords{i};
%!    try
%!      eval (sprintf ("@%s;", kword));
%!    catch
%!      unexpected_success = false;
%!    end_try_catch
%!    if (unexpected_success)
%!      error ("constructing function handle from keyword '%s' succeeded!",
%!             kword);
%!    endif
%!  endfor
