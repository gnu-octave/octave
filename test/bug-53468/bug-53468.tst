########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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

## Load an Octave 4.0.3 figure file
%!test <*53468>
%! unwind_protect
%!   hf = hgload ("ofig403.ofig");
%! unwind_protect_cleanup
%!   if (exist ("hf", "var") && isfigure (hf))
%!     close (hf)
%!   endif
%! end_unwind_protect

## Load an Octave 4.2.1 figure file
%!test <*53468>
%! unwind_protect
%!   hf = hgload ("ofig421.ofig");
%! unwind_protect_cleanup
%!   if (exist ("hf", "var") && isfigure (hf))
%!     close (hf)
%!   endif
%! end_unwind_protect

## Load an Octave 5.1 figure file
%!test <*53468>
%! unwind_protect
%!   hf = hgload ("ofig51.ofig");
%! unwind_protect_cleanup
%!   if (exist ("hf", "var") && isfigure (hf))
%!     close (hf)
%!   endif
%! end_unwind_protect
