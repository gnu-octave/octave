########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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

## bug #46330: segfault with matrices containing Inf & NaN

%!warning
%! a = [-0.46E-01,            0.10681415316, 0.0,   -0.17121680433;
%!      -0.1675901504661613, -0.515,         1.0,    0.6420630320636088E-02;
%!       0.1543104215347786, -0.547945,     -0.906, -0.1521689385990753E-02;
%!       0.0,                 0.0,           1.0,    0.0];
%!
%! b = [0.1602300107479095,      0.2111848453E-02;
%!      0.8196877780963616E-02, -0.3025E-01;
%!      0.9173594317692437E-01, -0.75283075;
%!      0.0,                     0.0];
%!
%! c = [1.0, 0.0, 0.0, 0.0;
%!      0.0, 0.0, 0.0, 1.0];
%!
%! ## This statement caused an error in LAPACK and eventually caused
%! ## a segmentation fault.
%! ## Triggers "warning: matrix singular to machine precision"
%! assert (c / (i * Inf * eye (4) - a) * b, zeros (2, 2))
