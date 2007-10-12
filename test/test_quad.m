## Copyright (C) 2007 John W. Eaton
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

%% Automatically generated from DejaGNU files

%% test/octave.test/quad/quad-1.m
%!function y = f (x) 
%! y = x + 1;
%!test
%! [v, ier, nfun, err] = quad ("f", 0, 5);
%! assert(ier == 0 && abs (v - 17.5) < sqrt (eps) && nfun > 0 && 
%!        err < sqrt (eps))

%% test/octave.test/quad/quad-2.m
%!function y = f (x)
%!  y = x .* sin (1 ./ x) .* sqrt (abs (1 - x));
%!test
%!  [v, ier, nfun, err] = quad ("f", 0.001, 3);
%! assert((ier == 0 || ier == 1) && abs (v - 1.98194120273598) < sqrt (eps) && nfun > 0);

%% test/octave.test/quad/quad-3.m
%!error <Invalid call to quad.*> quad ();

%% test/octave.test/quad/quad-4.m
%!error <Invalid call to quad.*> quad ("f", 1, 2, 3, 4, 5);

%% test/octave.test/quad/quad_options-1.m
%!test
%! quad_options ("absolute tolerance", eps);
%! assert(quad_options ("absolute tolerance") == eps);

%% test/octave.test/quad/quad_options-3.m
%!error <Invalid call to quad_options.*> quad_options (1, 2, 3);

