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

## Test if function handles correctly dispatch to class methods while keeping
## a link to the original function file.
%!test <*59451>
%! dpi = pi;
%! spi = single (pi);
%! ## Create function handle
%! fh = @bug59451;
%! ## All of these must dispatch to the corresponding function file.
%! assert (fh (dpi, dpi), "none");
%! assert (fh (dpi, spi), "none");
%! assert (fh (spi, spi), "none");
%! assert (fh (spi, dpi), "none");
%! assert (fh ({}, {}), "none");
%! ## cd to a directory that contains class overloads
%! cd foo
%! ## The overloads must take precedence.
%! assert (fh (dpi, dpi), "double");
%! assert (fh (dpi, spi), "single");
%! assert (fh (spi, spi), "single");
%! assert (fh (spi, dpi), "single");
%! ## This must still dispatch to the original function file.
%! assert (fh ({}, {}), "none");
