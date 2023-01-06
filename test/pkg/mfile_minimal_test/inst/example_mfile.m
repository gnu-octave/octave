########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of the
## License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{ret} =} example_mfile (@var{arg})
## Placeholder for an mfile function.
##
## @var{arg}: argument. @var{ret}: returned value.
##
## Although this file is so short, a license text is contained as an
## example.
##
## @seealso{example_open, example_close, example_do_something}
## @end deftypefn

function ret = example_mfile (arg)

  if (nargin () != 1)
    print_usage ();
  endif

  ret = arg;

endfunction
