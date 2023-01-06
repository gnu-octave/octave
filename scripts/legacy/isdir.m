########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn {} {@var{tf} =} isdir (@var{f})
##
## This function is not recommended.  Use @code{isfolder} or
## @code{file_in_loadpath} instead.
##
## Return true if @var{f} is a directory and false otherwise.
##
## Compatibility Note: The @sc{matlab} function of the same name will also
## search for @var{f} in the load path directories.  To emulate this behavior
## use
##
## @example
## @var{tf} = ! isempty (file_in_loadpath (@var{f}))
## @end example
##
## @seealso{isfolder, file_in_loadpath, exist, stat, is_absolute_filename,
## is_rooted_relative_filename}
## @end deftypefn

function tf = isdir (f)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:legacy-function",
             "isdir is obsolete; use isfolder or dir_in_loadpath instead\n");
  endif

  if (nargin < 1)
    print_usage ();
  endif

  ## Exist returns an integer but isdir should return a logical.
  tf = (exist (f, "dir") == 7);

endfunction


## First test is necessary to provoke 1-time legacy warning
%!test
%! warning ("off", "Octave:legacy-function", "local");
%! isdir (pwd ());

%!assert (isdir (pwd ()))
%!assert (! isdir (tempname ()))

%!error <Invalid call> isdir ()
