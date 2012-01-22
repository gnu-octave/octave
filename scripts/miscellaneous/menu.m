## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn {Function File} {} menu (@var{title}, @var{opt1}, @dots{})
## Print a title string followed by a series of options.  Each option will
## be printed along with a number.  The return value is the number of the
## option selected by the user.  This function is useful for interactive
## programs.  There is no limit to the number of options that may be passed
## in, but it may be confusing to present more than will fit easily on one
## screen.
## @seealso{disp, printf, input}
## @end deftypefn

## Author: jwe

function num = menu (title, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## Force pending output to appear before the menu.

  fflush (stdout);

  ## Don't send the menu through the pager since doing that can cause
  ## major confusion.

  page_screen_output (0, "local");

  if (! isempty (title))
    disp (title);
    printf ("\n");
  endif

  nopt = nargin - 1;

  while (1)
    for i = 1:nopt
      printf ("  [%2d] ", i);
      disp (varargin{i});
    endfor
    printf ("\n");
    s = input ("pick a number, any number: ", "s");
    num = sscanf (s, "%d");
    if (! isscalar (num) || num < 1 || num > nopt)
      printf ("\nerror: input invalid or out of range\n\n");
    else
      break;
    endif
  endwhile

endfunction

