## Copyright (C) 2010-2016 Ben Abbott
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {@var{has_terminal} =} __gnuplot_has_terminal__ (@var{terminal})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-09-13

function gnuplot_supports_term = __gnuplot_has_terminal__ (term, plot_stream)

  term = strtrim (term);
  term = lower (strtok (term, " "));

  if (nargin < 2)
    plot_stream = __gnuplot_open_stream__ (2);
  endif
  available_terminals = __gnuplot_get_var__ (plot_stream, "GPVAL_TERMINALS");
  available_terminals = regexp (available_terminals, '\w+', "match");
  if (nargin < 2 && ! isempty (plot_stream))
    pclose (plot_stream(1));
    if (numel (plot_stream) > 1)
      pclose (plot_stream(2));
    endif
    if (numel (plot_stream) > 2)
      waitpid (plot_stream(3));
    endif
  endif

  gnuplot_supports_term = any (strcmp (term, available_terminals));

endfunction

