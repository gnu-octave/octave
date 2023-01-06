########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn {} {@var{has_feature} =} __gnuplot_has_feature__ (@var{feature})
## Undocumented internal function.
## @end deftypefn

function res = __gnuplot_has_feature__ (feature)
  persistent features = {"minimum_version",
                         "needs_color_with_postscript",
                         "dashtype",
                         "alphablend_linecolor",
                         "qt_terminal",
                         "wxt_figure_position",
                         "qt_figure_position",
                         "fontspec_5"};

  persistent has_features;

  if (isempty (has_features))
    gnuplot_version = __gnuplot_version__ ();
    versions  = {"4.4", "4.6", "5.0", "4.6", "4.6", "5.0", "5.0", "5.0"};
    operators = {">=" , ">=" , ">=" , ">=" , ">=" , ">=" , ">=" , ">=" };
    have_features = false (size (features));
    for n = 1 : numel (have_features)
      has_features(n) = compare_versions (gnuplot_version, versions{n}, operators{n});
    endfor
  endif

  n = find (strcmpi (feature, features));
  if (isempty (n))
    res = NaN;
  else
    res = has_features(n);
  endif

endfunction
