## Copyright (C) 2009 Ben Abbott
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{has_feature} = } __gnuplot_has_feature__ (@var{feature})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2009-01-27

function res = __gnuplot_has_feature__ (feature)
  persistent features has_features
  features = {"x11_figure_position",
              "transparent_patches",
              "epslatex_implies_eps_filesuffix",
              "epslatexstandalone_terminal"};

  if (isempty (has_features))
    gnuplot_version = __gnuplot_version__ ();
    versions = {"4.2.4", "4.2.4", "4.2", "4.2"};
    operators = {">", ">", ">=", ">="};
    have_features = logical (zeros (size (features)));
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

