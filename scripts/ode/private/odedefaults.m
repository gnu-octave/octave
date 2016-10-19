## Copyright (C) 2016 Carlo de Falco
## Copyright (C) 2016 Francesco Faccio <francesco.faccio@mail.polimi.it>
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
## @deftypefn {} {[@var{defaults}, @var{classes}, @var{attributes}] =} odedefaults (@var{n}, @var{t0}, @var{tf})
## Undocumented internal function.
## @end deftypefn

function [defaults, classes, attributes] = odedefaults (n, t0, tf)

  persistent defaults = struct ("AbsTol", 1e-6,
                                "BDF", "off",
                                "Events", [],
                                "InitialSlope", zeros (n,1),
                                "InitialStep", [],
                                "Jacobian", [],
                                "JConstant", "off",
                                "JPattern", [],
                                "Mass", [],
                                "MassSingular", "maybe",
                                "MaxOrder", 5,
                                "MaxStep", 0.1 * abs (tf - t0),
                                "MStateDependence", "weak",
                                "MvPattern", [],
                                "NonNegative", [],
                                "NormControl", "off",
                                "OutputFcn", [],
                                "OutputSel", [],
                                "Refine", 1,
                                "RelTol", 1e-3,
                                "Stats", "off",
                                "Vectorized", "off");

  defaults.MaxStep = 0.1 * abs (tf -t0);

  persistent classes = struct ("AbsTol", {{"float"}},
                               "BDF", "char",
                               "Events", {{"function_handle"}},
                               "InitialSlope", {{"float"}},
                               "InitialStep", {{"float"}},
                               "Jacobian", {{"float", "function_handle", "cell"}},
                               "JConstant", "char",
                               "JPattern", {{"float"}},
                               "Mass", {{"float", "function_handle"}},
                               "MassSingular", "char",
                               "MaxOrder", {{"float"}},
                               "MaxStep", {{"float"}},
                               "MStateDependence", "char",
                               "MvPattern", {{"float"}},
                               "NonNegative", {{"float"}},
                               "NormControl", "char",
                               "OutputFcn", {{"function_handle"}},
                               "OutputSel", {{"float"}},
                               "Refine", {{"float"}},
                               "RelTol", {{"float"}},
                               "Stats", "char",
                               "Vectorized", "char");

  persistent attributes = struct ("AbsTol", {{"real", "vector", "positive"}},
                                  "BDF", {{"on", "off"}},
                                  "Events", {{}},
                                  "InitialSlope", {{"real", "vector", "numel", n}},
                                  "InitialStep", {{"positive", "scalar"}},
                                  "Jacobian", {{}},
                                  "JConstant", {{"on", "off"}},
                                  "JPattern", {{"vector"}},
                                  "Mass", {{}},
                                  "MassSingular", {{"no", "maybe", "yes"}},
                                  "MaxOrder", {{">=", 0, "<=", 5, "integer"}},
                                  "MaxStep", {{"positive", "scalar", "real"}},
                                  "MStateDependence", {{"weak", "strong", "none"}},
                                  "MvPattern", {{"vector"}},
                                  "NonNegative", {{"vector", "integer", "positive"}},
                                  "NormControl", {{"on", "off"}},
                                  "OutputFcn", {{}},
                                  "OutputSel", {{"vector", "integer", "positive",...
                                                 ">", 0, "<=", n}},
                                  "Refine", {{"scalar", ">", 0, "integer"}},
                                  "RelTol", {{"scalar", "positive", "real"}},
                                  "Stats", {{"on", "off"}},
                                  "Vectorized", {{"on", "off"}});
endfunction

